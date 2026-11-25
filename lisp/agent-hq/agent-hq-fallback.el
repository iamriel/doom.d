;;; agent-hq-fallback.el --- Move Claude shells to another account at the plan limit -*- lexical-binding: t; -*-

;;; Commentary:

;; Claude shells that `agent-hq-claude-config-dirs' puts on one account,
;; such as a work one, can go on in another once theirs reaches its plan
;; limit:
;;
;;   (setq agent-hq-claude-fallback-dirs '(("~/.claude-work" . "~/.claude")))
;;
;; Claude Code learns an account's limits from every answer, and
;; claude-agent-acp passes them on: as `_claude/rateLimit' in usage
;; updates, and in the error data of a turn the account refused.  Once a
;; shell's account is past its plan limit, whether the turn goes on in
;; extra usage or is refused, Emacs asks when the turn is over whether to
;; move that shell to the fallback account, or every shell on the account,
;; and new ones too, until the limit resets.
;;
;; Moving copies the session's files (its transcript and its folder of
;; subagent transcripts and saved tool output) into the other account's
;; config dir and resumes the same session there.  The shell keeps its
;; name, window and history, and its memory too, which
;; `agent-hq-claude-shared-memory' keeps under ~/.claude.  What doesn't
;; carry over is the prompt cache, and the earlier thinking when the
;; accounts are in different organizations: Claude Code leaves that out
;; (with a warning in the shell) until the session is back where it was
;; written.  So the first turn on the other account pays for the whole
;; context again, and Claude thinks afresh.  A shell whose turn was
;; refused is then told to go on, with
;; `agent-hq-claude-fallback-continue-prompt'.  Background agents the
;; shell still runs stop when it moves.
;;
;; When the limit resets, a notification names the shells still on the
;; fallback.  Nothing moves back by itself: `agent-hq-switch-account'
;; moves a shell back, or to any account, at any time.
;;
;; Turn it on with `agent-hq-fallback-setup'.

;;; Code:

(require 'agent-hq)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell-restart "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell--current-shell "agent-shell")
(declare-function agent-hq-subagents--status "agent-hq-subagents")
(defvar agent-shell--state)
(defvar agent-hq-subagents--agents)

(defcustom agent-hq-claude-fallback-dirs nil
  "Where Claude shells may go when their account reaches its plan limit.
An alist of (CONFIG-DIR . FALLBACK-DIR).  Shells on CONFIG-DIR's
account (see `agent-hq-claude-config-dirs') are offered a move to
FALLBACK-DIR's once its plan limit is reached.  ~/.claude stands for
the default account, with CLAUDE_CONFIG_DIR left unset.

For example, ((\"~/.claude-work\" . \"~/.claude\")).  Takes effect
through `agent-hq-fallback-setup'."
  :type '(alist :key-type directory :value-type directory)
  :group 'agent-hq)

(defcustom agent-hq-claude-fallback-continue-prompt
  "Continue where you left off: the last turn stopped at a usage limit."
  "What a shell moved after a refused turn is sent, or nil for nothing.
The refused prompt is in the session already, so it isn't sent again."
  :type '(choice string (const :tag "Nothing" nil))
  :group 'agent-hq)

(defcustom agent-hq-claude-default-account-name "personal"
  "What the account of ~/.claude is called when shells move to or from it."
  :type 'string
  :group 'agent-hq)

;;;; Accounts

(defun agent-hq-fallback--config (dir)
  "Return config dir DIR as agent-hq keeps accounts: `default' for ~/.claude."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (if (equal dir (agent-hq--claude-default-config)) 'default dir)))

(defun agent-hq-fallback--config-dir (config)
  "Return the directory of account CONFIG, a config dir or `default'."
  (if (eq config 'default) (agent-hq--claude-default-config) config))

(defun agent-hq-fallback--name (config)
  "Return the name of account CONFIG: \"work\" for ~/.claude-work."
  (if (eq config 'default)
      agent-hq-claude-default-account-name
    (agent-hq--account-name config)))

(defun agent-hq-fallback-target (config)
  "Return the account that shells on account CONFIG fall back to, or nil.
CONFIG is a config dir or `default'; so is the result."
  (when config
    (seq-some (lambda (entry)
                (when (equal (agent-hq-fallback--config (car entry)) config)
                  (agent-hq-fallback--config (cdr entry))))
              agent-hq-claude-fallback-dirs)))

(defun agent-hq-fallback--accounts ()
  "Return every account shells can run on, ~/.claude first."
  (delete-dups
   (mapcar #'agent-hq-fallback--config
           (append (agent-hq--claude-configs)
                   (mapcan (lambda (entry) (list (car entry) (cdr entry)))
                           agent-hq-claude-fallback-dirs)))))

(defun agent-hq-fallback--account (buffer)
  "Return the account Claude shell BUFFER runs on, or nil if it isn't one."
  (buffer-local-value 'agent-hq--claude-started-with buffer))

(defun agent-hq-fallback--home (buffer)
  "Return the account that BUFFER's repo maps to, or `default'."
  (with-current-buffer buffer
    (or (agent-hq-claude-config-dir) 'default)))

(defun agent-hq-fallback--shell-label (buffer)
  "Return where shell BUFFER works, as repo:worktree, or its name."
  (if-let* ((ctx (with-current-buffer buffer (agent-hq-context))))
      (agent-hq-context-label ctx)
    (buffer-name buffer)))

;;;; Plan limits

(defvar agent-hq-fallback--limits (make-hash-table :test #'equal)
  "Accounts past their plan limit, as CONFIG -> plist.
:resets-at is when the limit resets, as a float time; :guessed non-nil
when that time is a guess; :type the limit (\"five_hour\", \"seven_day\"
and so on); :overage non-nil while extra usage pays for the account's
turns; :answer what was chosen for its shells, `all' (move them) or
`stay', nil until asked; :timer the timer that ends the limit.")

(defconst agent-hq-fallback--limit-prefixes
  '("You've hit your" "You've reached your" "You're out of extra usage"
    "You're out of usage credits")
  "How Claude Code's usage limit errors begin.
The Agent SDK's USAGE_LIMIT_ERROR_PREFIXES, those an account switch
fixes; for errors that come without claude-agent-acp's data.")

(defconst agent-hq-fallback--guessed-limit 3600
  "Seconds a limit known only from an error's text is taken to last.")

(defun agent-hq-fallback--limit (config)
  "Return account CONFIG's plan limit while it's on, else nil."
  (when-let* ((limit (gethash config agent-hq-fallback--limits))
              ((> (plist-get limit :resets-at) (float-time))))
    limit))

(defun agent-hq-fallback--rejected-p (info)
  "Return non-nil if rate limit INFO says the plan limit is reached.
INFO is Claude Code's SDKRateLimitInfo as an alist.  Its status is
`rejected' whether or not extra usage takes over (`isUsingOverage')."
  (and (consp info) (equal (alist-get 'status info) "rejected")))

(defun agent-hq-fallback--note (config info)
  "Record rate limit INFO from Claude Code for account CONFIG.
Returns CONFIG's limit when INFO says it's past its plan limit.  An
allowed status ends a limit before its time."
  (cond
   ((agent-hq-fallback--rejected-p info)
    (let* ((stale (gethash config agent-hq-fallback--limits))
           (old (agent-hq-fallback--limit config))
           (resets (alist-get 'resetsAt info))
           (resets (cond ((numberp resets) (float resets))
                         (old (plist-get old :resets-at))
                         (t (+ (float-time) agent-hq-fallback--guessed-limit))))
           (limit (list :resets-at resets
                        :guessed (and (not (numberp (alist-get 'resetsAt info)))
                                      (or (null old) (plist-get old :guessed)))
                        :type (or (alist-get 'rateLimitType info) (plist-get old :type))
                        :overage (eq (alist-get 'isUsingOverage info) t)
                        :answer (plist-get old :answer))))
      (when-let* ((timer (plist-get stale :timer)))
        (cancel-timer timer))
      (setq limit (plist-put limit :timer
                             (run-at-time (max 1 (- (+ resets 30) (float-time))) nil
                                          #'agent-hq-fallback--limit-over config)))
      (puthash config limit agent-hq-fallback--limits)
      limit))
   ((and (consp info)
         (member (alist-get 'status info) '("allowed" "allowed_warning"))
         (gethash config agent-hq-fallback--limits))
    (agent-hq-fallback--limit-over config)
    nil)))

(defun agent-hq-fallback--answer (config answer)
  "Remember ANSWER, `all' or `stay', for account CONFIG's shells."
  (when-let* ((limit (gethash config agent-hq-fallback--limits)))
    (puthash config (plist-put (copy-sequence limit) :answer answer)
             agent-hq-fallback--limits)))

(defun agent-hq-fallback--type-name (type)
  "Return limit TYPE, such as \"five_hour\", as words."
  (pcase type
    ("five_hour" "5-hour")
    ("seven_day" "weekly")
    ("seven_day_opus" "weekly Opus")
    ("seven_day_sonnet" "weekly Sonnet")
    ("overage" "extra usage")
    ((pred stringp) (string-replace "_" " " type))
    (_ "plan")))

(defun agent-hq-fallback--clock (time)
  "Return float TIME as \"4:40pm\", with the day when it isn't today."
  (replace-regexp-in-string
   "[AP]M" #'downcase
   (format-time-string (if (equal (format-time-string "%F" time) (format-time-string "%F"))
                           "%-I:%M%p"
                         "%a %-I:%M%p")
                       time)
   t t))

(defun agent-hq-fallback--describe (config limit)
  "Return \"work is at its 5-hour limit until 4:40pm\" for CONFIG's LIMIT."
  (format "%s is at its %s limit%s"
          (agent-hq-fallback--name config)
          (agent-hq-fallback--type-name (plist-get limit :type))
          (if (plist-get limit :guessed)
              ""
            (concat " until " (agent-hq-fallback--clock (plist-get limit :resets-at))))))

(defun agent-hq-fallback--limit-over (config)
  "End account CONFIG's plan limit and name the shells that could go back."
  (when-let* ((limit (gethash config agent-hq-fallback--limits)))
    (when-let* ((timer (plist-get limit :timer)))
      (cancel-timer timer))
    (remhash config agent-hq-fallback--limits)
    (let* ((target (agent-hq-fallback-target config))
           (away (and target
                      (seq-filter (lambda (shell)
                                    (and (equal (agent-hq-fallback--account shell) target)
                                         (equal (agent-hq-fallback--home shell) config)))
                                  (agent-hq--shells))))
           (title (format "Claude %s: %s limit reset" (agent-hq-fallback--name config)
                          (agent-hq-fallback--type-name (plist-get limit :type)))))
      (cond ((plist-get limit :guessed))
            (away
             (let ((body (format "%s still on %s: %s.  %s moves one back."
                                 (agent-hq--count (length away) "shell")
                                 (agent-hq-fallback--name target)
                                 (string-join (mapcar #'agent-hq-fallback--shell-label away) ", ")
                                 (substitute-command-keys "\\[agent-hq-switch-account]"))))
               (message "%s.  %s" title body)
               (agent-hq-notify (car away) title body)))
            (t (message "%s" title))))))

;;;; Noticing the limit

(defvar-local agent-hq-fallback--hit nil
  "Non-nil once this shell's turn found its account past its plan limit.")

(defvar-local agent-hq-fallback--moved nil
  "Non-nil in a shell `agent-hq-switch-account' made.")

(defun agent-hq-fallback--on-usage (buffer event)
  "Note the rate limit in BUFFER's usage-update EVENT, if it has one."
  (when-let* (((buffer-live-p buffer))
              (info (map-nested-elt event '(:data :meta _claude/rateLimit)))
              (config (agent-hq-fallback--account buffer))
              ((agent-hq-fallback-target config)))
    (when (agent-hq-fallback--note config info)
      (with-current-buffer buffer
        (setq agent-hq-fallback--hit t)))))

(defun agent-hq-fallback--on-turn-complete (buffer)
  "Offer to move BUFFER if its turn found the account past its limit."
  (when (and (buffer-live-p buffer)
             (buffer-local-value 'agent-hq-fallback--hit buffer))
    (with-current-buffer buffer
      (setq agent-hq-fallback--hit nil))
    (agent-hq-fallback--offer buffer nil)))

(defun agent-hq-fallback--usage-limit-p (acp-error)
  "Return non-nil if ACP-ERROR is a turn refused at the usage limit.
claude-agent-acp says so in its data (`errorKind' rate_limit, with the
rejected rate limit); without that, the message's text tells."
  (let ((data (alist-get 'data acp-error))
        (text (string-remove-prefix "Internal error: "
                                    (or (alist-get 'message acp-error) ""))))
    (or (and (consp data)
             (equal (alist-get 'errorKind data) "rate_limit")
             (agent-hq-fallback--rejected-p (alist-get 'rateLimit data)))
        (seq-some (lambda (prefix) (string-prefix-p prefix text))
                  agent-hq-fallback--limit-prefixes))))

(defun agent-hq-fallback--on-error (buffer acp-error)
  "Offer to move shell BUFFER if ACP-ERROR is its turn refused at the limit."
  (when-let* (((buffer-live-p buffer))
              ((consp acp-error))
              (config (agent-hq-fallback--account buffer))
              ((agent-hq-fallback-target config))
              ((agent-hq-fallback--usage-limit-p acp-error)))
    (let ((info (map-nested-elt acp-error '(data rateLimit))))
      (agent-hq-fallback--note config (if (agent-hq-fallback--rejected-p info)
                                          info
                                        '((status . "rejected")))))
    (with-current-buffer buffer
      (setq agent-hq-fallback--hit nil))
    (agent-hq-fallback--offer buffer t)))

;;;###autoload
(defun agent-hq-fallback-error-handler-a (fn &rest args)
  "Around advice for `agent-shell--make-error-handler' (FN, ARGS).
The handler it makes also offers a move to the fallback account when a
turn is refused at the usage limit.  agent-shell's own `error' event
leaves out the error's data, which tells that apart from other errors."
  (let ((handler (apply fn args))
        (shell (plist-get args :shell-buffer)))
    (lambda (acp-error &optional raw-message)
      (funcall handler acp-error raw-message)
      (with-demoted-errors "agent-hq-fallback: %S"
        (agent-hq-fallback--on-error shell acp-error)))))

;;;; Asking

(defvar agent-hq-fallback--queue nil
  "Shells to ask about, oldest first, as (BUFFER . FAILED).")

(defvar agent-hq-fallback--asking nil
  "Non-nil while a question is up.")

(defvar agent-hq-fallback--timer nil
  "The timer that asks the next question.")

(defun agent-hq-fallback--offer (buffer failed)
  "Ask soon whether to move shell BUFFER to its fallback account.
FAILED says its turn was refused.  Questions wait for each other and
for the minibuffer to be free."
  (if-let* ((queued (assq buffer agent-hq-fallback--queue)))
      (when failed
        (setcdr queued t))
    (setq agent-hq-fallback--queue
          (append agent-hq-fallback--queue (list (cons buffer failed)))))
  (agent-hq-fallback--ask-soon 0))

(defun agent-hq-fallback--ask-soon (seconds)
  "Ask the next question in SECONDS, unless that's already planned."
  (unless (timerp agent-hq-fallback--timer)
    (setq agent-hq-fallback--timer
          (run-at-time seconds nil #'agent-hq-fallback--ask-next))))

(defun agent-hq-fallback--ask-next ()
  "Ask about the next shell in `agent-hq-fallback--queue'."
  (when (timerp agent-hq-fallback--timer)
    (cancel-timer agent-hq-fallback--timer))
  (setq agent-hq-fallback--timer nil)
  (cond (agent-hq-fallback--asking)
        ((null agent-hq-fallback--queue))
        ((or (active-minibuffer-window) (> (minibuffer-depth) 0))
         (agent-hq-fallback--ask-soon 2))
        (t
         (let ((item (pop agent-hq-fallback--queue)))
           (setq agent-hq-fallback--asking t)
           (unwind-protect
               (condition-case err
                   (agent-hq-fallback--handle (car item) (cdr item))
                 (user-error (message "%s" (error-message-string err)))
                 (error (message "agent-hq-fallback: %s" (error-message-string err))))
             (setq agent-hq-fallback--asking nil)
             (when agent-hq-fallback--queue
               (agent-hq-fallback--ask-soon 0)))))))

(defun agent-hq-fallback--working-agents (buffer)
  "Return how many background agents shell BUFFER runs, as far as is known."
  (let ((agents (and (boundp 'agent-hq-subagents--agents)
                     (fboundp 'agent-hq-subagents--status)
                     (buffer-local-value 'agent-hq-subagents--agents buffer)))
        (now (float-time)))
    (if (hash-table-p agents)
        (seq-count (lambda (agent) (eq (agent-hq-subagents--status agent now) 'working))
                   (hash-table-values agents))
      0)))

(defun agent-hq-fallback--capitalize (text)
  "Return TEXT with its first letter in upper case."
  (if (string-empty-p text)
      text
    (concat (upcase (substring text 0 1)) (substring text 1))))

(defun agent-hq-fallback--ask (buffer config target limit failed)
  "Ask what to do about shell BUFFER on CONFIG, past its LIMIT.
TARGET is the fallback account; FAILED says the turn was refused.
Returns ?y, ?a, ?n, or nil when the question was quit."
  (let* ((agents (agent-hq-fallback--working-agents buffer))
         (from (agent-hq-fallback--name config))
         (to (agent-hq-fallback--name target))
         (prompt (format "%s%s.  Move %s to %s%s?"
                         (agent-hq-fallback--capitalize (agent-hq-fallback--describe config limit))
                         (cond (failed "; the turn stopped")
                               ((plist-get limit :overage) ", now on extra usage")
                               (t ""))
                         (agent-hq-fallback--shell-label buffer)
                         to
                         (if (> agents 0)
                             (format " (its %s stop)" (agent-hq--count agents "background agent"))
                           ""))))
    (unless (eq (frame-focus-state) t)
      (agent-hq-notify buffer (format "Claude %s limit" from) prompt))
    (condition-case nil
        (car (read-multiple-choice
              prompt
              `((?y "yes" ,(format "Move this shell to %s" to))
                (?a "all" ,(format "Move every %s shell to %s, new ones too, until the reset"
                                   from to))
                (?n "no" ,(format "Stay on %s; ask again only if a turn is refused" from)))))
      (quit nil))))

(defun agent-hq-fallback--move (buffer target continue)
  "Move shell BUFFER to account TARGET, telling it to go on with CONTINUE."
  (condition-case err
      (agent-hq-switch-account buffer target continue)
    (user-error (message "%s" (error-message-string err)))))

(defun agent-hq-fallback--move-all (config target)
  "Move every shell on account CONFIG to TARGET.
Busy shells move once their turn is over."
  (dolist (shell (agent-hq--shells))
    (when (equal (agent-hq-fallback--account shell) config)
      (pcase (agent-hq--status shell)
        ('ready (agent-hq-fallback--move shell target nil))
        ((or 'busy 'blocked)
         (with-current-buffer shell
           (setq agent-hq-fallback--hit t)))))))

(defun agent-hq-fallback--handle (buffer failed)
  "Move shell BUFFER, or ask whether to; FAILED says its turn was refused."
  (when-let* (((buffer-live-p buffer))
              (config (agent-hq-fallback--account buffer))
              (target (agent-hq-fallback-target config))
              (limit (agent-hq-fallback--limit config)))
    (let ((answer (plist-get limit :answer)))
      (cond
       ((not (eq (agent-hq--status buffer) 'ready))
        ;; Another turn began meanwhile: think again once it's over.
        (with-current-buffer buffer
          (setq agent-hq-fallback--hit t)))
       ((eq answer 'all)
        (agent-hq-fallback--move buffer target failed))
       ((and (eq answer 'stay) (not failed)))
       (t
        (pcase (agent-hq-fallback--ask buffer config target limit failed)
          (?y (agent-hq-fallback--move buffer target failed))
          (?a (agent-hq-fallback--answer config 'all)
              (agent-hq-fallback--move buffer target failed)
              (agent-hq-fallback--move-all config target))
          (?n (agent-hq-fallback--answer config 'stay))))))))

;;;; Moving a shell

(defun agent-hq-fallback--session-file (config id dir)
  "Return account CONFIG's transcript of session ID, run in DIR, or nil.
Claude Code names its folder after DIR, but for long paths, so those
are found by ID."
  (let* ((projects (expand-file-name "projects" (agent-hq-fallback--config-dir config)))
         (key (agent-hq--claude-project-key dir))
         (guess (and key (expand-file-name (concat key "/" id ".jsonl") projects))))
    (if (and guess (file-exists-p guess))
        guess
      (car (file-expand-wildcards (expand-file-name (concat "*/" id ".jsonl") projects))))))

(defun agent-hq-fallback-copy-session (from to id dir)
  "Copy session ID, run in DIR, from account FROM to account TO.
Copies its transcript and its folder (subagent transcripts, saved tool
output) into the same project folder under TO, so TO can resume it.
TO's copy, if any, is replaced, unless it's newer and differs, which
means the session went on there; that is an error.  Returns TO's
transcript.  FROM and TO are config dirs or `default'."
  (let* ((source (or (agent-hq-fallback--session-file from id dir)
                     (user-error "No session %s under %s" id
                                 (abbreviate-file-name (agent-hq-fallback--config-dir from)))))
         (project (file-name-nondirectory (directory-file-name (file-name-directory source))))
         (dest-dir (expand-file-name project (expand-file-name
                                              "projects" (agent-hq-fallback--config-dir to))))
         (dest (expand-file-name (concat id ".jsonl") dest-dir))
         (folder (expand-file-name id (file-name-directory source))))
    (when (and (file-exists-p dest)
               (file-newer-than-file-p dest source)
               (not (eql (file-attribute-size (file-attributes dest))
                         (file-attribute-size (file-attributes source)))))
      (user-error "%s has a newer copy of this session; resume that one (agent-hq-find-session)"
                  (agent-hq-fallback--name to)))
    (make-directory dest-dir t)
    ;; Not keeping the time: the new copy is the one to resume, and
    ;; `agent-hq-find-session' lists the newest copy of a session.
    (let ((temp (make-temp-name (concat dest ".tmp"))))
      (copy-file source temp t)
      (rename-file temp dest t))
    (when (file-directory-p folder)
      (copy-directory folder (expand-file-name id dest-dir) t t t))
    dest))

(defun agent-hq-fallback--continue (buffer)
  "Tell shell BUFFER to go on after a refused turn."
  (when (and (buffer-live-p buffer) agent-hq-claude-fallback-continue-prompt)
    (with-current-buffer buffer
      (agent-shell-insert :text agent-hq-claude-fallback-continue-prompt
                          :submit t :no-focus t :shell-buffer buffer))))

(defun agent-hq-fallback--after-move (buffer continue)
  "Settle shell BUFFER once its session runs on the new account.
Names it after that account and, with CONTINUE, tells it to go on."
  (with-current-buffer buffer
    (setq agent-hq-fallback--moved t))
  (agent-hq-refresh-buffer-name buffer)
  (let (done)
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'init-finished
     :on-event (lambda (_event)
                 (unless done
                   (setq done t)
                   (agent-hq-refresh-buffer-name buffer)
                   (when continue
                     (run-at-time 0 nil #'agent-hq-fallback--continue buffer)))))))

(defun agent-hq-fallback--shell-here ()
  "Return the Claude shell to act on here: this one, or this worktree's."
  (let ((shell (or (and (fboundp 'agent-shell--current-shell) (agent-shell--current-shell))
                   (seq-find #'agent-hq-fallback--account
                             (agent-hq--shells-in (agent-hq--current-root))))))
    (unless (and shell (agent-hq-fallback--account shell))
      (user-error "No Claude shell here"))
    shell))

(defun agent-hq-fallback--switch-args ()
  "Read the arguments of `agent-hq-switch-account'."
  (let* ((buffer (agent-hq-fallback--shell-here))
         (from (agent-hq-fallback--account buffer))
         (home (agent-hq-fallback--home buffer))
         (accounts (remove from (agent-hq-fallback--accounts)))
         (default (if (equal from home) (agent-hq-fallback-target from) home))
         (agents (agent-hq-fallback--working-agents buffer)))
    (unless accounts
      (user-error "No other Claude account: see `agent-hq-claude-config-dirs'"))
    (let ((target
           (if (cdr accounts)
               (let* ((choices (mapcar (lambda (config)
                                         (cons (format "%s (%s)" (agent-hq-fallback--name config)
                                                       (abbreviate-file-name
                                                        (agent-hq-fallback--config-dir config)))
                                               config))
                                       accounts))
                      (choice (completing-read (format "Move %s to account: "
                                                       (agent-hq-fallback--shell-label buffer))
                                               choices nil t nil nil
                                               (car (rassoc default choices)))))
                 (cdr (assoc choice choices)))
             (car accounts))))
      (unless (y-or-n-p (format "Move %s to %s%s? "
                                (agent-hq-fallback--shell-label buffer)
                                (agent-hq-fallback--name target)
                                (if (> agents 0)
                                    (format ", stopping its %s"
                                            (agent-hq--count agents "background agent"))
                                  "")))
        (user-error "Cancelled"))
      (list buffer target nil))))

;;;###autoload
(defun agent-hq-switch-account (buffer target &optional continue)
  "Move Claude shell BUFFER, and its session, to account TARGET.
TARGET is a config dir, or `default' for ~/.claude.  The session's files
are copied to TARGET's config dir (`agent-hq-fallback-copy-session') and
resumed there in a shell with BUFFER's name and windows, on TARGET until
moved again.  The other account has no prompt cache for the session,
and in another organization doesn't get its earlier thinking, so its
first turn there pays for the whole context again.  A shell with no
session yet just restarts on TARGET.  With CONTINUE, the shell is told
to go on, with `agent-hq-claude-fallback-continue-prompt'.  Returns the
new shell.

Interactively, moves this shell (or this worktree's) and asks where to,
offering its fallback account (`agent-hq-claude-fallback-dirs') or, on
that, the account its repo maps to."
  (interactive (agent-hq-fallback--switch-args))
  (let* ((from (or (agent-hq-fallback--account buffer)
                   (user-error "%s isn't a Claude shell" (buffer-name buffer))))
         (name (buffer-name buffer))
         (label (agent-hq-fallback--shell-label buffer))
         (state (buffer-local-value 'agent-shell--state buffer))
         (id (map-nested-elt state '(:session :id)))
         (config (map-elt state :agent-config)))
    (when (equal from target)
      (user-error "%s is on %s already" label (agent-hq-fallback--name target)))
    (unless (eq (agent-hq--status buffer) 'ready)
      (user-error "%s is busy; move it once its turn is over" label))
    (when id
      (agent-hq-fallback-copy-session from target id (buffer-local-value 'default-directory buffer)))
    (with-current-buffer buffer
      (map-put! agent-shell--state :agent-config (agent-hq--claude-config-for target config))
      (condition-case err
          (agent-shell-restart :session-id id)
        ((error quit)
         ;; It still runs on FROM, so its next restart should too.
         (when (buffer-live-p buffer)
           (map-put! (buffer-local-value 'agent-shell--state buffer) :agent-config config))
         (signal (car err) (cdr err)))))
    (when-let* ((new (get-buffer name)))
      (agent-hq-fallback--after-move new (and id continue))
      (message "%s moved to %s" label (agent-hq-fallback--name target))
      new)))

;;;; New shells

(defun agent-hq-fallback--new-shell-config (config)
  "Return the account for a new shell whose repo maps to account CONFIG.
The fallback while CONFIG is past its limit and its shells were all
moved; else CONFIG.  For `agent-hq-claude-config-function'."
  (or (when-let* ((limit (agent-hq-fallback--limit config))
                  ((eq (plist-get limit :answer) 'all)))
        (agent-hq-fallback-target config))
      config))

(defun agent-hq-fallback--say-started (buffer)
  "Say so when new shell BUFFER started on the fallback account."
  (when-let* (((buffer-live-p buffer))
              ((not (buffer-local-value 'agent-hq-fallback--moved buffer)))
              (account (agent-hq-fallback--account buffer))
              (home (agent-hq-fallback--home buffer))
              ((not (equal account home)))
              ((equal account (agent-hq-fallback-target home)))
              (limit (agent-hq-fallback--limit home))
              ((eq (plist-get limit :answer) 'all)))
    (message "%s started on %s: %s" (agent-hq-fallback--shell-label buffer)
             (agent-hq-fallback--name account) (agent-hq-fallback--describe home limit))))

;;;; Setup

;;;###autoload
(defun agent-hq-fallback-setup-shell-h ()
  "Watch this shell's rate limits.  For `agent-shell-mode-hook'."
  (let ((buffer (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'usage-update
     :on-event (lambda (event)
                 (with-demoted-errors "agent-hq-fallback: %S"
                   (agent-hq-fallback--on-usage buffer event))))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'turn-complete
     :on-event (lambda (_event)
                 (with-demoted-errors "agent-hq-fallback: %S"
                   (agent-hq-fallback--on-turn-complete buffer))))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'init-finished
     :on-event (lambda (_event)
                 (with-demoted-errors "agent-hq-fallback: %S"
                   (agent-hq-fallback--say-started buffer))))))

;;;###autoload
(defun agent-hq-fallback-setup ()
  "Offer Claude shells their fallback account at the plan limit.
See `agent-hq-claude-fallback-dirs'."
  (advice-add 'agent-shell--make-error-handler :around #'agent-hq-fallback-error-handler-a)
  (add-hook 'agent-shell-mode-hook #'agent-hq-fallback-setup-shell-h)
  (setq agent-hq-claude-config-function #'agent-hq-fallback--new-shell-config))

(provide 'agent-hq-fallback)

;;; agent-hq-fallback.el ends here
