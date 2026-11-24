;;; agent-git-header.el --- Model, plan usage and session time in agent-shell headers -*- lexical-binding: t; -*-

;;; Commentary:

;; Adds to the header of every agent-shell:
;;
;;   Claude ➤ Opus 5.5 ➤ Xhigh ➤ Auto ➤ 38k/1m (4%) ➤ $1.20
;;   webapp:worktree1 ➤ feat/x ↑1 +2 ~1 ?3 ➤ work 5h 2% (12:10pm) · week 3% (Mon 7pm) ➤ 1h 05m
;;
;; much like the status line ~/.claude/statusline.mjs gives Claude Code in a
;; terminal:
;;
;; - A linked worktree's project shows as repo:worktree.
;; - The worktree's branch, commits ahead (↑) and behind (↓) its upstream,
;;   and its staged (+), modified (~), untracked (?) and conflicted (!)
;;   files, or ✓ when clean; a merge, cherry-pick, revert or bisect in
;;   progress is named.  git status runs in the background, at most every
;;   `agent-git-header-git-ttl' seconds and after magit refreshes the repo.
;; - The model Claude actually answered with, where agent-shell shows the
;;   picker's label ("Default (recommended)").  Before the first answer it
;;   is read from the picker's description ("Opus 5.5 · Best for …").
;; - The account's 5-hour and weekly plan usage, with when each resets.
;;   Each account (see `agent-git-claude-config-dirs') has its own.  Claude
;;   Code reports them through the Agent SDK's usage call, the data behind
;;   /usage (agent-git-usage.mjs), so nothing here reads credentials and no
;;   model is called.  They're fetched in the background and kept for
;;   `agent-git-header-usage-ttl' seconds.  A window the plan doesn't have
;;   (or that isn't running) is left out.  An account other than ~/.claude
;;   is named first ("work" for ~/.claude-work).  The percentages turn
;;   yellow at 60% and red at 85%, as agent-shell's context indicator does.
;; - How long the shell has been open, refreshed every minute.
;;
;; agent-shell draws the graphical header's second line in one color;
;; `agent-git-header-render-a' gives each part above its own.  Codex shells
;; get the git status and session time.  Turn it all on with
;; `agent-git-header-setup'.
;;
;; `agent-git-show-usage' prints the session's context, tokens and cost
;; with the plan usage, and unlike agent-shell's own command it works
;; before the agent has answered.

;;; Code:

(require 'agent-git)
(require 'cl-lib)
(require 'dom)
(require 'iso8601)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell--update-header-and-mode-line "agent-shell")
(declare-function agent-shell--current-model-id "agent-shell-config")
(declare-function agent-shell--get-available-models "agent-shell-config")
(declare-function agent-shell--usage-has-data-p "agent-shell-usage")
(declare-function agent-shell--format-usage "agent-shell-usage")
(declare-function agent-shell--svg-fill-color "agent-shell")
(declare-function svg-image "svg")
(defvar agent-shell--state)
(defvar agent-shell-header-style)

(defgroup agent-git-header nil
  "Model, plan usage and session time in agent-shell headers."
  :group 'agent-git
  :prefix "agent-git-header-")

(defcustom agent-git-header-usage-ttl 180
  "Seconds plan usage is shown before it is fetched again."
  :type 'natnum)

(defcustom agent-git-header-claude-sdk nil
  "The Agent SDK's sdk.mjs, or nil for the copy claude-agent-acp ships."
  :type '(choice (const :tag "claude-agent-acp's" nil) file))

(defcustom agent-git-header-show-git t
  "Non-nil to show the shell's branch and git status in its header."
  :type 'boolean)

(defcustom agent-git-header-git-ttl 5
  "Seconds a worktree's git status is shown before git is asked again."
  :type 'number)

(defface agent-git-header-branch '((t :inherit font-lock-keyword-face))
  "The branch, and commits ahead or behind, in agent-shell headers.")

(defface agent-git-header-account '((t :inherit warning :weight bold))
  "A Claude account other than ~/.claude, in agent-shell headers.")

(defconst agent-git-header--script
  (expand-file-name "agent-git-usage.mjs"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The script that prints an account's plan usage.")

;;;; The model Claude answered with

(defvar-local agent-git-header--answered-with nil
  "(CHOICE . MODEL): Claude last answered here with MODEL, picker on CHOICE.")

(defun agent-git-header-model-name (model)
  "Return a short name for MODEL, an API model id.
\"claude-opus-5-5\" gives \"Opus 5.5\"; ids it doesn't know come back as
they are."
  (let ((case-fold-search nil)
        (id (replace-regexp-in-string "\\[[^]]*\\]\\'" "" model)))
    (if (string-match (rx bos "claude-" (group (+ (any "a-z"))) "-" (group (+ digit))
                          (? "-" (group (** 1 2 digit)))
                          (? "-" (= 8 digit))
                          eos)
                      id)
        (concat (capitalize (match-string 1 id)) " " (match-string 2 id)
                (when (match-string 3 id) (concat "." (match-string 3 id))))
      model)))

(defun agent-git-header--leading-model (description)
  "Return the model DESCRIPTION starts with, as in \"Opus 5.5 · Best for …\"."
  (when (and (stringp description)
             (string-match "\\`\\([^·\n]+?\\) *·" description))
    (match-string 1 description)))

(defun agent-git-header--described-model (state choice)
  "Return the model that picker entry CHOICE stands for in STATE, or nil.
Its description either starts with it (\"Opus 5.5 · Best for …\") or, as
for \"default\" in Claude's config options, names another entry (\"Opus\"),
whose description or name then gives it (\"Opus 5.5\")."
  (let* ((models (agent-shell--get-available-models state))
         (entry-for (lambda (id)
                      (seq-find (lambda (model) (equal (map-elt model :model-id) id)) models)))
         (description (map-elt (funcall entry-for choice) :description)))
    (when (stringp description)
      (or (agent-git-header--leading-model description)
          (when-let* ((alias (funcall entry-for (downcase (string-trim description))))
                      ((not (equal (map-elt alias :model-id) choice))))
            (or (agent-git-header--leading-model (map-elt alias :description))
                (map-elt alias :name)))))))

(defun agent-git-header--model (state label)
  "Return the model to show for STATE in place of LABEL, the picker's name."
  (let ((choice (agent-shell--current-model-id state)))
    (if (and agent-git-header--answered-with
             (equal (car agent-git-header--answered-with) choice))
        (agent-git-header-model-name (cdr agent-git-header--answered-with))
      (or (agent-git-header--described-model state choice) label))))

(defun agent-git-header--note-model (buffer event)
  "Remember the model of BUFFER's usage-update EVENT, and redraw if it's new."
  (when-let* ((model (map-nested-elt event '(:data :meta _claude/model)))
              ((stringp model))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((seen (cons (agent-shell--current-model-id agent-shell--state) model)))
        (unless (equal seen agent-git-header--answered-with)
          (setq agent-git-header--answered-with seen)
          (agent-shell--update-header-and-mode-line))))))

;;;; Plan usage per account

(defvar agent-git-header--usage (make-hash-table :test #'equal)
  "Plan usage per account, keyed by CLAUDE_CONFIG_DIR (\"\" for the default).
Each value is a plist: :data, the parsed JSON; :time, when it was last
fetched or tried; :process, the fetch running now.")

(defun agent-git-header--account ()
  "Return this shell's usage key, or nil if it isn't a Claude shell.
Only Claude clients are made through `agent-git-claude-environment-a',
which records the account."
  (pcase agent-git--claude-started-with
    ('nil nil)
    ('default "")
    (dir dir)))

(defun agent-git-header-acp-root ()
  "Return the directory claude-agent-acp is installed in, or nil."
  (when-let* ((acp (executable-find "claude-agent-acp")))
    (locate-dominating-file (file-truename acp) "package.json")))

(defun agent-git-header--usage-command ()
  "Return the command that prints plan usage, or nil if node or the SDK is missing."
  (let* ((acp (executable-find "claude-agent-acp"))
         (sdk (or agent-git-header-claude-sdk
                  (when-let* ((root (agent-git-header-acp-root)))
                    (expand-file-name "node_modules/@anthropic-ai/claude-agent-sdk/sdk.mjs"
                                      root))))
         ;; The node claude-agent-acp runs on, so fnm's PATH doesn't matter.
         (node (or (and acp (let ((node (expand-file-name "node" (file-name-directory acp))))
                              (and (file-executable-p node) node)))
                   (executable-find "node"))))
    (when (and node sdk (file-readable-p sdk))
      (list node agent-git-header--script sdk))))

(defun agent-git-header--parse-usage (buffer)
  "Return the usage JSON in BUFFER's last line that starts with \"{\"."
  (with-current-buffer buffer
    (goto-char (point-max))
    (when (re-search-backward "^{" nil t)
      (ignore-errors (json-parse-buffer :object-type 'alist :null-object nil)))))

(defun agent-git-header--fetch-usage (account)
  "Start fetching ACCOUNT's plan usage unless a fetch is already running."
  (let ((entry (gethash account agent-git-header--usage)))
    (unless (process-live-p (plist-get entry :process))
      (setq entry (plist-put (copy-sequence entry) :time (float-time)))
      (when-let* ((command (agent-git-header--usage-command)))
        (let* ((default-directory (expand-file-name "~/"))
               (process-environment
                (cons (if (string-empty-p account)
                          "CLAUDE_CONFIG_DIR"   ; unset: ~/.claude
                        (concat "CLAUDE_CONFIG_DIR=" (directory-file-name account)))
                      process-environment)))
          (setq entry (plist-put entry :process
                                 (make-process
                                  :name "agent-git-usage"
                                  :buffer (generate-new-buffer " *agent-git-usage*")
                                  :command command
                                  :connection-type 'pipe
                                  :noquery t
                                  :sentinel (lambda (process _)
                                              (agent-git-header--usage-done process account)))))))
      (puthash account entry agent-git-header--usage))))

(defun agent-git-header--usage-done (process account)
  "Store what PROCESS fetched for ACCOUNT and redraw that account's shells.
A failed fetch keeps the last usage; it's tried again after the TTL."
  (unless (process-live-p process)
    (let* ((buffer (process-buffer process))
           (data (and (buffer-live-p buffer)
                      (eql (process-exit-status process) 0)
                      (agent-git-header--parse-usage buffer)))
           (entry (gethash account agent-git-header--usage)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (puthash account
               (list :data (or data (plist-get entry :data)) :time (float-time) :process nil)
               agent-git-header--usage)
      (when data
        (agent-git-header--redraw
         (lambda (shell)
           (equal (with-current-buffer shell (agent-git-header--account)) account)))))))

(defun agent-git-header--clock (time with-day zone)
  "Return TIME as \"12:10pm\", or \"Mon 7pm\" with WITH-DAY, in time zone ZONE.
Rounded to the minute, since resets come as 10:59:59.8; \":00\" is dropped."
  (let* ((time (time-add time 30))
         (minute (decoded-time-minute (decode-time time zone))))
    (replace-regexp-in-string
     "[AP]M" #'downcase
     (format-time-string (concat (if with-day "%a " "")
                                 (if (zerop minute) "%-I%p" "%-I:%M%p"))
                         time zone)
     t t)))

(defun agent-git-header-usage-face (percent)
  "Return the face for plan usage at PERCENT, as agent-shell colors context."
  (cond ((>= percent 85) 'agent-shell-error)
        ((>= percent 60) 'agent-shell-warning)
        (t 'agent-shell-success)))

(defun agent-git-header-account-label (account)
  "Return the name to show for ACCOUNT, a usage key, or nil for ~/.claude.
~/.claude-work gives \"work\"; a dir not named .claude-NAME gives its name."
  (unless (or (null account) (string-empty-p account))
    (let ((dir (file-name-nondirectory (directory-file-name account))))
      (if (string-match "\\`\\.?claude[-_.]\\(.+\\)\\'" dir)
          (match-string 1 dir)
        dir))))

(defun agent-git-header--window (label window with-day zone)
  "Return \"LABEL N% (RESET)\" for WINDOW, or nil without a percentage.
RESET is a clock time, with the day when WITH-DAY, in time zone ZONE."
  (when-let* ((window)
              (percent (alist-get 'utilization window))
              ((numberp percent)))
    (concat label " " (propertize (format "%d%%" (round percent))
                                  'face (agent-git-header-usage-face percent))
            (when-let* ((resets (alist-get 'resets_at window))
                        ((stringp resets))
                        (time (ignore-errors (encode-time (iso8601-parse resets)))))
              (format " (%s)" (agent-git-header--clock time with-day zone))))))

(defun agent-git-header-format-usage (data &optional zone)
  "Return \"5h 2% (12:10pm) · week 3% (Mon 7pm)\" for DATA, or nil.
DATA is what agent-git-usage.mjs prints, as an alist.  Times are in time
zone ZONE, by default the local one."
  (when-let* ((parts (delq nil (list (agent-git-header--window
                                      "5h" (alist-get 'five_hour data) nil zone)
                                     (agent-git-header--window
                                      "week" (alist-get 'seven_day data) t zone)))))
    (string-join parts " · ")))

(defun agent-git-header--usage-text (account)
  "Return ACCOUNT's plan usage for the header, fetching it when stale."
  (let ((entry (gethash account agent-git-header--usage)))
    (when (or (null entry)
              (> (- (float-time) (or (plist-get entry :time) 0)) agent-git-header-usage-ttl))
      (agent-git-header--fetch-usage account))
    (when-let* ((data (plist-get entry :data)))
      (agent-git-header-format-usage data))))

(defun agent-git-header--account-text (account)
  "Return ACCOUNT's name and plan usage for the header, or nil.
The default account isn't named; another one is even before its usage
comes, since which account a shell runs on matters on its own."
  (let ((label (when-let* ((label (agent-git-header-account-label account)))
                 (propertize label 'face 'agent-git-header-account)))
        (usage (agent-git-header--usage-text account)))
    (if (and label usage)
        (concat label " " usage)
      (or label usage))))

;;;; Usage on demand

(defun agent-git-header--row (label text)
  "Return a usage row: LABEL right-aligned as in agent-shell's, then TEXT."
  (concat (propertize (format "%9s " (concat label ":"))
                      'face 'agent-shell-secondary
                      'font-lock-face 'agent-shell-secondary)
          text))

(defun agent-git-header-plan-text (account)
  "Return ACCOUNT's plan and plan usage, fetching the usage when stale.
When there's no usage to show, says why."
  (let* ((text (agent-git-header--usage-text account))
         (entry (gethash account agent-git-header--usage))
         (data (plist-get entry :data))
         (plan (alist-get 'subscription_type data)))
    (cond
     (text (if (stringp plan) (concat plan " · " text) text))
     (data "no 5-hour or weekly limit reported")
     ((process-live-p (plist-get entry :process)) "fetching, try again in a moment")
     ((agent-git-header--usage-command) "couldn't be fetched")
     (t "unavailable (node or the Agent SDK wasn't found)"))))

;;;###autoload
(defun agent-git-show-usage ()
  "Show this shell's context, tokens and cost, and its plan usage.
agent-shell's `agent-shell-show-usage' errors until the agent reports
usage, which Claude only does while it answers, so a new or resumed shell
has none.  This shows the session's numbers once there are some and, for
Claude shells, the account's 5-hour and weekly usage from the header."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (let* ((usage (map-elt agent-shell--state :usage))
         (account (agent-git-header--account)))
    (message "\n%s\n"
             (string-join
              (delq nil (list (if (agent-shell--usage-has-data-p usage)
                                  (agent-shell--format-usage usage t)
                                (agent-git-header--row
                                 "Session" "nothing reported until the agent answers"))
                              (when account
                                (agent-git-header--row
                                 "Plan" (agent-git-header-plan-text account)))))
              "\n"))))

;;;; Git status of the shell's worktree

(defvar agent-git-header--git (make-hash-table :test #'equal)
  "Git status per worktree root.
Each value is a plist: :data, from `agent-git-header-parse-status' (nil
when git failed); :time, when git was last asked; :process, the git
running now.")

(defun agent-git-header-parse-status (output)
  "Return the counts in OUTPUT, from git status --porcelain=v2 --branch.
A plist: :ahead and :behind are commits against the upstream, :staged,
:modified, :untracked and :conflicts are files.  A file both staged and
changed again counts as both."
  (let ((ahead 0) (behind 0) (staged 0) (modified 0) (untracked 0) (conflicts 0))
    (dolist (line (split-string output "\n" t))
      (cond
       ((string-match "\\`# branch\\.ab \\+\\([0-9]+\\) -\\([0-9]+\\)" line)
        (setq ahead (string-to-number (match-string 1 line))
              behind (string-to-number (match-string 2 line))))
       ((string-match "\\`[12] \\(.\\)\\(.\\) " line)
        (unless (equal (match-string 1 line) ".") (setq staged (1+ staged)))
        (unless (equal (match-string 2 line) ".") (setq modified (1+ modified))))
       ((string-prefix-p "u " line) (setq conflicts (1+ conflicts)))
       ((string-prefix-p "? " line) (setq untracked (1+ untracked)))))
    (list :ahead ahead :behind behind :staged staged :modified modified
          :untracked untracked :conflicts conflicts)))

(defun agent-git-header--git-state (git-dir)
  "Return the operation in progress in GIT-DIR, as \"merging\", or nil.
A rebase isn't named here: `agent-git--branch' already shows it."
  (when git-dir
    (seq-some (pcase-lambda (`(,file . ,state))
                (and (file-exists-p (expand-file-name file git-dir)) state))
              '(("MERGE_HEAD" . "merging") ("CHERRY_PICK_HEAD" . "cherry-picking")
                ("REVERT_HEAD" . "reverting") ("BISECT_LOG" . "bisecting")))))

(defun agent-git-header-format-git (branch data &optional state)
  "Return BRANCH and DATA's counts as \"main ↑1 +2 ~1 ?3\", with faces.
DATA is from `agent-git-header-parse-status', or nil before git answers
(or when it failed), which shows just BRANCH.  STATE names an operation
in progress."
  (let ((count (lambda (key prefix face)
                 (let ((n (plist-get data key)))
                   (when (and n (> n 0))
                     (propertize (format "%s%d" prefix n) 'face face))))))
    (string-join
     (delq nil (list (propertize branch 'face 'agent-git-header-branch)
                     (when state (propertize state 'face 'agent-shell-error))
                     (funcall count :ahead "↑" 'agent-git-header-branch)
                     (funcall count :behind "↓" 'agent-git-header-branch)
                     (funcall count :conflicts "!" 'agent-shell-error)
                     (funcall count :staged "+" 'agent-shell-success)
                     (funcall count :modified "~" 'agent-shell-warning)
                     (funcall count :untracked "?" 'shadow)
                     (when (and data (cl-every (lambda (key) (zerop (plist-get data key)))
                                               '(:staged :modified :untracked :conflicts)))
                       (propertize "✓" 'face 'agent-shell-success))))
     " ")))

(defun agent-git-header--git-fetch (root)
  "Start git status in ROOT unless it is already running there."
  (let ((entry (gethash root agent-git-header--git)))
    (unless (process-live-p (plist-get entry :process))
      (setq entry (plist-put (copy-sequence entry) :time (float-time)))
      (let ((default-directory root)
            (buffer (generate-new-buffer " *agent-git-header-status*")))
        (condition-case nil
            (setq entry
                  (plist-put entry :process
                             (make-process
                              :name "agent-git-header-status"
                              :buffer buffer
                              ;; Never take index.lock, so this can't
                              ;; collide with the agent's own git.
                              :command '("git" "--no-optional-locks" "status"
                                         "--porcelain=v2" "--branch")
                              :connection-type 'pipe
                              :noquery t
                              :sentinel (lambda (process _)
                                          (agent-git-header--git-done process root)))))
          (error (kill-buffer buffer))))
      (puthash root entry agent-git-header--git))))

(defun agent-git-header--git-done (process root)
  "Store what git status PROCESS said about ROOT; redraw ROOT's shells if new."
  (unless (process-live-p process)
    (let* ((buffer (process-buffer process))
           (data (and (buffer-live-p buffer)
                      (eql (process-exit-status process) 0)
                      (with-current-buffer buffer
                        (agent-git-header-parse-status (buffer-string)))))
           (old (plist-get (gethash root agent-git-header--git) :data)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (puthash root (list :data data :time (float-time) :process nil) agent-git-header--git)
      (unless (equal data old)
        (agent-git-header--redraw
         (lambda (shell)
           (equal (with-current-buffer shell
                    (when-let* ((ctx (agent-git-context)))
                      (agent-git-context-root ctx)))
                  root)))))))

(defun agent-git-header--git-text (ctx)
  "Return the branch and git status of CTX's worktree, asking git when stale."
  (when-let* ((agent-git-header-show-git)
              (ctx)
              (branch (agent-git-context-branch ctx)))
    (let* ((root (agent-git-context-root ctx))
           (entry (gethash root agent-git-header--git)))
      (when (or (null entry)
                (> (- (float-time) (or (plist-get entry :time) 0)) agent-git-header-git-ttl))
        (agent-git-header--git-fetch root))
      (agent-git-header-format-git branch (plist-get entry :data)
                                   (agent-git-header--git-state (agent-git--git-dir root))))))

;;;###autoload
(defun agent-git-header-magit-refresh-h ()
  "Ask git again for shells in the repo magit just refreshed.
For `magit-post-refresh-hook', so a commit or stage shows at once."
  (when-let* ((ctx (agent-git-context))
              (root (agent-git-context-root ctx))
              ((gethash root agent-git-header--git)))
    (agent-git-header--git-fetch root)))

;;;; Session time

(defvar-local agent-git-header--opened nil
  "When this shell was opened, as a float time.")

(defun agent-git-header-duration (seconds)
  "Return SECONDS as \"12m\" or \"1h 05m\"."
  (let ((minutes (floor seconds 60)))
    (if (< minutes 60)
        (format "%dm" minutes)
      (format "%dh %02dm" (/ minutes 60) (% minutes 60)))))

;;;; The header

(defun agent-git-header--status (ctx)
  "Return the git status, account and session time to show, or nil.
CTX is the shell's git context, nil outside git."
  (when-let* ((parts (delq nil
                           (list (agent-git-header--git-text ctx)
                                 (when-let* ((account (agent-git-header--account)))
                                   (agent-git-header--account-text account))
                                 (when agent-git-header--opened
                                   (agent-git-header-duration
                                    (- (float-time) agent-git-header--opened)))))))
    (string-join parts " ➤ ")))

(defun agent-git-header--face-runs (string)
  "Return STRING as a list of (TEXT . FACE), one per run of the same face."
  (let ((pos 0) (end (length string)) runs)
    (while (< pos end)
      (let ((next (next-single-property-change pos 'face string end)))
        (push (cons (substring-no-properties string pos next)
                    (get-text-property pos 'face string))
              runs)
        (setq pos next)))
    (nreverse runs)))

(defun agent-git-header--escape-percent (string)
  "Return STRING with each % doubled, its faces kept.
The text header is a mode-line format, where a lone % is a construct;
`string-replace' would leave the inserted % without a face."
  (mapconcat (pcase-lambda (`(,text . ,face))
               (let ((text (string-replace "%" "%%" text)))
                 (if face (propertize text 'face face) text)))
             (agent-git-header--face-runs string)))

;;;###autoload
(defun agent-git-header-model-a (fn state &rest args)
  "Around advice for `agent-shell--make-header-model' (FN, STATE and ARGS).
Shows the model Claude answered with, a linked worktree's project as
repo:worktree, then git status, account and plan usage and session time
after the project on the header's second line (the session id's place,
which agent-shell leaves empty unless `agent-shell-show-session-id')."
  (let ((model (apply fn state args))
        (shell (map-elt state :buffer)))
    (if (not (buffer-live-p shell))
        model
      (with-current-buffer shell
        (let* ((ctx (agent-git-context))
               (label (map-elt model :model-name))
               (name (if (and label (agent-git-header--account))
                         (agent-git-header--model state label)
                       label))
               (project (if (and ctx (agent-git-context-worktree ctx))
                            (agent-git-context-label ctx)
                          (map-elt model :project-name)))
               (status (when-let* ((status (agent-git-header--status ctx)))
                         ;; The SVG header is drawn as is.
                         (if (eq agent-shell-header-style 'text)
                             (agent-git-header--escape-percent status)
                           status)))
               (session-id (map-elt model :session-id))
               (bottom (if (and session-id status)
                           (concat session-id " ➤ " status)
                         (or session-id status))))
          (append (list (cons :model-name name)
                        (cons :project-name project)
                        (cons :session-id bottom))
                  model))))))

(defun agent-git-header--xml-escape (text)
  "Return TEXT with &, < and > escaped for SVG."
  (replace-regexp-in-string
   "[&<>]" (lambda (char) (pcase char ("&" "&amp;") ("<" "&lt;") (_ "&gt;"))) text t t))

(defun agent-git-header-color-runs (svg text)
  "Give each face in TEXT its own color where SVG draws TEXT in one tspan.
The tspan keeps its own fill for runs without a face, except for the ➤
separators, drawn in the default color as agent-shell draws its own.
Returns non-nil if TEXT was found."
  (let ((plain (substring-no-properties text)))
    (when-let* ((node (seq-find (lambda (node) (equal (dom-children node) (list plain)))
                                (dom-by-tag svg 'tspan))))
      (let ((tspan (lambda (face run)
                     ;; svg-print writes strings as they are: escape them here.
                     (dom-node 'tspan `((fill . ,(agent-shell--svg-fill-color face)))
                               (agent-git-header--xml-escape run)))))
        (setcdr (cdr node)
                (mapcan (pcase-lambda (`(,run . ,face))
                          (if face
                              (list (funcall tspan face run))
                            (cdr (mapcan (lambda (part)
                                           (list (funcall tspan 'default "➤")
                                                 (agent-git-header--xml-escape part)))
                                         (split-string run "➤")))))
                        (agent-git-header--face-runs text))))
      t)))

;;;###autoload
(defun agent-git-header-render-a (fn model)
  "Around advice for `agent-shell--render-header-model-uncached' (FN, MODEL).
agent-shell draws the graphical header's second line after the project
in one color; this colors each part of it, such as the git counts and the
usage percentages, by its face.  Should agent-shell draw it otherwise,
the line is left in its one color."
  (let ((text (map-elt model :session-id)))
    (if (not (and (eq agent-shell-header-style 'graphical)
                  (stringp text)
                  (text-property-not-all 0 (length text) 'face nil text)
                  (fboundp 'svg-image)))
        (funcall fn model)
      (let ((svg-image (symbol-function 'svg-image)))
        (cl-letf (((symbol-function 'svg-image)
                   (lambda (svg &rest props)
                     (ignore-errors (agent-git-header-color-runs svg text))
                     (apply svg-image svg props))))
          (funcall fn model))))))

(defun agent-git-header--redraw (predicate)
  "Redraw the header of each shell PREDICATE returns non-nil for."
  (dolist (shell (agent-shell-buffers))
    (when (funcall predicate shell)
      (with-current-buffer shell
        (ignore-errors (agent-shell--update-header-and-mode-line))))))

(defvar agent-git-header--timer nil
  "Redraws visible shells every minute, for the session time.")

(defun agent-git-header--tick ()
  "Redraw visible shells; stop once no shell is left."
  (if (agent-shell-buffers)
      (agent-git-header--redraw (lambda (shell) (get-buffer-window shell t)))
    (cancel-timer agent-git-header--timer)
    (setq agent-git-header--timer nil)))

;;;###autoload
(defun agent-git-header-setup-shell-h ()
  "Track this shell's open time and the model Claude answers with.
For `agent-shell-mode-hook'."
  (setq agent-git-header--opened (float-time))
  (let ((buffer (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'usage-update
     :on-event (lambda (event) (agent-git-header--note-model buffer event))))
  (unless (timerp agent-git-header--timer)
    (setq agent-git-header--timer (run-at-time 60 60 #'agent-git-header--tick))))

;;;###autoload
(defun agent-git-header-setup ()
  "Show the real model, git status, plan usage and session time in headers."
  (advice-add 'agent-shell--make-header-model :around #'agent-git-header-model-a)
  (advice-add 'agent-shell--render-header-model-uncached :around #'agent-git-header-render-a)
  (add-hook 'agent-shell-mode-hook #'agent-git-header-setup-shell-h)
  (add-hook 'magit-post-refresh-hook #'agent-git-header-magit-refresh-h))

(provide 'agent-git-header)
;;; agent-git-header.el ends here
