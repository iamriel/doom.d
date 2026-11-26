;;; agent-hq-dashboard.el --- Every agent shell at a glance, grouped by what it needs -*- lexical-binding: t; -*-

;;; Commentary:

;; `agent-hq-dashboard' lists every live agent shell, in every project,
;; grouped by what it needs from you:
;;
;;     Project         Branch               Session                    Age  Latest                       Model          Ctx
;;   Needs you (2)
;;   ● webapp:wt1      feat/x ↑1 ~2         Fix the max length check    2m  Permission: rm -rf build     Opus 5.5 work  61%
;;   ● bop-boulder<2>  main ✓               Rescued friends design     36m  Pushed the fix; tests pass   Opus 5.5       22%
;;
;;   Running (1)
;;   ⠋ bop-boulder     main ✓               Golden pebble bonus room    4m  Edit src/room.ts · 2 agents  Opus 5.5       12%
;;
;;   Idle (1)
;;   ✓ webapp:wt2      fix/y ✓ merged       Flaky date test             3h  Committed and pushed         Opus 5.5 work  40%
;;
;; - Needs you: a permission or question waits on you (red ●), or a turn
;;   ended while you were elsewhere and you haven't looked since (yellow ●,
;;   as agent-shell-attention counts it).  The longest waiting comes first.
;;   Running and Idle go most recent first.
;; - Branch is the header's: commits ahead/behind, staged (+), modified
;;   (~), untracked (?) and conflicted (!) files, ✓ when clean.  A clean
;;   linked worktree whose upstream is gone says "merged", one with
;;   nothing beyond its base branch "in develop" (or its base): what
;;   `agent-hq-clean-worktrees' would offer once its shells are closed.
;; - Age is how long the turn has run, the permission has waited, or the
;;   shell has been idle.
;; - Latest is what the shell is doing (the tool call under way, the words
;;   it's writing, its working subagents and queued prompts), what it asks
;;   you to allow, or the first line of its last answer.
;; - Model is the model Claude answered with and a non-default account,
;;   Ctx the context used.  The header line sums up the groups and each
;;   account's plan usage.
;;
;; The list follows the shells as they work: their events redraw it (at
;; most twice a second), running rows spin, and the ages tick while a
;; window shows it.  Rows act on their shell: see `agent-hq-dashboard-mode'.
;; Like a completion list, a dashboard in a side window (a popup along the
;; bottom) fits itself to its rows, and visiting a shell closes it.
;;
;; The last answer and the turn's times come from each shell's events,
;; which `agent-hq-dashboard-setup' starts following, so turns that ended
;; before it ran show no answer.

;;; Code:

(require 'agent-hq)
(require 'agent-hq-statusline)
(require 'agent-hq-spinner)
(require 'agent-hq-clock)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell--shorten-paths "agent-shell")
(declare-function agent-shell--current-model-id "agent-shell-config")
(declare-function agent-shell--prompt-queue-enqueue "agent-shell-prompt-queue")
(declare-function agent-shell-elicitation--pending-p "agent-shell-elicitation")
(declare-function agent-shell-attention-acknowledge "agent-shell-attention")
(declare-function agent-hq-subagents-view "agent-hq-subagents")
(declare-function agent-hq-subagents--status "agent-hq-subagents")
(declare-function agent-hq-switch-account "agent-hq-fallback")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function shell-maker-busy "shell-maker")
(defvar agent-shell--state)
(defvar agent-shell-display-action)
(defvar agent-shell-prompt-queue-setup-minibuffer-functions)
(defvar agent-shell-attention--pending)
(defvar agent-shell-attention--busy-since)
(defvar agent-hq-subagents--agents)
(defvar agent-hq-claude-default-account-name)

(defgroup agent-hq-dashboard nil
  "Every agent shell at a glance."
  :group 'agent-hq
  :prefix "agent-hq-dashboard-")

(defcustom agent-hq-dashboard-buffer-name "*Agents*"
  "Name of the dashboard buffer."
  :type 'string)

(defcustom agent-hq-dashboard-tick 15
  "Seconds between redraws of a dashboard a window shows, for its ages."
  :type 'number)

(defcustom agent-hq-dashboard-base-ttl 60
  "Seconds before asking git again what a worktree has beyond its base."
  :type 'number)

(defcustom agent-hq-dashboard-max-height 0.4
  "Most of its frame's height a dashboard in a side window takes, as a fraction.
In a side window, as Doom's popups are, the dashboard is fit to its rows
the way a completion list is, and refit as shells come and go.  Nil
leaves the window's height alone."
  :type '(choice (const :tag "Don't fit" nil) number))

(defcustom agent-hq-dashboard-quit-on-visit t
  "Whether visiting a shell from the dashboard closes the dashboard.
As picking from a completion list does.  Peeking at a shell and showing
it in another window keep the dashboard open either way."
  :type 'boolean)

(defcustom agent-hq-dashboard-key-hints
  "RET visit  o other window  TAB peek  r prompt  C-c C-c interrupt  T title  A account  v subagents  m magit  K remove worktree  D kill  d/u flag  ~ flag idle  x kill flagged  n/p next/previous  g refresh  q quit"
  "What `agent-hq-dashboard-help' says the keys are."
  :type 'string)

(defface agent-hq-dashboard-heading '((t :inherit bold))
  "Group headings.")

(defface agent-hq-dashboard-project '((t :inherit font-lock-keyword-face))
  "The repo:worktree a shell works in.")

(defface agent-hq-dashboard-answer '((t :inherit font-lock-doc-face))
  "What the agent said: its last answer, or what it's writing.")

(defface agent-hq-dashboard-flag '((t :inherit error :weight bold))
  "The D of a shell flagged for killing.")

;;;; Following each shell's turns

(defconst agent-hq-dashboard--text-limit 2000
  "Characters of the agent's latest words kept; only their first line shows.")

(defvar-local agent-hq-dashboard--tracked nil
  "Non-nil once this shell's events reach the dashboard.")

(defvar-local agent-hq-dashboard--turn-start nil
  "When this shell's latest turn started, as a float time.")

(defvar-local agent-hq-dashboard--turn-end nil
  "When this shell's latest turn ended, as a float time.")

(defvar-local agent-hq-dashboard--text nil
  "Chunks of what the agent wrote since its latest tool call, newest first.")

(defvar-local agent-hq-dashboard--text-length 0
  "Characters in `agent-hq-dashboard--text'.")

(defvar-local agent-hq-dashboard--text-done nil
  "Non-nil once a tool call followed `agent-hq-dashboard--text'.
The next chunk then starts the agent's words anew.")

(defvar-local agent-hq-dashboard--answer nil
  "First line of the agent's last words in its latest finished turn.")

(defvar-local agent-hq-dashboard--outcome nil
  "How the latest turn ended when it didn't simply finish, such as \"Cancelled\".")

(defun agent-hq-dashboard--plain (line)
  "Return LINE without Markdown list, quote and emphasis marks, trimmed."
  (string-trim
   (replace-regexp-in-string
    "\\*\\*\\|__\\|`" ""
    (replace-regexp-in-string "\\`[ \t]*\\(?:[-*+>]\\|[0-9]+[.)]\\)[ \t]+" "" line))))

(defun agent-hq-dashboard--first-line (text)
  "Return the first line of prose in Markdown TEXT, or nil.
Headings, rules, tables and code blocks are passed over; a heading is
the answer only when nothing else has words."
  (when (stringp text)
    (let ((lines (split-string text "\n"))
          fenced heading found)
      (while (and lines (not found))
        (let ((line (pop lines)))
          (cond ((string-match-p "\\`[ \t]*\\(?:```\\|~~~\\)" line)
                 (setq fenced (not fenced)))
                (fenced nil)
                ((string-match "\\`[ \t]*#+[ \t]+\\(.*\\)" line)
                 (let ((title (agent-hq-dashboard--plain (match-string 1 line))))
                   (unless (or heading (string-empty-p title))
                     (setq heading title))))
                ((string-match-p "\\`[ \t]*\\(?:|.*\\|[-=*_: ]*\\)\\'" line) nil)
                (t (let ((plain (agent-hq-dashboard--plain line)))
                     (unless (string-empty-p plain)
                       (setq found plain)))))))
      (or found heading))))

(defun agent-hq-dashboard--text-of (shell)
  "Return what SHELL's agent wrote since its latest tool call, or nil."
  (when-let* ((chunks (buffer-local-value 'agent-hq-dashboard--text shell)))
    (apply #'concat (reverse chunks))))

(defun agent-hq-dashboard--stop-text (reason)
  "Return how a turn that stopped for REASON ended, or nil for an ordinary end."
  (pcase reason
    ((or 'nil "end_turn") nil)
    ("cancelled" "Cancelled")
    ("max_tokens" "Stopped at the output token limit")
    ("max_turn_requests" "Stopped at the turn's request limit")
    ("refusal" "Refused")
    ((pred stringp) (concat "Stopped: " reason))))

(defun agent-hq-dashboard--note-event (shell event)
  "Remember what SHELL's EVENT says about its turn."
  (with-current-buffer shell
    (let ((data (map-elt event :data)))
      (pcase (map-elt event :event)
        ('input-submitted
         (setq agent-hq-dashboard--turn-start (float-time)
               agent-hq-dashboard--text nil
               agent-hq-dashboard--text-length 0
               agent-hq-dashboard--text-done nil
               agent-hq-dashboard--outcome nil))
        ('tool-call-update
         (setq agent-hq-dashboard--text-done t))
        ('agent-message-chunk
         (when-let* ((chunk (map-elt data :text-chunk))
                     ((stringp chunk)))
           (when agent-hq-dashboard--text-done
             (setq agent-hq-dashboard--text nil
                   agent-hq-dashboard--text-length 0
                   agent-hq-dashboard--text-done nil))
           (when (< agent-hq-dashboard--text-length agent-hq-dashboard--text-limit)
             (push chunk agent-hq-dashboard--text)
             (cl-incf agent-hq-dashboard--text-length (length chunk)))))
        ('turn-complete
         (setq agent-hq-dashboard--turn-end (float-time)
               agent-hq-dashboard--answer (agent-hq-dashboard--first-line
                                           (agent-hq-dashboard--text-of shell))
               agent-hq-dashboard--outcome (agent-hq-dashboard--stop-text
                                            (map-elt data :stop-reason))))
        ('error
         (setq agent-hq-dashboard--turn-end (float-time)
               agent-hq-dashboard--outcome
               (concat "Error: " (agent-hq-dashboard--one-line
                                  (format "%s" (or (map-elt data :message) "request failed"))))))))))

(defun agent-hq-dashboard--on-event (shell event)
  "Note SHELL's EVENT and redraw the dashboard soon.
An error here must not reach agent-shell, which dispatches the event."
  (when (buffer-live-p shell)
    (condition-case err
        (agent-hq-dashboard--note-event shell event)
      (error (message "agent-hq-dashboard: %s" (error-message-string err)))))
  (agent-hq-dashboard--schedule-refresh))

(defun agent-hq-dashboard--track (shell)
  "Follow SHELL's events for the dashboard, unless already following them."
  (when (and (buffer-live-p shell)
             (not (buffer-local-value 'agent-hq-dashboard--tracked shell)))
    (with-current-buffer shell
      (setq agent-hq-dashboard--tracked t)
      (agent-shell-subscribe-to
       :shell-buffer shell
       :on-event (lambda (event) (agent-hq-dashboard--on-event shell event))))))

(defun agent-hq-dashboard-track-h ()
  "Follow this shell's turns for the dashboard.  For `agent-shell-mode-hook'."
  (agent-hq-dashboard--track (current-buffer))
  (agent-hq-dashboard--schedule-refresh))

;;;###autoload
(defun agent-hq-dashboard-setup ()
  "Follow every agent shell's turns for the dashboard, open ones too."
  (add-hook 'agent-shell-mode-hook #'agent-hq-dashboard-track-h)
  (add-hook 'agent-hq-statusline-git-functions #'agent-hq-dashboard--schedule-refresh)
  (add-hook 'agent-hq-statusline-usage-functions #'agent-hq-dashboard--schedule-refresh)
  (mapc #'agent-hq-dashboard--track (agent-hq--shells)))

;;;; What each shell is up to

(defun agent-hq-dashboard--state (shell)
  "Return SHELL's agent-shell state, or nil."
  (ignore-errors (buffer-local-value 'agent-shell--state shell)))

(defun agent-hq-dashboard--attention (shell)
  "Return agent-shell-attention's entry for SHELL waiting on you, or nil."
  (and (boundp 'agent-shell-attention--pending)
       (hash-table-p agent-shell-attention--pending)
       (gethash shell agent-shell-attention--pending)))

(defun agent-hq-dashboard--unread-since (shell)
  "Return when SHELL's turn ended unseen, as agent-shell-attention noted it.
Nil when SHELL has no answer you haven't looked at.  Permission entries
don't count: those are a shell waiting, not one done."
  (let ((entry (agent-hq-dashboard--attention shell)))
    (when (and (consp entry) (not (plistp entry))
               (stringp (car entry))
               (not (string-prefix-p "Permission" (car entry))))
      (and (numberp (cdr entry)) (cdr entry)))))

(defun agent-hq-dashboard--group (shell status)
  "Return the group of SHELL, whose STATUS `agent-hq--status' gave."
  (pcase status
    ('blocked 'needs)
    ('busy 'running)
    ('ready (if (agent-hq-dashboard--unread-since shell) 'needs 'idle))
    ('asleep 'asleep)
    (_ 'starting)))

(defun agent-hq-dashboard--since (shell status)
  "Return when SHELL, of STATUS, began what it does now, as a float time, or nil."
  (let* ((local (lambda (var) (buffer-local-value var shell)))
         (start (funcall local 'agent-hq-dashboard--turn-start))
         (end (funcall local 'agent-hq-dashboard--turn-end))
         (activity (let ((time (agent-hq--activity shell))) (and (> time 0) time)))
         (busy-since (and (boundp 'agent-shell-attention--busy-since)
                          (hash-table-p agent-shell-attention--busy-since)
                          (let ((time (gethash shell agent-shell-attention--busy-since)))
                            (and (numberp time) time))))
         (entry (agent-hq-dashboard--attention shell)))
    (pcase status
      ('blocked (or (and (plistp entry) (numberp (plist-get entry :timestamp))
                         (plist-get entry :timestamp))
                    start busy-since activity))
      ('busy (or (and start (or (null end) (> start end)) start) busy-since activity))
      ('ready (or (agent-hq-dashboard--unread-since shell) end activity))
      (_ (and (boundp 'agent-hq-statusline--opened)
              (buffer-local-value 'agent-hq-statusline--opened shell))))))

(defun agent-hq-dashboard--age (seconds)
  "Return SECONDS as \"45s\", \"12m\", \"3h\" or \"2d\", or \"\" for nil."
  (cond ((not (numberp seconds)) "")
        ((< seconds 60) (format "%ds" (max 0 (floor seconds))))
        ((< seconds 3600) (format "%dm" (floor seconds 60)))
        ((< seconds 86400) (format "%dh" (floor seconds 3600)))
        (t (format "%dd" (floor seconds 86400)))))

(defun agent-hq-dashboard--one-line (text)
  "Return TEXT's first non-blank line, trimmed, with runs of spaces made one."
  (let ((line (seq-find (lambda (line) (not (string-blank-p line)))
                        (split-string (or text "") "\n"))))
    (if line
        (replace-regexp-in-string "[ \t]+" " " (string-trim line))
      "")))

(defun agent-hq-dashboard--tool-label (shell tool-call)
  "Return a line saying what TOOL-CALL, one of SHELL's, does, or nil.
A command shows its description, else the command after \"$ \"."
  (let* ((kind (map-elt tool-call :kind))
         (title (map-elt tool-call :title))
         (description (map-elt tool-call :description))
         (text (if (equal kind "execute")
                   (if (stringp description)
                       description
                     (and (stringp title) (concat "$ " title)))
                 (if (stringp title) title description))))
    (when (stringp text)
      (let ((short (with-current-buffer shell
                     (or (and (fboundp 'agent-shell--shorten-paths)
                              (ignore-errors (agent-shell--shorten-paths text)))
                         text))))
        (string-replace "`" "" (agent-hq-dashboard--one-line short))))))

(defun agent-hq-dashboard--current-tool (state)
  "Return the newest tool call in STATE that is pending or running, or nil.
agent-shell keeps the newest tool call first."
  (seq-some (lambda (entry)
              (let ((tool-call (cdr entry)))
                (and (member (map-elt tool-call :status) '(nil "pending" "in_progress"))
                     tool-call)))
            (map-elt state :tool-calls)))

(defun agent-hq-dashboard--permission-tool (state)
  "Return the tool call in STATE that waits for your permission, or nil."
  (seq-some (lambda (entry)
              (and (map-elt (cdr entry) :permission-request-id) (cdr entry)))
            (map-elt state :tool-calls)))

(defun agent-hq-dashboard--working-agents (shell)
  "Return how many of SHELL's subagents are at work, as far as is known."
  (let ((agents (and (boundp 'agent-hq-subagents--agents)
                     (fboundp 'agent-hq-subagents--status)
                     (buffer-local-value 'agent-hq-subagents--agents shell)))
        (now (float-time)))
    (if (hash-table-p agents)
        (seq-count (lambda (agent) (eq (agent-hq-subagents--status agent now) 'working))
                   (hash-table-values agents))
      0)))

(defun agent-hq-dashboard--latest (record)
  "Return the Latest column of RECORD: what its shell does, asks or said."
  (let* ((shell (plist-get record :shell))
         (state (agent-hq-dashboard--state shell))
         (local (lambda (var) (buffer-local-value var shell))))
    (pcase (plist-get record :status)
      ('blocked
       (if-let* ((tool (agent-hq-dashboard--permission-tool state)))
           (concat (propertize "Permission: " 'face 'error)
                   (or (agent-hq-dashboard--tool-label shell tool) "a tool call"))
         (propertize "A question waits for your answer" 'face 'error)))
      ('busy
       (let* ((tool (agent-hq-dashboard--current-tool state))
              (words (and (not (funcall local 'agent-hq-dashboard--text-done))
                          (agent-hq-dashboard--first-line (agent-hq-dashboard--text-of shell))))
              (agents (agent-hq-dashboard--working-agents shell))
              (queued (length (map-elt state :pending-prompts))))
         (string-join
          (delq nil (list (cond ((and tool (agent-hq-dashboard--tool-label shell tool)))
                                (words (propertize words 'face 'agent-hq-dashboard-answer))
                                (t (propertize "Thinking…" 'face 'shadow)))
                          (when (> agents 0)
                            (propertize (agent-hq--count agents "agent") 'face 'shadow))
                          (when (> queued 0)
                            (propertize (format "%d queued" queued) 'face 'shadow))))
          (propertize " · " 'face 'shadow))))
      ('ready
       (let ((outcome (funcall local 'agent-hq-dashboard--outcome))
             (answer (funcall local 'agent-hq-dashboard--answer))
             ;; Prompts still queued once the turn is over were held back,
             ;; as by an interrupt: they wait to be sent or dropped.
             (held (length (map-elt state :pending-prompts))))
         (string-join
          (delq nil (list (cond (outcome (propertize outcome 'face 'warning))
                                (answer (propertize answer 'face 'agent-hq-dashboard-answer)))
                          (when (> held 0)
                            (propertize (format "%d held" held) 'face 'warning))))
          (propertize " · " 'face 'shadow))))
      ('asleep (propertize "Starts once shown" 'face 'shadow))
      (_ (propertize "Starting…" 'face 'shadow)))))

(defun agent-hq-dashboard--model (shell)
  "Return the model SHELL's agent runs, plus its account when not the default."
  (let* ((state (agent-hq-dashboard--state shell))
         (choice (ignore-errors (agent-shell--current-model-id state)))
         (answered (and (boundp 'agent-hq-statusline--answered-with)
                        (buffer-local-value 'agent-hq-statusline--answered-with shell)))
         (name (or (and answered (equal (car answered) choice)
                        (agent-hq-statusline-model-name (cdr answered)))
                   (and choice (ignore-errors (agent-hq-statusline--described-model state choice)))
                   (and (stringp choice) (not (equal choice "default"))
                        (agent-hq-statusline-model-name choice))
                   (agent-hq--agent-name shell)))
         (account (agent-hq-statusline-account-label
                   (with-current-buffer shell (agent-hq-statusline--account)))))
    (concat name (when account
                   (concat " " (propertize account 'face 'agent-hq-statusline-account))))))

(defun agent-hq-dashboard--context (shell)
  "Return how much of its context SHELL's agent has used, as \"42%\", or \"\"."
  (let* ((usage (map-elt (agent-hq-dashboard--state shell) :usage))
         (used (map-elt usage :context-used))
         (size (map-elt usage :context-size)))
    (if (and (numberp used) (numberp size) (> size 0) (> used 0))
        (let ((percent (/ (* 100.0 used) size)))
          (propertize (format "%d%%" (round percent))
                      'face (agent-hq-statusline-usage-face percent)))
      "")))

;;;; Worktrees done with

(defvar agent-hq-dashboard--beyond (make-hash-table :test #'equal)
  "Commits each linked worktree's HEAD has beyond its base branch, by root.
Values are plists: :count, nil when git couldn't tell; :time, when git
was last asked; :process, the git running now.")

(defun agent-hq-dashboard--base (ctx)
  "Return the ref CTX's repo merges into.
That is `agent-hq-base-branch-alist''s, else origin/HEAD."
  (or (when-let* ((main (agent-hq--main-root (agent-hq-context-common-dir ctx))))
        (agent-hq--repo-setting agent-hq-base-branch-alist main))
      "origin/HEAD"))

(defun agent-hq-dashboard--base-name (ctx)
  "Return a short name for CTX's base branch: origin/develop gives \"develop\"."
  (let ((base (agent-hq-dashboard--base ctx)))
    (when (equal base "origin/HEAD")
      (when-let* ((line (agent-hq--read-first-line
                         (expand-file-name "refs/remotes/origin/HEAD"
                                           (agent-hq-context-common-dir ctx))))
                  ((string-match "\\`ref: refs/remotes/\\(.+\\)\\'" line)))
        (setq base (match-string 1 line))))
    (replace-regexp-in-string "\\`origin/" "" base)))

(defun agent-hq-dashboard--fetch-beyond (root base)
  "Ask git in the background how many commits ROOT's HEAD has beyond BASE."
  (let ((entry (plist-put (copy-sequence (gethash root agent-hq-dashboard--beyond))
                          :time (float-time)))
        (default-directory root)
        (buffer (generate-new-buffer " *agent-hq-dashboard-beyond*")))
    (condition-case nil
        (setq entry
              (plist-put entry :process
                         (make-process
                          :name "agent-hq-dashboard-beyond"
                          :buffer buffer
                          :command (list "git" "--no-optional-locks" "rev-list" "--count"
                                         "HEAD" "--not" base "--")
                          :connection-type 'pipe
                          :noquery t
                          :sentinel (lambda (process _)
                                      (agent-hq-dashboard--beyond-done process root)))))
      (error (kill-buffer buffer)))
    (puthash root entry agent-hq-dashboard--beyond)))

(defun agent-hq-dashboard--beyond-done (process root)
  "Store the count git PROCESS printed for ROOT; redraw if it changed."
  (unless (process-live-p process)
    (let* ((buffer (process-buffer process))
           (count (and (buffer-live-p buffer)
                       (eql (process-exit-status process) 0)
                       (with-current-buffer buffer
                         (let ((text (string-trim (buffer-string))))
                           (and (string-match-p "\\`[0-9]+\\'" text)
                                (string-to-number text))))))
           (old (plist-get (gethash root agent-hq-dashboard--beyond) :count)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (puthash root (list :count count :time (float-time) :process nil)
               agent-hq-dashboard--beyond)
      (unless (equal count old)
        (agent-hq-dashboard--schedule-refresh)))))

(defun agent-hq-dashboard--beyond-base (ctx)
  "Return how many commits CTX's HEAD has beyond its base, nil while unknown.
Asks git in the background when the count is older than
`agent-hq-dashboard-base-ttl'."
  (let* ((root (agent-hq-context-root ctx))
         (entry (gethash root agent-hq-dashboard--beyond)))
    (when (and (not agent-hq-statusline-no-fetch)
               (not (process-live-p (plist-get entry :process)))
               (> (- (float-time) (or (plist-get entry :time) 0)) agent-hq-dashboard-base-ttl))
      (agent-hq-dashboard--fetch-beyond root (agent-hq-dashboard--base ctx)))
    (plist-get entry :count)))

(defun agent-hq-dashboard--done (ctx data)
  "Return \"merged\" or \"in BASE\" when CTX's linked worktree looks done, or nil.
DATA is its git status, from `agent-hq-statusline-parse-status'.  Done
means clean and either its upstream is gone, as when a merged PR's
branch was deleted, or it has nothing beyond its base branch: what
`agent-hq-clean-worktrees' offers to remove once no shell is open there."
  (when (and ctx (agent-hq-context-worktree ctx) data
             (cl-every (lambda (key) (eql (plist-get data key) 0))
                       '(:staged :modified :untracked :conflicts)))
    (cond ((plist-get data :gone)
           (propertize "merged" 'face 'agent-shell-success
                       'help-echo "Its upstream branch is gone, as when a merged PR's branch is deleted"))
          ((eql (agent-hq-dashboard--beyond-base ctx) 0)
           (let ((base (agent-hq-dashboard--base-name ctx)))
             (propertize (concat "in " base) 'face 'agent-shell-success
                         'help-echo (format "Every commit on it is in %s" base)))))))

(defun agent-hq-dashboard--branch (git done width)
  "Return GIT, (BRANCH DATA STATE), as the header shows it, then DONE.
The branch is cut so the whole fits WIDTH columns, when it can be."
  (pcase-let ((`(,branch ,data ,state) git))
    (when branch
      (let* ((text (lambda (name)
                     (string-join (delq nil (list (agent-hq-statusline-format-git name data state)
                                                  done))
                                  " ")))
             (full (funcall text branch))
             (over (- (string-width full) (or width most-positive-fixnum))))
        (if (and (> over 0) (> (string-width branch) 8))
            (funcall text (truncate-string-to-width
                           branch (max 8 (- (string-width branch) over)) nil nil "…"))
          full)))))

;;;; The rows

(defun agent-hq-dashboard--record (shell now)
  "Return what the dashboard shows about SHELL at float time NOW, as a plist."
  (let* ((status (agent-hq--status shell))
         (since (agent-hq-dashboard--since shell status))
         (ctx (with-current-buffer shell (agent-hq-context)))
         (git (and ctx (or (agent-hq-statusline--git ctx)
                           (list (agent-hq-context-branch ctx) nil nil)))))
    (list :shell shell :status status
          :group (agent-hq-dashboard--group shell status)
          :ctx ctx :git git :done (agent-hq-dashboard--done ctx (nth 1 git))
          :since since :age (and since (- now since)))))

(defun agent-hq-dashboard--icon (record)
  "Return the state icon of RECORD's row."
  (pcase (plist-get record :group)
    ('needs (propertize "●" 'face (if (eq (plist-get record :status) 'blocked) 'error 'warning)))
    ('running (propertize (car agent-hq-spinner-frames) 'face 'success
                          'agent-hq-dashboard-spin t))
    ('idle (propertize "✓" 'face 'shadow))
    (_ (propertize "○" 'face 'shadow))))

(defun agent-hq-dashboard--project (record)
  "Return the Project column of RECORD: repo:worktree, and <N> for a twin shell."
  (let* ((shell (plist-get record :shell))
         (name (buffer-name shell)))
    (propertize
     (concat (propertize (agent-hq--shell-label shell (plist-get record :ctx))
                         'face 'agent-hq-dashboard-project)
             (when (string-match "<[0-9]+>\\'" name)
               (propertize (match-string 0 name) 'face 'shadow)))
     'help-echo (format "%s\n%s" name (abbreviate-file-name
                                       (buffer-local-value 'default-directory shell))))))

(defun agent-hq-dashboard--cells (record &optional branch-width)
  "Return RECORD's row as a vector of column strings.
BRANCH-WIDTH is the width the Branch column gets, if known yet."
  (let* ((shell (plist-get record :shell))
         (title (agent-hq--session-title shell))
         (latest (agent-hq-dashboard--latest record)))
    (vector (agent-hq-dashboard--icon record)
            (agent-hq-dashboard--project record)
            (or (agent-hq-dashboard--branch (plist-get record :git) (plist-get record :done)
                                            branch-width)
                "")
            (if title
                (propertize title 'help-echo title)
              (propertize "New session" 'face 'shadow))
            (propertize (agent-hq-dashboard--age (plist-get record :age)) 'face 'shadow)
            (if (string-empty-p latest)
                latest
              (propertize latest 'help-echo (substring-no-properties latest)))
            (agent-hq-dashboard--model shell)
            (agent-hq-dashboard--context shell))))

(defconst agent-hq-dashboard--names
  ["" "Project" "Branch" "Session" "Age" "Latest" "Model" "Ctx"]
  "Column names.")

(defconst agent-hq-dashboard--caps [1 26 34 42 4 nil 22 4]
  "Widest each column gets; Latest takes the room the others leave.")

(defconst agent-hq-dashboard--floors [1 12 14 16 4 24 0 4]
  "Narrowest a column shrinks to so that Latest keeps room.")

(defconst agent-hq-dashboard--latest 5
  "Index of the Latest column.")

(defun agent-hq-dashboard--widths (rows width)
  "Return column widths for ROWS, vectors of cells, in a WIDTH-column window.
Columns take what their cells need, up to `agent-hq-dashboard--caps'.  When
that leaves Latest less than its floor, Session, then Branch, then
Project give up columns, down to their own floors."
  (let* ((count (length agent-hq-dashboard--names))
         (latest agent-hq-dashboard--latest)
         (widths (vconcat
                  (cl-loop for i below count
                           collect (let ((need (apply #'max (string-width
                                                             (aref agent-hq-dashboard--names i))
                                                      (mapcar (lambda (row) (string-width (aref row i)))
                                                              rows)))
                                         (cap (aref agent-hq-dashboard--caps i)))
                                     (if cap (min need cap) need)))))
         (need-latest (aref widths latest))
         ;; Padding, then each column but the last followed by a space.
         (others (lambda ()
                   (+ tabulated-list-padding (1- count)
                      (cl-loop for i below count unless (= i latest) sum (aref widths i)))))
         (floor (aref agent-hq-dashboard--floors latest)))
    (dolist (i '(3 2 1))
      (let ((short (- (+ (funcall others) (min need-latest floor)) width)))
        (when (> short 0)
          ;; Never wider than it was: a column already under its floor stays.
          (aset widths i (max (min (aref widths i) (aref agent-hq-dashboard--floors i))
                              (- (aref widths i) short))))))
    (aset widths latest (max (min need-latest floor)
                             (min need-latest (- width (funcall others)))))
    widths))

(defun agent-hq-dashboard--set-format (widths)
  "Lay out the columns at WIDTHS, redoing the header if they changed."
  (let ((format (vconcat
                 (cl-loop for name across agent-hq-dashboard--names
                          for width across widths
                          collect (append (list name width nil)
                                          (when (member name '("Age" "Ctx"))
                                            '(:right-align t)))))))
    (unless (equal format tabulated-list-format)
      (setq tabulated-list-format format)
      (tabulated-list-init-header))))

(defconst agent-hq-dashboard--headings
  '((needs "Needs you" warning)
    (running "Running" success)
    (idle "Idle" shadow)
    (starting "Starting" shadow)
    ;; Restored shells whose agent starts once shown (agent-hq-restore).
    (asleep "Asleep" shadow))
  "The groups, in order: (GROUP NAME FACE).")

(defun agent-hq-dashboard--sorted (records group)
  "Return RECORDS of GROUP in their order.
Needs you: shells blocked on you, then those with an answer, each the
longest waiting first.  Others: most recent first."
  (sort (seq-filter (lambda (record) (eq (plist-get record :group) group)) records)
        (lambda (a b)
          (let ((ta (or (plist-get a :since) 0))
                (tb (or (plist-get b :since) 0))
                (ba (eq (plist-get a :status) 'blocked))
                (bb (eq (plist-get b :status) 'blocked)))
            (cond ((and (eq group 'needs) (not (eq ba bb))) ba)
                  ((/= ta tb) (if (eq group 'needs) (< ta tb) (> ta tb)))
                  (t (string< (buffer-name (plist-get a :shell))
                              (buffer-name (plist-get b :shell)))))))))

(defun agent-hq-dashboard--groups (records width)
  "Return `tabulated-list-groups' for RECORDS in a WIDTH-column window.
Also lays out the columns."
  (let* ((ordered (mapcar (lambda (heading)
                            (cons heading (agent-hq-dashboard--sorted records (car heading))))
                          agent-hq-dashboard--headings))
         (rows (mapcar (lambda (record) (cons record (agent-hq-dashboard--cells record)))
                       (apply #'append (mapcar #'cdr ordered))))
         (widths (agent-hq-dashboard--widths (mapcar #'cdr rows) width))
         (first t))
    (agent-hq-dashboard--set-format widths)
    (or (delq nil
              (mapcar
               (pcase-lambda (`((,_ ,name ,face) . ,members))
                 (when members
                   (prog1 (cons (concat (unless first "\n")
                                        (propertize (format "%s (%d)" name (length members))
                                                    'face (list 'agent-hq-dashboard-heading face)))
                                (mapcar (lambda (record)
                                          (let ((cells (copy-sequence
                                                        (cdr (assq record rows)))))
                                            ;; Now that its width is known, cut the
                                            ;; branch so the git counts stay in view.
                                            (aset cells 2 (or (agent-hq-dashboard--branch
                                                               (plist-get record :git)
                                                               (plist-get record :done)
                                                               (aref widths 2))
                                                              ""))
                                            (list (plist-get record :shell) cells)))
                                        members))
                     (setq first nil))))
               ordered))
        (list (list (propertize "No agent shells are running." 'face 'shadow))))))

(defun agent-hq-dashboard--summary (records)
  "Return the header line for RECORDS: each group's count, then plan usage."
  (let* ((counts (delq nil
                       (mapcar (pcase-lambda (`(,group ,name ,face))
                                 (let ((n (seq-count (lambda (r) (eq (plist-get r :group) group))
                                                     records)))
                                   (when (> n 0)
                                     (propertize (format "%s %d" name n) 'face face))))
                               agent-hq-dashboard--headings)))
         (accounts (seq-uniq (delq nil (mapcar (lambda (record)
                                                 (with-current-buffer (plist-get record :shell)
                                                   (agent-hq-statusline--account)))
                                               records))))
         (usage (delq nil
                      (mapcar (lambda (account)
                                (when-let* ((data (agent-hq-statusline--usage-data account))
                                            (text (agent-hq-statusline-format-usage data nil t)))
                                  (let ((label (or (agent-hq-statusline-account-label account)
                                                   (and (cdr accounts)
                                                        (or (bound-and-true-p
                                                             agent-hq-claude-default-account-name)
                                                            "default")))))
                                    (if label
                                        (concat (propertize label 'face 'agent-hq-statusline-account)
                                                " " text)
                                      text))))
                              accounts))))
    (agent-hq-statusline--escape-percent
     (concat " "
             (string-join (or counts (list (propertize "No shells" 'face 'shadow)))
                          (propertize " · " 'face 'shadow))
             (when usage
               (concat (propertize "    plan " 'face 'shadow)
                       (string-join usage (propertize "  │  " 'face 'shadow))))
             (propertize "    ? keys" 'face 'shadow)))))

;;;; Drawing

(defvar-local agent-hq-dashboard--flagged nil
  "Shells flagged for killing with \\<agent-hq-dashboard-mode-map>\\[agent-hq-dashboard-kill-flagged].")

(defvar-local agent-hq-dashboard--spinners nil
  "Overlays spinning over the icons of running rows.")

(defvar-local agent-hq-dashboard--width nil
  "Window width the dashboard was last laid out for.")

(defun agent-hq-dashboard--print-entry (id cols)
  "Print the row of shell ID with cells COLS, tagged D when flagged.
For `tabulated-list-printer'."
  (tabulated-list-print-entry id cols)
  (when (memq id agent-hq-dashboard--flagged)
    (save-excursion
      (forward-line -1)
      (tabulated-list-put-tag (propertize "D" 'face 'agent-hq-dashboard-flag)))))

(defun agent-hq-dashboard--position (id)
  "Return where shell ID's row starts, or nil."
  (when id
    (when-let* ((pos (text-property-any (point-min) (point-max) 'tabulated-list-id id)))
      (save-excursion (goto-char pos) (line-beginning-position)))))

(defun agent-hq-dashboard--spots ()
  "Return where point is and where each window showing the dashboard has it.
A list of (WINDOW ID LINE COLUMN); WINDOW is nil for the buffer's point."
  (mapcar (lambda (window)
            (save-excursion
              (goto-char (if window (window-point window) (point)))
              (list window (tabulated-list-get-id) (line-number-at-pos) (current-column))))
          (cons nil (get-buffer-window-list (current-buffer) nil t))))

(defun agent-hq-dashboard--restore (spots)
  "Put point, and each window's, back on the row SPOTS had it on.
Rows go by shell, so they follow a shell that moved group; a shell gone,
or a heading, keeps its line."
  (pcase-dolist (`(,window ,id ,line ,column) spots)
    (let ((pos (save-excursion
                 (if-let* ((start (agent-hq-dashboard--position id)))
                     (goto-char start)
                   (goto-char (point-min))
                   (forward-line (1- line)))
                 (move-to-column column)
                 (point))))
      (if (window-live-p window)
          (set-window-point window pos)
        (goto-char pos)))))

(defun agent-hq-dashboard--place-spinners ()
  "Put a spinner overlay over the icon of each running row."
  (mapc #'delete-overlay agent-hq-dashboard--spinners)
  (setq agent-hq-dashboard--spinners nil)
  (let ((pos (point-min)))
    (while (setq pos (text-property-not-all pos (point-max) 'agent-hq-dashboard-spin nil))
      (let ((overlay (make-overlay pos (1+ pos))))
        (overlay-put overlay 'evaporate t)
        (push overlay agent-hq-dashboard--spinners))
      (setq pos (1+ pos))))
  (when agent-hq-dashboard--spinners
    (agent-hq-dashboard--start-spinning)))

(defun agent-hq-dashboard--fit (window)
  "Fit side WINDOW's height to the dashboard's lines, if they changed.
Up to `agent-hq-dashboard-max-height' of the frame.  Only when the line
count differs from WINDOW's last fit, so a window resized by hand keeps
its size until shells come or go."
  (when (and agent-hq-dashboard-max-height
             (window-live-p window)
             (window-parameter window 'window-side))
    (let ((lines (count-lines (point-min) (point-max))))
      (unless (eql lines (window-parameter window 'agent-hq-dashboard-lines))
        (set-window-parameter window 'agent-hq-dashboard-lines lines)
        (fit-window-to-buffer
         window
         (max window-min-height
              (floor (* agent-hq-dashboard-max-height (frame-height (window-frame window)))))
         window-min-height)))))

(defvar agent-hq-dashboard--fetch nil
  "Non-nil while a redraw may start git status and plan usage fetches.
Only opening the dashboard, its tick and \\[agent-hq-dashboard-refresh]
do: redraws on shell events come up to twice a second and show what
was last fetched.")

;;;###autoload
(defun agent-hq-dashboard-refresh ()
  "Redraw the dashboard from the shells as they are now.
Interactively, also ask git and the plan usage again where stale."
  (interactive)
  (unless (derived-mode-p 'agent-hq-dashboard-mode)
    (user-error "Not in the agents dashboard"))
  (let* ((agent-hq-statusline-no-fetch
          (not (or agent-hq-dashboard--fetch (called-interactively-p 'any))))
         (now (float-time))
         (window (get-buffer-window (current-buffer) t))
         (width (if window (window-max-chars-per-line window) (frame-width)))
         (records (mapcar (lambda (shell) (agent-hq-dashboard--record shell now))
                          (agent-hq--shells)))
         (spots (agent-hq-dashboard--spots)))
    (setq agent-hq-dashboard--flagged (seq-filter #'buffer-live-p agent-hq-dashboard--flagged)
          agent-hq-dashboard--width width
          tabulated-list-entries nil
          tabulated-list-groups (agent-hq-dashboard--groups records width)
          header-line-format (agent-hq-dashboard--summary records))
    (tabulated-list-print)
    (agent-hq-dashboard--restore spots)
    (agent-hq-dashboard--place-spinners)
    (mapc #'agent-hq-dashboard--fit (get-buffer-window-list (current-buffer) nil t))))

(defun agent-hq-dashboard--buffer ()
  "Return the dashboard buffer if it exists."
  (when-let* ((buffer (get-buffer agent-hq-dashboard-buffer-name)))
    (and (buffer-live-p buffer)
         (eq (buffer-local-value 'major-mode buffer) 'agent-hq-dashboard-mode)
         buffer)))

(defun agent-hq-dashboard--refresh-if-shown ()
  "Redraw the dashboard if a window shows it."
  (when-let* ((buffer (agent-hq-dashboard--buffer))
              ((get-buffer-window buffer 'visible)))
    (with-current-buffer buffer
      (with-demoted-errors "agent-hq-dashboard: %S"
        (agent-hq-dashboard-refresh)))))

(defvar agent-hq-dashboard--soon nil
  "Timer of a redraw coming soon.")

(defun agent-hq-dashboard--redraw-soon ()
  "Run the redraw `agent-hq-dashboard--schedule-refresh' scheduled."
  (setq agent-hq-dashboard--soon nil)
  (agent-hq-dashboard--refresh-if-shown))

(defun agent-hq-dashboard--schedule-refresh (&rest _)
  "Redraw the dashboard in half a second, if a window shows it.
Events come many a second while an agent writes; this redraws at most
twice a second."
  (unless (timerp agent-hq-dashboard--soon)
    (when-let* ((buffer (agent-hq-dashboard--buffer))
                ((get-buffer-window buffer 'visible)))
      (setq agent-hq-dashboard--soon (run-at-time 0.5 nil #'agent-hq-dashboard--redraw-soon)))))

(defvar agent-hq-dashboard--timer nil
  "Redraws the dashboard every `agent-hq-dashboard-tick' seconds while shown.")

(defun agent-hq-dashboard--tick ()
  "Redraw the dashboard if shown, fetching what's stale; stop once it's gone."
  (if (agent-hq-dashboard--buffer)
      (let ((agent-hq-dashboard--fetch t))
        (agent-hq-dashboard--refresh-if-shown))
    (cancel-timer agent-hq-dashboard--timer)
    (setq agent-hq-dashboard--timer nil)))

(defvar agent-hq-dashboard--frames nil
  "(FRAMES . STRINGS): `agent-hq-spinner-frames' made into icons once.")

(defun agent-hq-dashboard--frame-strings ()
  "Return the spinner frames as icon strings, made when the frames change."
  (unless (eq (car agent-hq-dashboard--frames) agent-hq-spinner-frames)
    (setq agent-hq-dashboard--frames
          (cons agent-hq-spinner-frames
                (mapcar (lambda (frame) (propertize frame 'face 'success))
                        agent-hq-spinner-frames))))
  (cdr agent-hq-dashboard--frames))

(defun agent-hq-dashboard--spin (beat)
  "Show BEAT's frame on the running rows; stop when none is in view.
On the clock every agent-hq animation shares (agent-hq-clock)."
  (let ((buffer (agent-hq-dashboard--buffer)))
    (if (not (and buffer
                  (buffer-local-value 'agent-hq-dashboard--spinners buffer)
                  (get-buffer-window buffer 'visible)))
        (agent-hq-clock-stop 'agent-hq-dashboard)
      (let* ((frames (agent-hq-dashboard--frame-strings))
             (frame (nth (mod beat (length frames)) frames)))
        (dolist (overlay (buffer-local-value 'agent-hq-dashboard--spinners buffer))
          (overlay-put overlay 'display frame))))))

(defun agent-hq-dashboard--start-spinning ()
  "Turn the running rows' spinners on the shared clock."
  (agent-hq-clock-start 'agent-hq-dashboard #'agent-hq-dashboard--spin))

(defun agent-hq-dashboard--selected-h (window)
  "Redraw soon when WINDOW, showing the dashboard, gets selected.
For a buffer-local `window-selection-change-functions', which run as
redisplay starts: the redraw waits for a timer."
  (when (and (eq window (selected-window))
             (eq (window-buffer window) (current-buffer)))
    (agent-hq-dashboard--schedule-refresh)))

(defun agent-hq-dashboard--resized-h (window)
  "Lay the dashboard out again when WINDOW, showing it, changed width.
For a buffer-local `window-size-change-functions'."
  (when (and (eq (window-buffer window) (current-buffer))
             (not (eql (window-max-chars-per-line window) agent-hq-dashboard--width)))
    (agent-hq-dashboard--schedule-refresh)))

;;;; Commands

(defun agent-hq-dashboard--shell ()
  "Return the shell of the row at point, or signal a `user-error'."
  (let ((shell (tabulated-list-get-id)))
    (cond ((null shell) (user-error "No shell on this line"))
          ((not (buffer-live-p shell))
           (agent-hq-dashboard-refresh)
           (user-error "That shell is gone"))
          (t shell))))

(defun agent-hq-dashboard--acknowledge (shell)
  "Count SHELL's answer as seen, as visiting it does."
  (when (fboundp 'agent-shell-attention-acknowledge)
    (agent-shell-attention-acknowledge shell)))

(defun agent-hq-dashboard-visit ()
  "Show this row's shell in the agent window, in its workspace.
The dashboard closes, as a completion list does once you pick, unless
`agent-hq-dashboard-quit-on-visit' is nil; \\<agent-hq-dashboard-mode-map>\
\\[agent-hq-dashboard-peek] and \\[agent-hq-dashboard-visit-other-window] keep it open."
  (interactive)
  (let ((shell (agent-hq-dashboard--shell)))
    (agent-hq-dashboard--acknowledge shell)
    (when (and agent-hq-dashboard-quit-on-visit
               (eq (window-buffer (selected-window)) (current-buffer)))
      (quit-window))
    (agent-hq-visit-shell shell)))

(defun agent-hq-dashboard-visit-other-window ()
  "Show this row's shell in another window and select it."
  (interactive)
  (let ((shell (agent-hq-dashboard--shell)))
    (agent-hq-dashboard--acknowledge shell)
    (switch-to-buffer-other-window shell)))

(defun agent-hq-dashboard-peek ()
  "Show this row's shell in the agent window, staying in the dashboard."
  (interactive)
  (display-buffer (agent-hq-dashboard--shell) (bound-and-true-p agent-shell-display-action)))

(defun agent-hq-dashboard--read-prompt (shell)
  "Read a prompt for SHELL, completing @files and /commands as its prompt does."
  (let ((busy (memq (agent-hq--status shell) '(busy blocked))))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (boundp 'agent-shell-prompt-queue-setup-minibuffer-functions)
            (run-hook-with-args 'agent-shell-prompt-queue-setup-minibuffer-functions
                                `((:shell-buffer . ,shell)))))
      (read-string (format "%s %s: " (if busy "Queue for" "Send to")
                           (agent-hq--shell-label shell (with-current-buffer shell
                                                          (agent-hq-context))))))))

(defun agent-hq-dashboard-send (shell prompt)
  "Send PROMPT to SHELL, the row's, or queue it while SHELL works.
A queued prompt goes once the turn under way ends, as with agent-shell's
own queue."
  (interactive (let ((shell (agent-hq-dashboard--shell)))
                 (list shell (agent-hq-dashboard--read-prompt shell))))
  (when (string-blank-p prompt)
    (user-error "No prompt given"))
  (with-current-buffer shell
    (if (and (fboundp 'shell-maker-busy) (shell-maker-busy))
        (agent-shell--prompt-queue-enqueue :prompt prompt)
      (agent-shell-insert :text prompt :submit t :no-focus t :shell-buffer shell)
      (message "Sent to %s" (buffer-name shell))))
  (agent-hq-dashboard--schedule-refresh))

(defun agent-hq-dashboard-interrupt ()
  "Stop the turn this row's shell is working on, as \\[keyboard-quit] would there."
  (interactive)
  (let ((shell (agent-hq-dashboard--shell)))
    (unless (memq (agent-hq--status shell) '(busy blocked))
      (user-error "%s isn't working" (buffer-name shell)))
    (with-current-buffer shell
      (agent-shell-interrupt))
    (agent-hq-dashboard--schedule-refresh)))

(defun agent-hq-dashboard-rename (shell title)
  "Set the session TITLE of SHELL, the row's; see `agent-hq-rename-session'."
  (interactive (let* ((shell (agent-hq-dashboard--shell))
                      (old (or (agent-hq--session-title shell) "")))
                 (list shell (read-string "Session title: " old nil old))))
  (agent-hq-rename-session title shell))

(defun agent-hq-dashboard-switch-account ()
  "Move this row's Claude shell to another account; see `agent-hq-switch-account'."
  (interactive)
  (let ((shell (agent-hq-dashboard--shell)))
    (require 'agent-hq-fallback)
    (with-current-buffer shell
      (call-interactively #'agent-hq-switch-account))
    (agent-hq-dashboard--schedule-refresh)))

(defun agent-hq-dashboard-subagents ()
  "Pick one of this row's shell's subagents and show its transcript."
  (interactive)
  (let ((shell (agent-hq-dashboard--shell)))
    (require 'agent-hq-subagents)
    (with-current-buffer shell
      (call-interactively #'agent-hq-subagents-view))))

(defun agent-hq-dashboard-magit ()
  "Show magit's status of this row's worktree, in its workspace."
  (interactive)
  (let* ((shell (agent-hq-dashboard--shell))
         (ctx (or (with-current-buffer shell (agent-hq-context))
                  (user-error "%s doesn't work in git" (buffer-name shell)))))
    (require 'magit)
    (agent-hq-switch-to-workspace-of shell)
    (magit-status-setup-buffer (agent-hq-context-root ctx))))

(defun agent-hq-dashboard-remove-worktree (&optional force)
  "Remove this row's linked worktree; see `agent-hq-remove-worktree'.
With FORCE (prefix argument), uncommitted changes go too."
  (interactive "P")
  (let* ((shell (agent-hq-dashboard--shell))
         (ctx (with-current-buffer shell (agent-hq-context))))
    (unless (and ctx (agent-hq-context-worktree ctx))
      (user-error "%s works in a main checkout, not a linked worktree" (buffer-name shell)))
    (agent-hq-remove-worktree-at ctx force)
    (agent-hq-dashboard-refresh)))

(defun agent-hq-dashboard-kill ()
  "Kill this row's shell, asking first only while it works or waits on you."
  (interactive)
  (agent-hq-kill-shell (agent-hq-dashboard--shell))
  (agent-hq-dashboard-refresh))

(defun agent-hq-dashboard-next (&optional n)
  "Move to the Nth next shell's row, past group headings."
  (interactive "p")
  (let ((step (if (< (or n 1) 0) -1 1)))
    (dotimes (_ (abs (or n 1)))
      (let ((start (point)))
        (forward-line step)
        (while (and (not (tabulated-list-get-id))
                    (not (if (> step 0) (eobp) (bobp))))
          (forward-line step))
        (unless (tabulated-list-get-id)
          (goto-char start))))))

(defun agent-hq-dashboard-previous (&optional n)
  "Move to the Nth previous shell's row, past group headings."
  (interactive "p")
  (agent-hq-dashboard-next (- (or n 1))))

(defun agent-hq-dashboard-flag ()
  "Flag this row's shell for killing with \\[agent-hq-dashboard-kill-flagged]."
  (interactive)
  (cl-pushnew (agent-hq-dashboard--shell) agent-hq-dashboard--flagged)
  (tabulated-list-put-tag (propertize "D" 'face 'agent-hq-dashboard-flag))
  (agent-hq-dashboard-next))

(defun agent-hq-dashboard-unflag ()
  "Take the kill flag off this row's shell."
  (interactive)
  (setq agent-hq-dashboard--flagged (delq (agent-hq-dashboard--shell) agent-hq-dashboard--flagged))
  (tabulated-list-put-tag " ")
  (agent-hq-dashboard-next))

(defun agent-hq-dashboard-unflag-all ()
  "Take the kill flag off every shell."
  (interactive)
  (setq agent-hq-dashboard--flagged nil)
  (agent-hq-dashboard-refresh))

(defun agent-hq-dashboard-flag-idle ()
  "Flag every idle shell for killing: done, and its answer seen.
Their sessions stay, so \\[agent-hq-resume-session] brings any back."
  (interactive)
  (let ((idle (seq-filter (lambda (shell)
                            (eq (agent-hq-dashboard--group shell (agent-hq--status shell)) 'idle))
                          (agent-hq--shells))))
    (unless idle
      (user-error "No idle shells"))
    (setq agent-hq-dashboard--flagged (seq-uniq (append agent-hq-dashboard--flagged idle)))
    (agent-hq-dashboard-refresh)
    (message "Flagged %s; %s kills them"
             (agent-hq--count (length idle) "idle shell")
             (substitute-command-keys "\\[agent-hq-dashboard-kill-flagged]"))))

(defun agent-hq-dashboard-kill-flagged ()
  "Kill every flagged shell, after one question.
Their sessions stay, so \\[agent-hq-resume-session] brings any back."
  (interactive)
  (let* ((shells (seq-filter #'buffer-live-p agent-hq-dashboard--flagged))
         (working (seq-count (lambda (shell) (memq (agent-hq--status shell) '(busy blocked)))
                             shells))
         (queued (seq-count (lambda (shell)
                              (map-elt (agent-hq-dashboard--state shell) :pending-prompts))
                            shells))
         (at-stake (delq nil (list (when (> working 0)
                                     (format "%d working or waiting on you" working))
                                   (when (> queued 0)
                                     (format "%d with prompts queued" queued))))))
    (unless shells
      (user-error "No shells flagged (%s flags one, %s every idle one)"
                  (substitute-command-keys "\\[agent-hq-dashboard-flag]")
                  (substitute-command-keys "\\[agent-hq-dashboard-flag-idle]")))
    (when (y-or-n-p (format "Kill %s%s? " (agent-hq--count (length shells) "shell")
                            (if at-stake (format " (%s)" (string-join at-stake ", ")) "")))
      (mapc #'agent-hq--kill-shell-buffer shells)
      (setq agent-hq-dashboard--flagged nil)
      (agent-hq-dashboard-refresh)
      (message "Killed %s" (agent-hq--count (length shells) "shell")))))

(defun agent-hq-dashboard-help ()
  "Say which keys the dashboard has."
  (interactive)
  (message "%s" agent-hq-dashboard-key-hints))

(defvar-keymap agent-hq-dashboard-mode-map
  :doc "Keys of `agent-hq-dashboard-mode'.
Only keys of its own: Doom copies them to Evil's normal state, where
j/k, gg and gr stay Evil's (gr refreshes)."
  :parent tabulated-list-mode-map
  "RET" #'agent-hq-dashboard-visit
  "<return>" #'agent-hq-dashboard-visit
  "o" #'agent-hq-dashboard-visit-other-window
  "TAB" #'agent-hq-dashboard-peek
  "<tab>" #'agent-hq-dashboard-peek
  "r" #'agent-hq-dashboard-send
  "C-c C-c" #'agent-hq-dashboard-interrupt
  "T" #'agent-hq-dashboard-rename
  "A" #'agent-hq-dashboard-switch-account
  "v" #'agent-hq-dashboard-subagents
  "m" #'agent-hq-dashboard-magit
  "K" #'agent-hq-dashboard-remove-worktree
  "D" #'agent-hq-dashboard-kill
  "d" #'agent-hq-dashboard-flag
  "u" #'agent-hq-dashboard-unflag
  "U" #'agent-hq-dashboard-unflag-all
  "~" #'agent-hq-dashboard-flag-idle
  "x" #'agent-hq-dashboard-kill-flagged
  "n" #'agent-hq-dashboard-next
  "p" #'agent-hq-dashboard-previous
  "?" #'agent-hq-dashboard-help
  "q" #'quit-window)

(define-derived-mode agent-hq-dashboard-mode tabulated-list-mode "Agents"
  "Every agent shell, grouped by what it needs from you.
See `agent-hq-dashboard'.

\\{agent-hq-dashboard-mode-map}"
  (setq tabulated-list-padding 2
        tabulated-list-use-header-line nil
        tabulated-list-sort-key nil
        tabulated-list-printer #'agent-hq-dashboard--print-entry
        tabulated-list-format (vconcat (mapcar (lambda (name) (list name 1 nil))
                                               agent-hq-dashboard--names)))
  (setq-local revert-buffer-function (lambda (&rest _)
                                       (let ((agent-hq-dashboard--fetch t))
                                         (agent-hq-dashboard-refresh))))
  (add-hook 'window-selection-change-functions #'agent-hq-dashboard--selected-h nil t)
  (add-hook 'window-size-change-functions #'agent-hq-dashboard--resized-h nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun agent-hq-dashboard ()
  "Show every agent shell in every project, grouped by what it needs from you.
See `agent-hq-dashboard-mode' for what it shows and its keys."
  (interactive)
  (agent-hq-dashboard-setup)
  (let ((buffer (get-buffer-create agent-hq-dashboard-buffer-name))
        (agent-hq-dashboard--fetch t))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-hq-dashboard-mode)
        (setq default-directory (expand-file-name "~/"))
        (agent-hq-dashboard-mode))
      ;; Laid out once before it shows, so a window fit to it has the rows.
      (agent-hq-dashboard-refresh))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (if (eql (window-max-chars-per-line) agent-hq-dashboard--width)
          (agent-hq-dashboard--fit (selected-window))
        (agent-hq-dashboard-refresh)))
    (unless (timerp agent-hq-dashboard--timer)
      (setq agent-hq-dashboard--timer
            (run-at-time agent-hq-dashboard-tick agent-hq-dashboard-tick
                         #'agent-hq-dashboard--tick)))))

(provide 'agent-hq-dashboard)
;;; agent-hq-dashboard.el ends here
