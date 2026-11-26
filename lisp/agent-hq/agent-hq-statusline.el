;;; agent-hq-statusline.el --- Status line for agent-shell: header and mode line -*- lexical-binding: t; -*-

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
;;   `agent-hq-statusline-git-ttl' seconds and after magit refreshes the repo.
;; - The model Claude actually answered with, where agent-shell shows the
;;   picker's label ("Default (recommended)").  Before the first answer it
;;   is read from the picker's description ("Opus 5.5 · Best for …").
;; - The account's 5-hour and weekly plan usage, with when each resets.
;;   Each account (see `agent-hq-claude-config-dirs') has its own.  Claude
;;   Code reports them through the Agent SDK's usage call, the data behind
;;   /usage (agent-hq-usage.mjs), so nothing here reads credentials and no
;;   model is called.  They're fetched in the background and kept for
;;   `agent-hq-statusline-usage-ttl' seconds.  A window the plan doesn't have
;;   (or that isn't running) is left out.  An account other than ~/.claude
;;   is named first ("work" for ~/.claude-work).  The percentages turn
;;   yellow at 60% and red at 85%, as agent-shell's context indicator does.
;; - How long the shell has been open, refreshed every minute.
;;
;; A window too narrow for a line would clip it, so the second line drops
;; the reset times, then cuts the branch, then drops the session time,
;; and the first drops the cost, until they fit (`agent-hq-statusline--fits').
;;
;; agent-shell draws the graphical header's second line in one color;
;; `agent-hq-statusline-render-a' gives each part above its own.  Codex shells
;; get the git status and session time.
;;
;; The session title goes in the mode line, not the header, which has no
;; room for it: `agent-hq-mode-line-title' is a mode-line segment to use
;; in place of the buffer name, whose agent, repo, worktree and branch the
;; header already shows.  `agent-hq-rename-session' edits the title.
;;
;; Turn it all on with
;; `agent-hq-statusline-setup'.
;;
;; `agent-hq-show-usage' prints the session's context, tokens and cost
;; with the plan usage, and unlike agent-shell's own command it works
;; before the agent has answered.

;;; Code:

(require 'agent-hq)
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
(declare-function agent-shell--send-request "agent-shell")
(declare-function agent-shell--set-session-title "agent-shell")
(declare-function svg-image "svg")
(declare-function agent-hq-attention-mode-line-width "agent-hq-attention")
(defvar agent-shell--state)
(defvar agent-shell-header-style)

(defgroup agent-hq-statusline nil
  "Model, plan usage and session time in agent-shell headers."
  :group 'agent-hq
  :prefix "agent-hq-statusline-")

(defcustom agent-hq-statusline-usage-ttl 600
  "Seconds plan usage is shown before it is fetched again.
Each fetch runs node and the Claude Code CLI for a few seconds, and only
for an account a window shows a shell of (or the dashboard)."
  :type 'natnum)

(defvar agent-hq-statusline-no-fetch nil
  "When non-nil, git status and plan usage are shown as last fetched.
No git and no usage fetch starts.  Bound while drawing a header no
window shows, and by redraws that come too often to fetch on.")

(defcustom agent-hq-statusline-claude-sdk nil
  "The Agent SDK's sdk.mjs, or nil for the copy claude-agent-acp ships."
  :type '(choice (const :tag "claude-agent-acp's" nil) file))

(defcustom agent-hq-statusline-show-git t
  "Non-nil to show the shell's branch and git status in its header."
  :type 'boolean)

(defcustom agent-hq-statusline-git-ttl 5
  "Seconds a worktree's git status is shown before git is asked again."
  :type 'number)

(defface agent-hq-statusline-branch '((t :inherit font-lock-keyword-face))
  "The branch, and commits ahead or behind, in agent-shell headers.")

(defface agent-hq-statusline-account '((t :inherit warning :weight bold))
  "A Claude account other than ~/.claude, in agent-shell headers.")

(defconst agent-hq-statusline--script
  (expand-file-name "agent-hq-usage.mjs"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The script that prints an account's plan usage.")

;;;; The model Claude answered with

(defvar-local agent-hq-statusline--answered-with nil
  "(CHOICE . MODEL): Claude last answered here with MODEL, picker on CHOICE.")

(defun agent-hq-statusline-model-name (model)
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

(defun agent-hq-statusline--leading-model (description)
  "Return the model DESCRIPTION starts with, as in \"Opus 5.5 · Best for …\"."
  (when (and (stringp description)
             (string-match "\\`\\([^·\n]+?\\) *·" description))
    (match-string 1 description)))

(defun agent-hq-statusline--described-model (state choice)
  "Return the model that picker entry CHOICE stands for in STATE, or nil.
Its description either starts with it (\"Opus 5.5 · Best for …\") or, as
for \"default\" in Claude's config options, names another entry (\"Opus\"),
whose description or name then gives it (\"Opus 5.5\")."
  (let* ((models (agent-shell--get-available-models state))
         (entry-for (lambda (id)
                      (seq-find (lambda (model) (equal (map-elt model :model-id) id)) models)))
         (description (map-elt (funcall entry-for choice) :description)))
    (when (stringp description)
      (or (agent-hq-statusline--leading-model description)
          (when-let* ((alias (funcall entry-for (downcase (string-trim description))))
                      ((not (equal (map-elt alias :model-id) choice))))
            (or (agent-hq-statusline--leading-model (map-elt alias :description))
                (map-elt alias :name)))))))

(defun agent-hq-statusline--model (state label)
  "Return the model to show for STATE in place of LABEL, the picker's name."
  (let ((choice (agent-shell--current-model-id state)))
    (if (and agent-hq-statusline--answered-with
             (equal (car agent-hq-statusline--answered-with) choice))
        (agent-hq-statusline-model-name (cdr agent-hq-statusline--answered-with))
      (or (agent-hq-statusline--described-model state choice) label))))

(defun agent-hq-statusline--note-model (buffer event)
  "Remember the model of BUFFER's usage-update EVENT, and redraw if it's new."
  (when-let* ((model (map-nested-elt event '(:data :meta _claude/model)))
              ((stringp model))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((seen (cons (agent-shell--current-model-id agent-shell--state) model)))
        (unless (equal seen agent-hq-statusline--answered-with)
          (setq agent-hq-statusline--answered-with seen)
          (agent-shell--update-header-and-mode-line))))))

;;;; Plan usage per account

(defvar agent-hq-statusline--usage (make-hash-table :test #'equal)
  "Plan usage per account, keyed by CLAUDE_CONFIG_DIR (\"\" for the default).
Each value is a plist: :data, the parsed JSON; :time, when it was last
fetched or tried; :process, the fetch running now.")

(defun agent-hq-statusline--account ()
  "Return this shell's usage key, or nil if it isn't a Claude shell.
Only Claude clients are made through `agent-hq-claude-environment-a',
which records the account."
  (pcase agent-hq--claude-started-with
    ('nil nil)
    ('default "")
    (dir dir)))

(defun agent-hq-statusline-acp-root ()
  "Return the directory claude-agent-acp is installed in, or nil."
  (when-let* ((acp (executable-find "claude-agent-acp")))
    (locate-dominating-file (file-truename acp) "package.json")))

(defun agent-hq-statusline--usage-command ()
  "Return the command that prints plan usage, or nil if node or the SDK is missing."
  (let* ((acp (executable-find "claude-agent-acp"))
         (sdk (or agent-hq-statusline-claude-sdk
                  (when-let* ((root (agent-hq-statusline-acp-root)))
                    (expand-file-name "node_modules/@anthropic-ai/claude-agent-sdk/sdk.mjs"
                                      root))))
         ;; The node claude-agent-acp runs on, so fnm's PATH doesn't matter.
         (node (or (and acp (let ((node (expand-file-name "node" (file-name-directory acp))))
                              (and (file-executable-p node) node)))
                   (executable-find "node"))))
    (when (and node sdk (file-readable-p sdk))
      (list node agent-hq-statusline--script sdk))))

(defun agent-hq-statusline--parse-usage (buffer)
  "Return the usage JSON in BUFFER's last line that starts with \"{\"."
  (with-current-buffer buffer
    (goto-char (point-max))
    (when (re-search-backward "^{" nil t)
      (ignore-errors (json-parse-buffer :object-type 'alist :null-object nil)))))

(defun agent-hq-statusline--fetch-usage (account)
  "Start fetching ACCOUNT's plan usage unless a fetch is already running."
  (let ((entry (gethash account agent-hq-statusline--usage)))
    (unless (process-live-p (plist-get entry :process))
      (setq entry (plist-put (copy-sequence entry) :time (float-time)))
      (when-let* ((command (agent-hq-statusline--usage-command)))
        (let* ((default-directory (expand-file-name "~/"))
               (process-environment
                (cons (if (string-empty-p account)
                          "CLAUDE_CONFIG_DIR"   ; unset: ~/.claude
                        (concat "CLAUDE_CONFIG_DIR=" (directory-file-name account)))
                      process-environment)))
          (setq entry (plist-put entry :process
                                 (make-process
                                  :name "agent-hq-usage"
                                  :buffer (generate-new-buffer " *agent-hq-usage*")
                                  :command command
                                  :connection-type 'pipe
                                  :noquery t
                                  :sentinel (lambda (process _)
                                              (agent-hq-statusline--usage-done process account)))))))
      (puthash account entry agent-hq-statusline--usage))))

(defun agent-hq-statusline--usage-done (process account)
  "Store what PROCESS fetched for ACCOUNT and redraw that account's shells.
A failed fetch keeps the last usage; it's tried again after the TTL."
  (unless (process-live-p process)
    (let* ((buffer (process-buffer process))
           (data (and (buffer-live-p buffer)
                      (eql (process-exit-status process) 0)
                      (agent-hq-statusline--parse-usage buffer)))
           (entry (gethash account agent-hq-statusline--usage)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (puthash account
               (list :data (or data (plist-get entry :data)) :time (float-time) :process nil)
               agent-hq-statusline--usage)
      (when data
        (agent-hq-statusline--redraw
         (lambda (shell)
           (equal (with-current-buffer shell (agent-hq-statusline--account)) account)))
        (run-hook-with-args 'agent-hq-statusline-usage-functions account)))))

(defun agent-hq-statusline--clock (time with-day zone)
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

(defun agent-hq-statusline-usage-face (percent)
  "Return the face for plan usage at PERCENT, as agent-shell colors context."
  (cond ((>= percent 85) 'agent-shell-error)
        ((>= percent 60) 'agent-shell-warning)
        (t 'agent-shell-success)))

(defun agent-hq-statusline-account-label (account)
  "Return the name to show for ACCOUNT, a usage key, or nil for ~/.claude.
~/.claude-work gives \"work\"; a dir not named .claude-NAME gives its name."
  (unless (or (null account) (string-empty-p account))
    (let ((dir (file-name-nondirectory (directory-file-name account))))
      (if (string-match "\\`\\.?claude[-_.]\\(.+\\)\\'" dir)
          (match-string 1 dir)
        dir))))

(defun agent-hq-statusline--window (label window with-day zone &optional no-reset)
  "Return \"LABEL N% (RESET)\" for WINDOW, or nil without a percentage.
RESET is a clock time, with the day when WITH-DAY, in time zone ZONE;
NO-RESET leaves it out."
  (when-let* ((window)
              (percent (alist-get 'utilization window))
              ((numberp percent)))
    (concat label " " (propertize (format "%d%%" (round percent))
                                  'face (agent-hq-statusline-usage-face percent))
            (when-let* (((not no-reset))
                        (resets (alist-get 'resets_at window))
                        ((stringp resets))
                        (time (ignore-errors (encode-time (iso8601-parse resets)))))
              (format " (%s)" (agent-hq-statusline--clock time with-day zone))))))

(defun agent-hq-statusline-format-usage (data &optional zone no-resets)
  "Return \"5h 2% (12:10pm) · week 3% (Mon 7pm)\" for DATA, or nil.
DATA is what agent-hq-usage.mjs prints, as an alist.  Times are in time
zone ZONE, by default the local one.  NO-RESETS gives \"5h 2% · week 3%\"."
  (when-let* ((parts (delq nil (list (agent-hq-statusline--window
                                      "5h" (alist-get 'five_hour data) nil zone no-resets)
                                     (agent-hq-statusline--window
                                      "week" (alist-get 'seven_day data) t zone no-resets)))))
    (string-join parts " · ")))

(defun agent-hq-statusline--usage-data (account)
  "Return ACCOUNT's last fetched plan usage, fetching it again when stale."
  (let ((entry (gethash account agent-hq-statusline--usage)))
    (when (and (not agent-hq-statusline-no-fetch)
               (or (null entry)
                   (> (- (float-time) (or (plist-get entry :time) 0))
                      agent-hq-statusline-usage-ttl)))
      (agent-hq-statusline--fetch-usage account))
    (plist-get entry :data)))

(defun agent-hq-statusline--usage-text (account)
  "Return ACCOUNT's plan usage for the header, fetching it when stale."
  (when-let* ((data (agent-hq-statusline--usage-data account)))
    (agent-hq-statusline-format-usage data)))

;;;; Usage on demand

(defun agent-hq-statusline--row (label text)
  "Return a usage row: LABEL right-aligned as in agent-shell's, then TEXT."
  (concat (propertize (format "%9s " (concat label ":"))
                      'face 'agent-shell-secondary
                      'font-lock-face 'agent-shell-secondary)
          text))

(defun agent-hq-statusline-plan-text (account)
  "Return ACCOUNT's plan and plan usage, fetching the usage when stale.
When there's no usage to show, says why."
  (let* ((text (agent-hq-statusline--usage-text account))
         (entry (gethash account agent-hq-statusline--usage))
         (data (plist-get entry :data))
         (plan (alist-get 'subscription_type data)))
    (cond
     (text (if (stringp plan) (concat plan " · " text) text))
     (data "no 5-hour or weekly limit reported")
     ((process-live-p (plist-get entry :process)) "fetching, try again in a moment")
     ((agent-hq-statusline--usage-command) "couldn't be fetched")
     (t "unavailable (node or the Agent SDK wasn't found)"))))

;;;###autoload
(defun agent-hq-show-usage ()
  "Show this shell's context, tokens and cost, and its plan usage.
agent-shell's `agent-shell-show-usage' errors until the agent reports
usage, which Claude only does while it answers, so a new or resumed shell
has none.  This shows the session's numbers once there are some and, for
Claude shells, the account's 5-hour and weekly usage from the header."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (let* ((usage (map-elt agent-shell--state :usage))
         (account (agent-hq-statusline--account)))
    (message "\n%s\n"
             (string-join
              (delq nil (list (if (agent-shell--usage-has-data-p usage)
                                  (agent-shell--format-usage usage t)
                                (agent-hq-statusline--row
                                 "Session" "nothing reported until the agent answers"))
                              (when account
                                (agent-hq-statusline--row
                                 "Plan" (agent-hq-statusline-plan-text account)))))
              "\n"))))

;;;; Git status of the shell's worktree

(defvar agent-hq-statusline-git-functions nil
  "Functions called with a worktree's root when its git status changed.
For views besides the headers that show it, such as the dashboard.")

(defvar agent-hq-statusline-usage-functions nil
  "Functions called with an account's usage key when its plan usage came in.")

(defvar agent-hq-statusline--git (make-hash-table :test #'equal)
  "Git status per worktree root.
Each value is a plist: :data, from `agent-hq-statusline-parse-status' (nil
when git failed); :time, when git was last asked; :process, the git
running now.")

(defun agent-hq-statusline-parse-status (output)
  "Return the counts in OUTPUT, from git status --porcelain=v2 --branch.
A plist: :ahead and :behind are commits against the upstream, :staged,
:modified, :untracked and :conflicts are files.  A file both staged and
changed again counts as both.  :gone is there, as t, when the branch's
upstream no longer exists, as after a merged PR's branch was deleted:
git then names the upstream but can't count against it."
  (let ((ahead 0) (behind 0) (staged 0) (modified 0) (untracked 0) (conflicts 0)
        upstream counted)
    (dolist (line (split-string output "\n" t))
      (cond
       ((string-prefix-p "# branch.upstream " line) (setq upstream t))
       ((string-match "\\`# branch\\.ab \\+\\([0-9]+\\) -\\([0-9]+\\)" line)
        (setq counted t
              ahead (string-to-number (match-string 1 line))
              behind (string-to-number (match-string 2 line))))
       ((string-match "\\`[12] \\(.\\)\\(.\\) " line)
        (unless (equal (match-string 1 line) ".") (setq staged (1+ staged)))
        (unless (equal (match-string 2 line) ".") (setq modified (1+ modified))))
       ((string-prefix-p "u " line) (setq conflicts (1+ conflicts)))
       ((string-prefix-p "? " line) (setq untracked (1+ untracked)))))
    (append (list :ahead ahead :behind behind :staged staged :modified modified
                  :untracked untracked :conflicts conflicts)
            (when (and upstream (not counted))
              (list :gone t)))))

(defun agent-hq-statusline--git-state (git-dir)
  "Return the operation in progress in GIT-DIR, as \"merging\", or nil.
A rebase isn't named here: `agent-hq--branch' already shows it."
  (when git-dir
    (seq-some (pcase-lambda (`(,file . ,state))
                (and (file-exists-p (expand-file-name file git-dir)) state))
              '(("MERGE_HEAD" . "merging") ("CHERRY_PICK_HEAD" . "cherry-picking")
                ("REVERT_HEAD" . "reverting") ("BISECT_LOG" . "bisecting")))))

(defun agent-hq-statusline-format-git (branch data &optional state)
  "Return BRANCH and DATA's counts as \"main ↑1 +2 ~1 ?3\", with faces.
DATA is from `agent-hq-statusline-parse-status', or nil before git answers
(or when it failed), which shows just BRANCH.  STATE names an operation
in progress."
  (let ((count (lambda (key prefix face)
                 (let ((n (plist-get data key)))
                   (when (and n (> n 0))
                     (propertize (format "%s%d" prefix n) 'face face))))))
    (string-join
     (delq nil (list (propertize branch 'face 'agent-hq-statusline-branch)
                     (when state (propertize state 'face 'agent-shell-error))
                     (funcall count :ahead "↑" 'agent-hq-statusline-branch)
                     (funcall count :behind "↓" 'agent-hq-statusline-branch)
                     (funcall count :conflicts "!" 'agent-shell-error)
                     (funcall count :staged "+" 'agent-shell-success)
                     (funcall count :modified "~" 'agent-shell-warning)
                     (funcall count :untracked "?" 'shadow)
                     (when (and data (cl-every (lambda (key) (zerop (plist-get data key)))
                                               '(:staged :modified :untracked :conflicts)))
                       (propertize "✓" 'face 'agent-shell-success))))
     " ")))

(defun agent-hq-statusline--git-fetch (root)
  "Start git status in ROOT unless it is already running there."
  (let ((entry (gethash root agent-hq-statusline--git)))
    (unless (process-live-p (plist-get entry :process))
      (setq entry (plist-put (copy-sequence entry) :time (float-time)))
      (let ((default-directory root)
            (buffer (generate-new-buffer " *agent-hq-statusline-status*")))
        (condition-case nil
            (setq entry
                  (plist-put entry :process
                             (make-process
                              :name "agent-hq-statusline-status"
                              :buffer buffer
                              ;; Never take index.lock, so this can't
                              ;; collide with the agent's own git.
                              :command '("git" "--no-optional-locks" "status"
                                         "--porcelain=v2" "--branch")
                              :connection-type 'pipe
                              :noquery t
                              :sentinel (lambda (process _)
                                          (agent-hq-statusline--git-done process root)))))
          (error (kill-buffer buffer))))
      (puthash root entry agent-hq-statusline--git))))

(defun agent-hq-statusline--git-done (process root)
  "Store what git status PROCESS said about ROOT; redraw ROOT's shells if new."
  (unless (process-live-p process)
    (let* ((buffer (process-buffer process))
           (data (and (buffer-live-p buffer)
                      (eql (process-exit-status process) 0)
                      (with-current-buffer buffer
                        (agent-hq-statusline-parse-status (buffer-string)))))
           (old (plist-get (gethash root agent-hq-statusline--git) :data)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (puthash root (list :data data :time (float-time) :process nil) agent-hq-statusline--git)
      (unless (equal data old)
        (agent-hq-statusline--redraw
         (lambda (shell)
           (equal (with-current-buffer shell
                    (when-let* ((ctx (agent-hq-context)))
                      (agent-hq-context-root ctx)))
                  root)))
        (run-hook-with-args 'agent-hq-statusline-git-functions root)))))

(defun agent-hq-statusline--git (ctx)
  "Return (BRANCH DATA STATE) for CTX's worktree, asking git when stale.
As `agent-hq-statusline-format-git' takes them; nil outside git."
  (when-let* ((agent-hq-statusline-show-git)
              (ctx)
              (branch (agent-hq-context-branch ctx)))
    (let* ((root (agent-hq-context-root ctx))
           (entry (gethash root agent-hq-statusline--git)))
      (when (and (not agent-hq-statusline-no-fetch)
                 (or (null entry)
                     (> (- (float-time) (or (plist-get entry :time) 0))
                        agent-hq-statusline-git-ttl)))
        (agent-hq-statusline--git-fetch root))
      (list branch (plist-get entry :data)
            (agent-hq-statusline--git-state (agent-hq--git-dir root))))))

(defun agent-hq-statusline--git-text (ctx)
  "Return the branch and git status of CTX's worktree, asking git when stale."
  (when-let* ((git (agent-hq-statusline--git ctx)))
    (apply #'agent-hq-statusline-format-git git)))

;;;###autoload
(defun agent-hq-statusline-magit-refresh-h ()
  "Ask git again for shells in the repo magit just refreshed.
For `magit-post-refresh-hook', so a commit or stage shows at once."
  (when-let* ((ctx (agent-hq-context))
              (root (agent-hq-context-root ctx))
              ((gethash root agent-hq-statusline--git)))
    (agent-hq-statusline--git-fetch root)))

;;;; Session time

(defvar-local agent-hq-statusline--opened nil
  "When this shell was opened, as a float time.")

(defun agent-hq-statusline-duration (seconds)
  "Return SECONDS as \"12m\" or \"1h 05m\"."
  (let ((minutes (floor seconds 60)))
    (if (< minutes 60)
        (format "%dm" minutes)
      (format "%dh %02dm" (/ minutes 60) (% minutes 60)))))

;;;; The header

(defun agent-hq-statusline--parts (ctx)
  "Return what this shell's header shows after the project, as a plist.
:git is (BRANCH DATA STATE) of CTX's worktree, or nil outside git;
:account the usage key of a Claude shell and :usage its plan usage;
:time the session time.  Asks git, and fetches usage, when stale."
  (let ((account (agent-hq-statusline--account)))
    (list :git (agent-hq-statusline--git ctx)
          :account account
          :usage (when account (agent-hq-statusline--usage-data account))
          :time (when agent-hq-statusline--opened
                  (agent-hq-statusline-duration (- (float-time) agent-hq-statusline--opened))))))

(defconst agent-hq-statusline--fits
  '(nil
    (:no-resets t)
    (:no-resets t :branch 24)
    (:no-resets t :branch 24 :no-time t)
    (:no-resets t :branch 12 :no-time t))
  "How the header's second line shrinks, tried in order until one fits.
:no-resets drops when plan usage resets, :branch cuts the branch to
that many columns and :no-time drops the session time.")

(defun agent-hq-statusline--status-text (parts &optional fit)
  "Return PARTS, from `agent-hq-statusline--parts', as header text, or nil.
FIT, one of `agent-hq-statusline--fits', says what to leave out."
  (pcase-let* ((`(,branch ,data ,state) (plist-get parts :git))
               (max (plist-get fit :branch))
               (label (when-let* ((label (agent-hq-statusline-account-label
                                          (plist-get parts :account))))
                        (propertize label 'face 'agent-hq-statusline-account)))
               (usage (when-let* ((usage (plist-get parts :usage)))
                        (agent-hq-statusline-format-usage usage nil (plist-get fit :no-resets)))))
    (when-let* ((items
                 (delq nil
                       (list (when branch
                               (agent-hq-statusline-format-git
                                (if max (truncate-string-to-width branch max nil nil "…") branch)
                                data state))
                             (if (and label usage) (concat label " " usage) (or label usage))
                             (unless (plist-get fit :no-time) (plist-get parts :time))))))
      (string-join items " ➤ "))))

(defun agent-hq-statusline--text-width (string)
  "Return about how many columns STRING takes on a header line.
The ➤ separators come from a fallback font and draw wider, so count twice."
  (+ (string-width string) (seq-count (lambda (char) (eq char ?➤)) string)))

(defun agent-hq-statusline--columns (model)
  "Return how many columns of text a header line of MODEL has room for.
Nil when MODEL doesn't say how wide it's drawn."
  (when-let* ((width (map-elt model :width))
              ((numberp width))
              (char (frame-char-width))
              ((> char 0)))
    (floor (- width
              (if (eq agent-shell-header-style 'graphical)
                  ;; agent-shell's icon: a square three lines tall at x 6,
                  ;; with the text 10px after it.
                  (+ 6 (* 3 (or (map-elt model :font-height) (frame-char-height))) 10)
                char)                   ; the text header's leading space
              (* 2 char))               ; slack, as agent-shell leaves
           char)))

(defun agent-hq-statusline--line-1-width (model name)
  "Return about how wide MODEL's first header line is, with model NAME."
  (agent-hq-statusline--text-width
   (concat (string-join (delq nil (list (map-elt model :buffer-name) name
                                        (map-elt model :thought-level-name)
                                        (map-elt model :mode-name)
                                        (map-elt model :context-indicator)
                                        (map-elt model :cost-indicator)))
                        " ➤ ")
           (when (and (eq agent-shell-header-style 'text)
                      (> (or (map-elt model :more-config-option-count) 0) 0))
             " (+N)"))))

(defun agent-hq-statusline--face-runs (string)
  "Return STRING as a list of (TEXT . FACE), one per run of the same face."
  (let ((pos 0) (end (length string)) runs)
    (while (< pos end)
      (let ((next (next-single-property-change pos 'face string end)))
        (push (cons (substring-no-properties string pos next)
                    (get-text-property pos 'face string))
              runs)
        (setq pos next)))
    (nreverse runs)))

(defun agent-hq-statusline--escape-percent (string)
  "Return STRING with each % doubled, its faces kept.
The text header is a mode-line format, where a lone % is a construct;
`string-replace' would leave the inserted % without a face."
  (mapconcat (pcase-lambda (`(,text . ,face))
               (let ((text (string-replace "%" "%%" text)))
                 (if face (propertize text 'face face) text)))
             (agent-hq-statusline--face-runs string)))

;;;###autoload
(defun agent-hq-statusline-model-a (fn state &rest args)
  "Around advice for `agent-shell--make-header-model' (FN, STATE and ARGS).
Shows the model Claude answered with, a linked worktree's project as
repo:worktree, then git status, account and plan usage and session time
after the project on the header's second line (the session id's place,
which agent-shell leaves empty unless `agent-shell-show-session-id').

agent-shell clips a header line the window is too narrow for, so the
second line sheds parts as `agent-hq-statusline--fits' says, and the first
drops the cost, until they fit."
  (let ((model (apply fn state args))
        (shell (map-elt state :buffer)))
    (if (not (buffer-live-p shell))
        model
      (with-current-buffer shell
        (let* ((ctx (agent-hq-context))
               (label (map-elt model :model-name))
               (name (if (and label (agent-hq-statusline--account))
                         (agent-hq-statusline--model state label)
                       label))
               (project (if (and ctx (agent-hq-context-worktree ctx))
                            (agent-hq-context-label ctx)
                          (map-elt model :project-name)))
               (columns (agent-hq-statusline--columns model))
               ;; agent-shell rebuilds a hidden shell's header too (on each
               ;; usage update, say): show it as last fetched.
               (parts (let ((agent-hq-statusline-no-fetch
                             (or agent-hq-statusline-no-fetch
                                 (not (get-buffer-window shell t)))))
                        (agent-hq-statusline--parts ctx)))
               (session-id (map-elt model :session-id))
               (bottom-for (lambda (fit)
                             (let ((status (agent-hq-statusline--status-text parts fit)))
                               (if (and session-id status)
                                   (concat session-id " ➤ " status)
                                 (or session-id status)))))
               (fits agent-hq-statusline--fits)
               (bottom (funcall bottom-for (car fits))))
          (while (and columns (cdr fits)
                      (> (agent-hq-statusline--text-width
                          (concat project " ➤ " bottom (map-elt model :busy-indicator-frame)))
                         columns))
            (setq fits (cdr fits)
                  bottom (funcall bottom-for (car fits))))
          (append (list (cons :model-name name)
                        (cons :project-name project)
                        ;; The SVG header is drawn as is.
                        (cons :session-id (if (and bottom (eq agent-shell-header-style 'text))
                                              (agent-hq-statusline--escape-percent bottom)
                                            bottom)))
                  (when (and columns (> (agent-hq-statusline--line-1-width model name) columns))
                    (list (cons :cost-indicator nil)))
                  model))))))

(defun agent-hq-statusline--xml-escape (text)
  "Return TEXT with &, < and > escaped for SVG."
  (replace-regexp-in-string
   "[&<>]" (lambda (char) (pcase char ("&" "&amp;") ("<" "&lt;") (_ "&gt;"))) text t t))

(defun agent-hq-statusline-color-runs (svg text)
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
                               (agent-hq-statusline--xml-escape run)))))
        (setcdr (cdr node)
                (mapcan (pcase-lambda (`(,run . ,face))
                          (if face
                              (list (funcall tspan face run))
                            (cdr (mapcan (lambda (part)
                                           (list (funcall tspan 'default "➤")
                                                 (agent-hq-statusline--xml-escape part)))
                                         (split-string run "➤")))))
                        (agent-hq-statusline--face-runs text))))
      t)))

;;;###autoload
(defun agent-hq-statusline-render-a (fn model)
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
                     (ignore-errors (agent-hq-statusline-color-runs svg text))
                     (apply svg-image svg props))))
          (funcall fn model))))))

(defvar-local agent-hq-statusline--stale nil
  "Non-nil when this shell's header missed a redraw while no window showed it.")

(defun agent-hq-statusline--redraw (predicate)
  "Redraw the header of each shell PREDICATE returns non-nil for.
Only shells a window shows: a header is an SVG image, made anew each
time.  The others are marked and redrawn once shown, by
`agent-hq-statusline--shown-h'."
  (dolist (shell (agent-shell-buffers))
    (when (funcall predicate shell)
      (with-current-buffer shell
        (if (not (get-buffer-window shell t))
            (setq agent-hq-statusline--stale t)
          (setq agent-hq-statusline--stale nil)
          (ignore-errors (agent-shell--update-header-and-mode-line)))))))

(defun agent-hq-statusline--shown-h (window)
  "Redraw the header of WINDOW's shell if it missed a redraw while hidden.
For a shell's `window-buffer-change-functions', which since Emacs 31 also
run for the buffer leaving a window: WINDOW's buffer is then another."
  (let ((shell (window-buffer window)))
    (when (buffer-local-value 'agent-hq-statusline--stale shell)
      (with-current-buffer shell
        (setq agent-hq-statusline--stale nil)
        (ignore-errors (agent-shell--update-header-and-mode-line))))))

;;;; The session title, in the mode line

(defun agent-hq-mode-line-title (&optional reserve)
  "Return this shell's session title for its mode line.
The title is cut to leave RESERVE columns (default 32) of the window for
the rest of the mode line: buffer size, line, column and position, and
on the right misc info; and room for agent-hq-attention's badge, when
the mode line has one.  Until the session has one, a dimmed \"New
session\".  The buffer name, whose agent, repo, worktree and branch the
header shows, is in the tooltip."
  (let* ((title (agent-hq--session-title (current-buffer)))
         (badge (if (fboundp 'agent-hq-attention-mode-line-width)
                    (agent-hq-attention-mode-line-width)
                  0))
         ;; When a mode line is drawn, its window is the selected one.
         (room (max 12 (- (window-total-width) (or reserve 32) badge)))
         (text (if title (truncate-string-to-width title room nil nil "…") "New session")))
    ;; A mode line reads % in an :eval's result as a construct.
    (propertize (string-replace "%" "%%" text)
                'face (cond ((not title) 'shadow)
                            ((or (not (fboundp 'mode-line-window-selected-p))
                                 (mode-line-window-selected-p))
                             'mode-line-buffer-id))
                'help-echo (if title (concat title "\n" (buffer-name)) (buffer-name)))))

(defun agent-hq-statusline--shell-to-rename ()
  "Return the shell `agent-hq-rename-session' renames when called interactively.
This shell, or outside one, a shell you pick, this worktree's first."
  (if (derived-mode-p 'agent-shell-mode)
      (current-buffer)
    (let ((here (agent-hq--shells-in (agent-hq--current-root))))
      (agent-hq--pick-shell "Rename the session of: "
                            (or (agent-hq--shells) (user-error "No agent shells running"))
                            (lambda (_ ctx) (if ctx (agent-hq-context-repo ctx) "other"))
                            (lambda (buffer _) (memq buffer here))))))

;;;###autoload
(defun agent-hq-rename-session (title &optional shell)
  "Set the session TITLE of SHELL, which its mode line and session pickers show.
SHELL defaults to the current buffer.  Interactively, outside a shell, you
pick one, this worktree's shells first.  The agent stores the title the
way Claude Code's /rename does, so a resumed session keeps it and Claude
doesn't replace it with one it generates."
  (interactive
   (let* ((shell (agent-hq-statusline--shell-to-rename))
          (title (or (agent-hq--session-title shell) "")))
     (list (read-string "Session title: " title nil title) shell)))
  (let ((shell (or shell (current-buffer))))
    (unless (and (buffer-live-p shell)
                 (with-current-buffer shell (derived-mode-p 'agent-shell-mode)))
      (user-error "Not an agent-shell buffer"))
    (with-current-buffer shell
      (let ((title (string-trim title))
            (session-id (map-nested-elt agent-shell--state '(:session :id))))
        (when (string-empty-p title)
          (user-error "A session title can't be empty"))
        (unless session-id
          (user-error "The session hasn't started yet"))
        (agent-shell--send-request
         :state agent-shell--state
         :client (map-elt agent-shell--state :client)
         :request `((:method . "session/set_title")
                    (:params . ((sessionId . ,session-id) (title . ,title))))
         :buffer shell
         :on-success (lambda (response)
                       ;; The agent may tidy the title: show what it stored.
                       (let ((stored (or (map-elt response 'title) title)))
                         (when (buffer-live-p shell)
                           (with-current-buffer shell
                             (agent-shell--set-session-title stored)
                             (force-mode-line-update)))
                         (message "Session title: %s" stored)))
         :on-failure (lambda (error &rest _)
                       (message "Couldn't rename the session: %s"
                                (or (map-elt error 'message) error))))))))

(defvar agent-hq-statusline--timer nil
  "Redraws visible shells every minute, for the session time.")

(defun agent-hq-statusline--tick ()
  "Redraw visible shells; stop once no shell is left."
  (if (agent-shell-buffers)
      (agent-hq-statusline--redraw (lambda (shell) (get-buffer-window shell t)))
    (cancel-timer agent-hq-statusline--timer)
    (setq agent-hq-statusline--timer nil)))

;;;###autoload
(defun agent-hq-statusline-setup-shell-h ()
  "Track this shell's open time and the model Claude answers with.
Also redraws its header when it's shown after missing a redraw.
For `agent-shell-mode-hook'."
  (setq agent-hq-statusline--opened (float-time))
  (add-hook 'window-buffer-change-functions #'agent-hq-statusline--shown-h nil t)
  (let ((buffer (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'usage-update
     :on-event (lambda (event) (agent-hq-statusline--note-model buffer event)))
    ;; The mode line shows the title: redraw it when the title changes.
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'session-title-changed
     :on-event (lambda (_)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (force-mode-line-update))))))
  (unless (timerp agent-hq-statusline--timer)
    (setq agent-hq-statusline--timer (run-at-time 60 60 #'agent-hq-statusline--tick))))

;;;###autoload
(defun agent-hq-statusline-setup ()
  "Show the real model, git status, plan usage and session time in headers."
  (advice-add 'agent-shell--make-header-model :around #'agent-hq-statusline-model-a)
  (advice-add 'agent-shell--render-header-model-uncached :around #'agent-hq-statusline-render-a)
  (add-hook 'agent-shell-mode-hook #'agent-hq-statusline-setup-shell-h)
  (add-hook 'magit-post-refresh-hook #'agent-hq-statusline-magit-refresh-h))

(provide 'agent-hq-statusline)
;;; agent-hq-statusline.el ends here
