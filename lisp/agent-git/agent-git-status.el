;;; agent-git-status.el --- /status in Claude shells -*- lexical-binding: t; -*-

;;; Commentary:

;; Claude Code's /status only exists in its terminal UI; over ACP it
;; answers "/status isn't available in this environment".  With
;; `agent-git-status-command-a' around `agent-shell--handle', a /status
;; typed in a Claude shell isn't sent: the shell shows what the terminal's
;; Status tab does, for this shell:
;;
;;   Version       Claude Code 2.1.330 · claude-agent-acp 0.110.0
;;   Session ID    8fdfa331-edf5-49c9-98d5-1b175ea5c5dc
;;   cwd           ~/projects/doom.d/
;;   Login method  Claude Max account
;;   Organization  …
;;   Email         …
;;   Config dir    ~/.claude
;;   Model         Opus 5.5 · Xhigh effort
;;   Mode          Auto
;;   Context       88k/1m (8.8%)
;;   Plan usage    max · week 24% (Mon 5am)
;;   Base URL      http://127.0.0.1:8787
;;   Memory        ~/.claude/CLAUDE.md · ~/projects/doom.d/CLAUDE.md
;;   Auto memory   ~/.claude/projects/-Users-light-projects-doom-d/memory
;;   MCP servers   /mcp lists them
;;
;; The version and login come from the Claude CLI the adapter runs
;; (`--version' and `auth status --json', with the shell's
;; CLAUDE_CONFIG_DIR), as the adapter itself does; neither prints
;; credentials.  The rest is already in Emacs.  Codex shells, and /status
;; before the session is up, go to the agent as usual.

;;; Code:

(require 'agent-git)
(require 'agent-git-header)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--finish-output "agent-shell-prompt")
(declare-function agent-shell--config-options "agent-shell-config")
(declare-function agent-shell--current-model-id "agent-shell-config")
(declare-function agent-shell--format-number-compact "agent-shell-usage")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function shell-maker--current-request-id "shell-maker")
(defvar agent-shell--state)
(defvar shell-maker--config)

(defcustom agent-git-status-timeout 5
  "Seconds the Claude CLI may take to say its version or login."
  :type 'number
  :group 'agent-git)

(defun agent-git-status--command-p (command)
  "Return non-nil if COMMAND, a prompt, is just /status."
  (and (stringp command)
       (string-match-p "\\`[ \t\n]*/status[ \t\n]*\\'" command)))

;;;; From the Claude CLI

(defun agent-git-status--env (env name)
  "Return NAME's value in ENV, a list of \"NAME=VALUE\" strings, or nil."
  (seq-some (lambda (entry)
              (when (string-prefix-p (concat name "=") entry)
                (substring entry (1+ (length name)))))
            env))

(defun agent-git-status--cli (env)
  "Return the Claude CLI the adapter runs with ENV, or nil.
Like the adapter: CLAUDE_CODE_EXECUTABLE, else the Agent SDK's own binary."
  (or (agent-git-status--env env "CLAUDE_CODE_EXECUTABLE")
      (getenv "CLAUDE_CODE_EXECUTABLE")
      (when-let* ((root (agent-git-header-acp-root)))
        (seq-find #'file-executable-p
                  (file-expand-wildcards
                   (expand-file-name "node_modules/@anthropic-ai/claude-agent-sdk-*/claude"
                                     root))))
      (executable-find "claude")))

(defun agent-git-status--output (command env &optional any-exit)
  "Return what COMMAND prints with ENV added, or nil.
Nil when it runs past `agent-git-status-timeout', or exits non-zero
unless ANY-EXIT.  Its stderr is dropped."
  (let* ((default-directory (expand-file-name "~/"))
         (process-environment (append env process-environment))
         (out (generate-new-buffer " *agent-git-status*"))
         (err (generate-new-buffer " *agent-git-status-stderr*"))
         (done nil)
         (process (make-process :name "agent-git-status"
                                :buffer out
                                :stderr err
                                :command command
                                :connection-type 'pipe
                                :noquery t
                                :sentinel (lambda (_ _) (setq done t))))
         (deadline (+ (float-time) agent-git-status-timeout)))
    (unwind-protect
        (progn
          (while (and (not done) (< (float-time) deadline))
            (accept-process-output process 0.05))
          (when (and done (or any-exit (eql (process-exit-status process) 0)))
            (with-current-buffer out (buffer-string))))
      (delete-process process)
      (when-let* ((stderr (get-buffer-process err)))
        (delete-process stderr))
      (kill-buffer out)
      (kill-buffer err))))

(defun agent-git-status--version (cli)
  "Return CLI's version, as \"2.1.330\", or nil."
  (when-let* ((cli)
              (out (agent-git-status--output (list cli "--version") nil))
              ((string-match "[0-9]+\\(?:\\.[0-9]+\\)+" out)))
    (match-string 0 out)))

(defun agent-git-status--acp-version ()
  "Return claude-agent-acp's version, or nil."
  (when-let* ((root (agent-git-header-acp-root))
              (json (ignore-errors
                      (agent-git--read-json (expand-file-name "package.json" root))))
              (version (alist-get 'version json))
              ((stringp version)))
    version))

(defun agent-git-status--auth (cli env)
  "Return `CLI auth status --json' run with ENV, as an alist, or nil.
A logged-out CLI exits 1 but still prints its status."
  (when-let* ((cli)
              (out (agent-git-status--output (list cli "auth" "status" "--json") env t)))
    (ignore-errors
      (json-parse-string out :object-type 'alist :null-object nil :false-object :false))))

(defun agent-git-status--login-method (auth)
  "Return the login method AUTH, from `agent-git-status--auth', names."
  (let ((method (alist-get 'authMethod auth))
        (plan (alist-get 'subscriptionType auth)))
    (cond
     ((not (eq (alist-get 'loggedIn auth) t)) "not logged in")
     ((equal method "claude.ai")
      (if (stringp plan) (format "Claude %s account" (capitalize plan)) "Claude account"))
     ((stringp method) method)
     (t "logged in"))))

;;;; From the shell

(defun agent-git-status--choice (state id)
  "Return the name of the current choice of STATE's config option ID, or nil."
  (when-let* ((option (seq-find (lambda (option) (equal (map-elt option :id) id))
                                (agent-shell--config-options state)))
              (choice (seq-find (lambda (choice)
                                  (equal (map-elt choice :value)
                                         (map-elt option :current-value)))
                                (map-elt option :options))))
    (map-elt choice :name)))

(defun agent-git-status--model (state)
  "Return the model STATE runs, with its effort and fast mode."
  (let ((model (agent-git-header--model state (agent-git-status--choice state "model")))
        (effort (agent-git-status--choice state "effort"))
        (fast (agent-git-status--choice state "fast")))
    (string-join (delq nil (list model
                                 (when (and effort (not (equal effort "Default")))
                                   (concat effort " effort"))
                                 (when (equal fast "On") "fast mode")))
                 " · ")))

(defun agent-git-status--context (usage)
  "Return the context USAGE says is used, as \"88k/1m (8.8%)\"."
  (let ((used (or (map-elt usage :context-used) 0))
        (size (or (map-elt usage :context-size) 0)))
    (if (> size 0)
        (format "%s/%s (%.1f%%)"
                (agent-shell--format-number-compact used)
                (agent-shell--format-number-compact size)
                (/ (* 100.0 used) size))
      "nothing reported until Claude answers")))

(defun agent-git-status--config-dir ()
  "Return this shell's Claude config dir."
  (if (memq agent-git--claude-started-with '(nil default))
      (expand-file-name "~/.claude")
    (directory-file-name agent-git--claude-started-with)))

(defun agent-git-status--memory-files (config cwd)
  "Return the CLAUDE.md files Claude Code reads for CONFIG and CWD.
CONFIG's own, then those of CWD and each directory above it."
  (let ((files (list (expand-file-name "CLAUDE.md" config)))
        (dir (file-name-as-directory (expand-file-name cwd))))
    (while dir
      (dolist (name '("CLAUDE.md" "CLAUDE.local.md" ".claude/CLAUDE.md"))
        (push (expand-file-name name dir) files))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (equal parent dir) parent))))
    (seq-filter #'file-exists-p (delete-dups (nreverse files)))))

(defun agent-git-status--auto-memory (config cwd)
  "Return the auto memory dir that CONFIG's Claude uses in CWD, as settings say.
The repo's main worktree's .claude/settings.local.json (see
`agent-git-claude-share-memory'), then CONFIG's settings.json, then the
default under CONFIG's projects/."
  (let ((root (agent-git--repo-root cwd)))
    (or (seq-some (lambda (file)
                    (let ((dir (alist-get 'autoMemoryDirectory
                                          (and (file-readable-p file)
                                               (ignore-errors (agent-git--read-json file))))))
                      (and (stringp dir) dir)))
                  (list (expand-file-name ".claude/settings.local.json" root)
                        (expand-file-name "settings.json" config)))
        (when-let* ((key (agent-git--claude-project-key root)))
          (expand-file-name (concat "projects/" key "/memory") config)))))

;;;; The status

(defun agent-git-status--row (label value)
  "Return a status row: LABEL in a column, then VALUE."
  (concat (propertize (format "%-13s" label)
                      'face 'agent-shell-secondary
                      'font-lock-face 'agent-shell-secondary)
          value))

(defun agent-git-status--code (text)
  "Return TEXT as inline code, so the shell's markdown leaves it as is."
  (when text
    (concat "`" text "`")))

(defun agent-git-status-text ()
  "Return the status of the Claude shell in the current buffer."
  (let* ((state agent-shell--state)
         (env (map-nested-elt state '(:client :environment-variables)))
         (cli (agent-git-status--cli env))
         (auth (agent-git-status--auth cli env))
         (config (agent-git-status--config-dir))
         (cwd (agent-shell-cwd))
         (account (agent-git-header--account))
         (version (agent-git-status--version cli))
         (acp (agent-git-status--acp-version))
         (rows
          `(("Version" . ,(string-join
                           (delq nil (list (when version (concat "Claude Code " version))
                                           (when acp (concat "claude-agent-acp " acp))))
                           " · "))
            ("Session ID" . ,(agent-git-status--code
                              (map-nested-elt state '(:session :id))))
            ("cwd" . ,(agent-git-status--code (abbreviate-file-name cwd)))
            ("Login method" . ,(if auth
                                   (agent-git-status--login-method auth)
                                 "couldn't be read from the Claude CLI"))
            ("Organization" . ,(alist-get 'orgName auth))
            ("Email" . ,(alist-get 'email auth))
            ("Config dir" . ,(agent-git-status--code (abbreviate-file-name config)))
            ("Model" . ,(agent-git-status--model state))
            ("Mode" . ,(agent-git-status--choice state "mode"))
            ("Context" . ,(agent-git-status--context (map-elt state :usage)))
            ("Plan usage" . ,(when account (agent-git-header-plan-text account)))
            ("Base URL" . ,(agent-git-status--code
                            (agent-git-status--env env "ANTHROPIC_BASE_URL")))
            ("Memory" . ,(when-let* ((files (agent-git-status--memory-files config cwd)))
                           (mapconcat (lambda (file)
                                        (agent-git-status--code (abbreviate-file-name file)))
                                      files " · ")))
            ("Auto memory" . ,(agent-git-status--code
                               (when-let* ((dir (agent-git-status--auto-memory config cwd)))
                                 (abbreviate-file-name dir))))
            ("MCP servers" . "/mcp lists them"))))
    (mapconcat (lambda (row) (agent-git-status--row (car row) (cdr row)))
               (seq-filter (lambda (row) (and (stringp (cdr row))
                                              (not (string-empty-p (cdr row)))))
                           rows)
               "\n")))

;;;###autoload
(defun agent-git-status-command-a (fn &rest args)
  "Around advice for `agent-shell--handle' (FN, ARGS): answer /status here.
Claude Code's /status only exists in its terminal UI.  So a prompt that
is just /status, in a Claude shell whose session is up, isn't sent: the
shell shows `agent-git-status-text' and ends the turn.  Anything else
goes to FN as usual."
  (let ((buffer (plist-get args :shell-buffer)))
    (if (and (agent-git-status--command-p (plist-get args :command))
             (buffer-live-p buffer)
             (with-current-buffer buffer
               (and (agent-git-header--account)
                    (map-nested-elt agent-shell--state '(:session :id)))))
        (with-current-buffer buffer
          (map-put! agent-shell--state :request-count (shell-maker--current-request-id))
          (unwind-protect
              (agent-shell--update-fragment
               :state agent-shell--state
               :block-id "agent-git-status"
               :label-left (propertize "Status" 'font-lock-face 'agent-shell-section-heading)
               :body (condition-case err
                         (agent-git-status-text)
                       (error (format "Couldn't read the status: %s"
                                      (error-message-string err))))
               :create-new t
               :expanded t)
            (agent-shell--finish-output :config shell-maker--config :success t)))
      (apply fn args))))

(provide 'agent-git-status)
;;; agent-git-status.el ends here
