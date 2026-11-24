;;; agent-git.el --- Repo, worktree and branch context for agent-shell -*- lexical-binding: t; -*-

;;; Commentary:

;; Names every agent-shell buffer after the repo, worktree and branch it works
;; in, for example "Claude @ webapp:worktree1 ⎇ bug/KZN-19576-max-length",
;; and renames it when the agent switches branches.  The name is what the mode
;; line shows and what every buffer switcher matches, so the branch is both
;; visible and searchable.
;;
;; Claude shells can use a different account per repo, through
;; `agent-git-claude-config-dirs' and `agent-git-claude-environment-a'; the
;; name then shows it, as in "Claude (work) @ webapp ⎇ develop".
;;
;; Files git ignores, such as a private .claude/CLAUDE.md, live only in the
;; main worktree; `agent-git-worktree-shared-files' links them into the
;; others as shells start there.
;;
;; Commands:
;;   `agent-git-switch-shell'        every live shell, all projects
;;   `agent-git-switch-worktree'     every worktree of every known repo
;;   `agent-git-new-worktree'        new branch + sibling worktree + shell
;;   `agent-git-remove-worktree'     close a worktree's shells, delete it and,
;;                                   if nothing would be lost, its branch
;;   `agent-git-clean-worktrees'     offer each finished worktree for removal
;;   `agent-git-toggle'              this worktree's shell, never another's
;;   `agent-git-kill-shell'          kill this shell, asking only if it's working
;;   `agent-git-resume-session'      pick a past session for this worktree
;;   `agent-git-resume-in-shell'     same, in this shell (what /resume runs)
;;   `agent-git-new-claude-in-project', `agent-git-new-codex-in-project'
;;                                   any known project: its workspace + a shell
;;   `agent-git-search-transcripts'  ripgrep over every worktree's transcripts
;;
;; Git state is read from the files under .git (HEAD, commondir,
;; worktrees/*/gitdir) instead of by running git, so the pickers can scan
;; every known project on each call.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

;; agent-shell is not required here: its Doom config requires this file, and
;; everything below that needs it runs only once shells exist.

(declare-function agent-shell "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-initialized-p "agent-shell")
(declare-function agent-shell-last-activity-time "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-restart "agent-shell")
(declare-function agent-shell-anthropic-start-claude-code "agent-shell-anthropic")
(declare-function agent-shell-openai-start-codex "agent-shell-openai")
(declare-function agent-shell--list-sessions "agent-shell")
(declare-function agent-shell--sort-sessions-by-recency "agent-shell")
(declare-function agent-shell--session-choice-label "agent-shell")
(declare-function agent-shell--session-selection-columns "agent-shell")
(declare-function agent-shell--session-column-value "agent-shell")
(declare-function agent-shell--resolve-path "agent-shell")
(declare-function agent-shell--finish-output "agent-shell-prompt")
(declare-function shell-maker-set-buffer-name "shell-maker")
(declare-function consult-ripgrep "consult")
(declare-function json-pretty-print-buffer "json")
(declare-function projectile-load-known-projects "projectile")
(declare-function persp--buffer-in-persps "persp-mode")
(declare-function get-current-persp "persp-mode")
(declare-function safe-persp-name "persp-mode")
(declare-function persp-switch "persp-mode")
(declare-function +workspace-switch "workspaces")
(defvar agent-shell--state)
(defvar agent-shell-display-action)
(defvar agent-shell-session-strategy)
(defvar agent-shell-preferred-agent-config)
(defvar agent-shell-anthropic-claude-environment)
(defvar shell-maker--config)
(defvar consult-ripgrep-args)
(defvar projectile-known-projects)
(defvar server-name)

(defgroup agent-git nil
  "Repo, worktree and branch context for agent-shell."
  :group 'tools
  :prefix "agent-git-")

(defcustom agent-git-branch-separator " ⎇ "
  "Text between the worktree label and the branch in shell names."
  :type 'string)

(defcustom agent-git-branch-types '("feature" "bug" "task" "fix" "chore")
  "Branch type prefixes offered by `agent-git-new-worktree'.
An empty answer creates the branch without a type prefix."
  :type '(repeat string))

(defcustom agent-git-branch-prefix-alist nil
  "Branch name prefixes per repo, as (DIR-REGEXP . PREFIX).
The first entry whose regexp matches the main worktree's root puts PREFIX
before the name typed in `agent-git-new-worktree', for example
\(\"/kizen/\" . \"KZN-\") turns \"123-fix\" into \"bug/KZN-123-fix\"."
  :type '(alist :key-type regexp :value-type string))

(defcustom agent-git-switch-project-function nil
  "Function called with a worktree root to make it the current project.
Doom points this at a function that switches to (or creates) the
project's workspace.  When nil, nothing is switched."
  :type '(choice (const :tag "Don't switch" nil) function))

(defcustom agent-git-worktree-created-hook nil
  "Hook run with the new worktree's root after `agent-git-new-worktree'.
Use it to copy untracked files such as .env into the worktree."
  :type 'hook)

(defcustom agent-git-worktree-shared-files nil
  "Files kept out of git that every worktree of a repo should have, per repo.
An alist of (DIR-REGEXP . FILES).  The first entry whose regexp matches
the main worktree's root names FILES, relative to the worktree root.  A
linked worktree that lacks one gets a symlink to the main worktree's
copy, where git ignores that path, when a shell starts in it or
`agent-git-new-worktree' creates it.  For example
\(\"/kizen/webapp/\\\\='\" \".claude/CLAUDE.md\" \"AGENTS.md\")."
  :type '(alist :key-type regexp :value-type (repeat string)))

(defcustom agent-git-base-branch-alist nil
  "The branch work merges into, per repo, as (DIR-REGEXP . REF).
The first entry whose regexp matches the main worktree's root gives REF,
such as \"origin/develop\"; other repos use origin/HEAD.  Removing a
worktree deletes its branch only when every commit on it is in REF or on
its upstream, and `agent-git-clean-worktrees' offers worktrees with
nothing beyond REF."
  :type '(alist :key-type regexp :value-type string))

(defcustom agent-git-disposable-ignored
  '("__pycache__" "*.pyc" "node_modules" ".DS_Store" ".ruff_cache" ".pytest_cache"
    ".mypy_cache" ".venv" ".next" ".turbo" ".agent-shell" ".claude" ".omc"
    ".serena" ".tokensave")
  "Ignored files that removing a worktree deletes without mentioning them.
Wildcards, matched against each part of an ignored path.  Before removing
a worktree, `agent-git-remove-worktree' lists any other ignored file it
would delete, such as notes or local settings."
  :type '(repeat string))

(defcustom agent-git-remove-worktree-functions nil
  "Functions called with a worktree's root just before it is removed.
They run once its shells are closed, while the directory still exists.
Doom uses this to close the worktree's workspace."
  :type 'hook)

(defcustom agent-git-claude-config-dirs nil
  "Claude Code config dirs per repo, as (DIR-REGEXP . CONFIG-DIR).
The first entry whose regexp matches the main worktree's root runs
Claude shells in every worktree of that repo with CLAUDE_CONFIG_DIR set
to CONFIG-DIR, and so with the account logged in there.  Outside git the
shell's own directory is matched.  Shells nothing matches leave
CLAUDE_CONFIG_DIR unset and use ~/.claude.

For example, (\"/kizen/webapp/\\\\='\" . \"~/.claude-work\").  Log in to a
new CONFIG-DIR once with CLAUDE_CONFIG_DIR=CONFIG-DIR claude, then /login.

Takes effect through `agent-git-claude-environment-a'."
  :type '(alist :key-type regexp :value-type directory))

(defcustom agent-git-claude-shared-memory t
  "Whether repos on another Claude account keep using ~/.claude's memory.
When non-nil, starting a Claude shell in a repo that
`agent-git-claude-config-dirs' maps sets `autoMemoryDirectory' in the
repo's .claude/settings.local.json to its memory dir under ~/.claude, so
both accounts read and write that repo's memory, in every worktree and
in tmux too.  The other account sees no other project's memory.  See
`agent-git-claude-share-memory'."
  :type 'boolean)

;;;; Reading git state from disk

(cl-defstruct (agent-git-context (:constructor agent-git--make-context)
                                 (:copier nil))
  "Where a directory sits in git: its repo, worktree and branch."
  repo worktree branch root common-dir)

(defun agent-git--read-first-line (file)
  "Return FILE's first line, trimmed, or nil if FILE can't be read."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (string-trim (buffer-substring-no-properties
                    (point-min) (progn (goto-char (point-min))
                                       (line-end-position)))))))

(defun agent-git--git-dir (root)
  "Return the git dir of the worktree at ROOT, or nil.
For a linked worktree, .git is a file naming the real git dir."
  (let ((dotgit (expand-file-name ".git" root)))
    (cond ((file-directory-p dotgit) (file-name-as-directory dotgit))
          ((file-regular-p dotgit)
           (when-let* ((line (agent-git--read-first-line dotgit))
                       ((string-match "\\`gitdir: *\\(.+\\)\\'" line)))
             (file-name-as-directory
              (expand-file-name (match-string 1 line) root)))))))

(defun agent-git--common-dir (git-dir)
  "Return the directory GIT-DIR shares with its repo's other worktrees."
  (if-let* ((rel (agent-git--read-first-line (expand-file-name "commondir" git-dir)))
            ((not (string-empty-p rel))))
      (file-name-as-directory (expand-file-name rel git-dir))
    git-dir))

(defun agent-git--main-root (common-dir)
  "Return the main worktree root for COMMON-DIR, or nil for a bare repo."
  (let ((dir (directory-file-name common-dir)))
    (when (equal (file-name-nondirectory dir) ".git")
      (file-name-directory dir))))

(defun agent-git--repo-name (common-dir)
  "Return the repo name for COMMON-DIR: the main worktree's directory name."
  (if-let* ((main (agent-git--main-root common-dir)))
      (file-name-nondirectory (directory-file-name main))
    (string-remove-suffix ".git" (file-name-nondirectory
                                  (directory-file-name common-dir)))))

(defun agent-git--branch (git-dir)
  "Return the branch checked out according to GIT-DIR's HEAD.
A rebase in progress shows the branch being rebased; any other
detached HEAD shows as \"detached@\" and the short hash."
  (when-let* ((head (agent-git--read-first-line (expand-file-name "HEAD" git-dir))))
    (if (string-match "\\`ref: refs/heads/\\(.+\\)\\'" head)
        (match-string 1 head)
      (if-let* ((name (seq-some (lambda (file)
                                  (agent-git--read-first-line
                                   (expand-file-name file git-dir)))
                                '("rebase-merge/head-name" "rebase-apply/head-name")))
                ((string-match "\\`refs/heads/\\(.+\\)\\'" name)))
          (format "%s (rebasing)" (match-string 1 name))
        (format "detached@%s" (substring head 0 (min 8 (length head))))))))

(defun agent-git--worktree-label (repo root main-root)
  "Return a short name for the worktree at ROOT of REPO, or nil for MAIN-ROOT.
The repo name is dropped from the front, so webapp_worktree1 becomes
\"worktree1\" and gymmers-app_w1 becomes \"w1\".  A directory named just
like the repo, as in Codex's ~/.codex/worktrees/08f5/triply, is named
after its parent instead (\"08f5\")."
  (unless (and main-root (string= (agent-git--truename root)
                                  (agent-git--truename main-root)))
    (let* ((dir (directory-file-name root))
           (base (file-name-nondirectory dir)))
      (cond ((string-match (concat "\\`" (regexp-quote repo) "[-_.]+\\(.+\\)\\'") base)
             (match-string 1 base))
            ((string= base repo)
             (file-name-nondirectory (directory-file-name (file-name-directory dir))))
            (t base)))))

(defun agent-git--truename (dir)
  "Return DIR as an absolute, symlink-free directory name."
  (file-name-as-directory (file-truename (expand-file-name dir))))

(defun agent-git-context (&optional dir)
  "Return the git context of DIR (default `default-directory').
Returns nil outside a git worktree and for remote directories."
  (let ((dir (expand-file-name (or dir default-directory))))
    (unless (file-remote-p dir)
      (when-let* ((found (locate-dominating-file dir ".git"))
                  (root (file-name-as-directory (expand-file-name found)))
                  (git-dir (agent-git--git-dir root)))
        (let* ((common (agent-git--common-dir git-dir))
               (repo (agent-git--repo-name common)))
          (agent-git--make-context
           :repo repo
           :worktree (agent-git--worktree-label repo root (agent-git--main-root common))
           :branch (agent-git--branch git-dir)
           :root root
           :common-dir common))))))

(defun agent-git-context-label (ctx)
  "Return \"repo:worktree\" for CTX, or just \"repo\" for the main worktree."
  (if-let* ((worktree (agent-git-context-worktree ctx)))
      (format "%s:%s" (agent-git-context-repo ctx) worktree)
    (agent-git-context-repo ctx)))

(defun agent-git--repo-worktrees (common-dir)
  "Return a context for each worktree of the repo at COMMON-DIR, main first.
Worktrees whose directory is gone are left out."
  (let* ((repo (agent-git--repo-name common-dir))
         (main (agent-git--main-root common-dir))
         (admin-root (expand-file-name "worktrees" common-dir))
         (linked
          (when (file-directory-p admin-root)
            (delq nil
                  (mapcar
                   (lambda (name)
                     (let* ((admin (file-name-as-directory (expand-file-name name admin-root)))
                            (gitfile (agent-git--read-first-line
                                      (expand-file-name "gitdir" admin)))
                            (root (and gitfile
                                       (file-name-directory (expand-file-name gitfile admin)))))
                       (when (and root (file-directory-p root))
                         (agent-git--make-context
                          :repo repo
                          :worktree (agent-git--worktree-label repo root main)
                          :branch (agent-git--branch admin)
                          :root root
                          :common-dir common-dir))))
                   (directory-files admin-root nil "\\`[^.]"))))))
    (append (when (and main (file-directory-p main))
              (list (agent-git--make-context
                     :repo repo :worktree nil
                     :branch (agent-git--branch common-dir)
                     :root main :common-dir common-dir)))
            (sort linked (lambda (a b) (string< (agent-git-context-worktree a)
                                                (agent-git-context-worktree b)))))))

;;;; Claude accounts

(defun agent-git--repo-root (dir)
  "Return the main worktree root of DIR's repo, or DIR itself outside git.
A bare repo's worktrees fall back to their own root."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (if-let* ((ctx (agent-git-context dir)))
        (or (agent-git--main-root (agent-git-context-common-dir ctx))
            (agent-git-context-root ctx))
      dir)))

(defun agent-git-claude-config-dir (&optional dir)
  "Return the CLAUDE_CONFIG_DIR for Claude shells in DIR, or nil for ~/.claude.
DIR defaults to `default-directory'.  See `agent-git-claude-config-dirs'."
  (when agent-git-claude-config-dirs
    (let ((root (agent-git--repo-root (or dir default-directory))))
      (when-let* ((entry (seq-find (lambda (entry) (string-match-p (car entry) root))
                                   agent-git-claude-config-dirs)))
        (file-name-as-directory (expand-file-name (cdr entry)))))))

(defun agent-git--account-name (config-dir)
  "Return a short name for CONFIG-DIR: ~/.claude-work gives \"work\"."
  (let ((base (file-name-nondirectory (directory-file-name config-dir))))
    (if (string-match "\\`\\.?claude[-_.]+\\(.+\\)\\'" base)
        (match-string 1 base)
      base)))

(defun agent-git-claude-account (&optional dir)
  "Return a short name for the Claude account used in DIR, or nil for ~/.claude.
The name comes from the config dir: ~/.claude-work gives \"work\"."
  (when-let* ((config (agent-git-claude-config-dir dir)))
    (agent-git--account-name config)))

(defun agent-git--claude-project-key (dir)
  "Return the name Claude Code gives DIR's folder under projects/.
Every character but ASCII letters and digits becomes \"-\".  Returns nil
for paths so long that Claude Code shortens them with a hash instead."
  (let ((key (replace-regexp-in-string
              "[^a-zA-Z0-9]" "-"
              (directory-file-name (file-truename (expand-file-name dir))))))
    (when (<= (length key) 200)
      key)))

(defun agent-git-claude-shared-memory-dir (&optional dir)
  "Return the auto memory dir that ~/.claude keeps for DIR's repo, or nil.
Claude Code keeps one memory per repo, under its main worktree's root."
  (when-let* ((key (agent-git--claude-project-key
                    (agent-git--repo-root (or dir default-directory)))))
    (expand-file-name (concat "projects/" key "/memory")
                      (or (getenv "CLAUDE_CONFIG_DIR") "~/.claude"))))

(defun agent-git--read-json (file)
  "Return FILE's JSON as an alist; an empty FILE gives nil."
  (with-temp-buffer
    (insert-file-contents file)
    (unless (string-blank-p (buffer-string))
      (json-parse-buffer :object-type 'alist :null-object :null :false-object :false))))

(defun agent-git--write-json (file data)
  "Write DATA, an alist from `agent-git--read-json', to FILE as indented JSON."
  (require 'json)
  (with-temp-buffer
    (insert (json-serialize data :null-object :null :false-object :false))
    (json-pretty-print-buffer)
    (goto-char (point-max))
    (insert "\n")
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region nil nil file nil 'silent))))

(defun agent-git--git-ignored-p (root file)
  "Return non-nil if git ignores FILE, relative to the worktree at ROOT."
  (let ((default-directory root))
    (eql 0 (ignore-errors (process-file "git" nil nil nil "check-ignore" "-q" file)))))

(defun agent-git-claude-share-memory (&optional dir)
  "Make every Claude account use DIR's repo memory under ~/.claude.
Sets `autoMemoryDirectory' in .claude/settings.local.json of the repo's
main worktree, keeping the rest of that file.  Claude Code applies that
file's setting in every linked worktree too, even ones with their own
local settings, and ~/.claude itself already keeps the repo's memory in
that dir, so only accounts with another CLAUDE_CONFIG_DIR change: they
read and write this repo's memory, and no other project's.  A missing
file is created only where git ignores it.  Returns the memory dir once
the setting is in place, nil when the file was left alone (outside git,
unreadable JSON, not ignored)."
  (let* ((root (and (agent-git-context dir)
                    (agent-git--repo-root (or dir default-directory))))
         (memory (and root (agent-git-claude-shared-memory-dir root)))
         (rel ".claude/settings.local.json")
         (file (and root (expand-file-name rel root))))
    (when (and memory
               (or (file-exists-p file) (agent-git--git-ignored-p root rel)))
      (condition-case err
          (let ((settings (and (file-exists-p file) (agent-git--read-json file))))
            (unless (equal (alist-get 'autoMemoryDirectory settings) memory)
              (make-directory (file-name-directory file) t)
              (agent-git--write-json
               file (if (assq 'autoMemoryDirectory settings)
                        (cons (cons 'autoMemoryDirectory memory)
                              (assq-delete-all 'autoMemoryDirectory settings))
                      (append settings (list (cons 'autoMemoryDirectory memory))))))
            memory)
        (error (message "agent-git: left %s alone: %s"
                        (abbreviate-file-name file) (error-message-string err))
               nil)))))

(defvar-local agent-git--claude-started-with nil
  "The CLAUDE_CONFIG_DIR this shell's Claude client was made with.
`default' when it was left unset; nil before any client was made.")

(defun agent-git--claude-account-here ()
  "Return the account name of the Claude shell in this buffer, or nil.
Once the shell has a client this is the account it was started with, so
editing `agent-git-claude-config-dirs' doesn't relabel running shells."
  (pcase agent-git--claude-started-with
    ('default nil)
    ('nil (agent-git-claude-account))
    (config (agent-git--account-name config))))

;;;###autoload
(defun agent-git-claude-environment-a (fn &rest args)
  "Run FN, a Claude client maker, with the account its shell's repo uses.
Around advice for `agent-shell-anthropic-make-claude-client': adds
CLAUDE_CONFIG_DIR for repos in `agent-git-claude-config-dirs' and leaves
it out elsewhere.  ARGS holds :buffer, the shell the client is for.
With `agent-git-claude-shared-memory', such repos also keep ~/.claude's
project memory; see `agent-git-claude-share-memory'."
  (let* ((buffer (or (plist-get args :buffer) (current-buffer)))
         (config (with-current-buffer buffer
                   (setq agent-git--claude-started-with
                         (or (agent-git-claude-config-dir) 'default)))))
    (when (and agent-git-claude-shared-memory (not (eq config 'default)))
      (with-current-buffer buffer
        (agent-git-claude-share-memory)))
    (if (eq config 'default)
        (apply fn args)
      (let ((agent-shell-anthropic-claude-environment
             (cons (concat "CLAUDE_CONFIG_DIR=" (directory-file-name config))
                   agent-shell-anthropic-claude-environment)))
        (apply fn args)))))

;;;; Shell names

(defun agent-git--agent-display-name (agent-name account)
  "Return AGENT-NAME, plus ACCOUNT when Claude runs on a non-default one.
For example \"Claude (work)\" for a repo mapped to ~/.claude-work."
  (if (and account (string-prefix-p "Claude" agent-name))
      (format "%s (%s)" agent-name account)
    agent-name))

(defun agent-git--format-name (agent-name account ctx fallback)
  "Return a shell name for AGENT-NAME on ACCOUNT in CTX, or FALLBACK outside git."
  (concat (agent-git--agent-display-name agent-name account) " @ "
          (if ctx
              (concat (agent-git-context-label ctx)
                      (when-let* ((branch (agent-git-context-branch ctx)))
                        (concat agent-git-branch-separator branch)))
            fallback)))

;;;###autoload
(defun agent-git-buffer-name (agent-name project-name)
  "Name a new shell after AGENT-NAME and its repo, worktree and branch.
For `agent-shell-buffer-name-format'.  Uses PROJECT-NAME outside git."
  ;; The account comes from the directory: the current buffer may be another
  ;; shell, on another account.
  (agent-git--format-name agent-name (agent-git-claude-account)
                          (agent-git-context) project-name))

(defun agent-git--strip-uniquifier (name)
  "Return NAME without a trailing <N> that `rename-buffer' adds."
  (replace-regexp-in-string "<[0-9]+>\\'" "" name))

(defun agent-git--agent-name (&optional buffer)
  "Return the agent's display name (\"Claude\", \"Codex\") in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (ignore-errors (map-nested-elt agent-shell--state '(:agent-config :buffer-name)))
        "Agent")))

(defun agent-git-refresh-buffer-name (&optional buffer)
  "Rename agent shell BUFFER if its branch changed since it was named."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when-let* ((ctx (agent-git-context))
                    (old (buffer-name))
                    (new (agent-git--format-name (agent-git--agent-name)
                                                 (agent-git--claude-account-here) ctx nil))
                    ((not (string= (agent-git--strip-uniquifier old) new))))
          (let ((viewport (get-buffer (concat old " [viewport]"))))
            (shell-maker-set-buffer-name buffer new)
            (when (buffer-live-p viewport)
              (with-current-buffer viewport
                (rename-buffer (concat (buffer-name buffer) " [viewport]") t)))))))))

;;;###autoload
(defun agent-git-setup-shell-h ()
  "Rename this shell whenever a turn ends on a different branch.
Also links the worktree's `agent-git-worktree-shared-files', before the
agent starts.  For `agent-shell-mode-hook'."
  (with-demoted-errors "agent-git: %S"
    (agent-git-link-shared-files))
  (let ((buffer (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'turn-complete
     :on-event (lambda (_event) (agent-git-refresh-buffer-name buffer)))))

;;;; Finding and showing shells

(defun agent-git--shells ()
  "Return all live agent shell buffers."
  (when (featurep 'agent-shell)
    (seq-filter #'buffer-live-p (agent-shell-buffers))))

(defun agent-git--activity (buffer)
  "Return BUFFER's last activity as a float time, 0 if none yet."
  (if-let* ((time (ignore-errors (agent-shell-last-activity-time :shell-buffer buffer))))
      (float-time time)
    0))

(defun agent-git--shells-in (root)
  "Return the shells working in ROOT, most recently active first."
  (let ((root (agent-git--truename root)))
    (sort (seq-filter (lambda (buffer)
                        (string= (agent-git--truename
                                  (buffer-local-value 'default-directory buffer))
                                 root))
                      (agent-git--shells))
          (lambda (a b) (> (agent-git--activity a) (agent-git--activity b))))))

(defun agent-git--status (buffer)
  "Return BUFFER's status: `blocked', `busy', `ready', `starting' or `unknown'."
  (condition-case nil
      (if (agent-shell-initialized-p :shell-buffer buffer)
          (agent-shell-status :shell-buffer buffer)
        'starting)
    (error 'unknown)))

(defun agent-git--session-title (buffer)
  "Return the session title of BUFFER's agent, or nil."
  (when-let* ((title (ignore-errors
                       (map-nested-elt (buffer-local-value 'agent-shell--state buffer)
                                       '(:session :title))))
              ((stringp title)))
    (car (split-string title "\n" t))))

(defun agent-git--current-root ()
  "Return the root of the worktree a new shell here would work in."
  (file-name-as-directory
   (or (and (derived-mode-p 'agent-shell-mode) default-directory)
       (and (require 'agent-shell nil t)
            (ignore-errors (agent-shell-cwd)))
       default-directory)))

(defun agent-git-switch-to-workspace-of (buffer)
  "Switch to the persp-mode workspace that holds BUFFER, if not current.
Also for `agent-shell-attention-before-jump-hook'."
  (when (and (bound-and-true-p persp-mode) (buffer-live-p buffer))
    (let ((persps (persp--buffer-in-persps buffer)))
      (unless (or (null persps) (memq (get-current-persp) persps))
        (let ((name (safe-persp-name (car persps))))
          (if (fboundp '+workspace-switch)
              (+workspace-switch name)
            (persp-switch name)))))))

;;;###autoload
(defun agent-git-visit-shell (buffer)
  "Show agent shell BUFFER, switching to the workspace that holds it."
  (agent-git-switch-to-workspace-of buffer)
  (if-let* ((window (get-buffer-window buffer)))
      (select-window window)
    (pop-to-buffer buffer (bound-and-true-p agent-shell-display-action))))

;;;###autoload
(defun agent-git-visit-shell-named (name)
  "Visit the agent shell named NAME and raise its frame.
Notifications call this through emacsclient when clicked."
  (if-let* ((buffer (get-buffer name)))
      (progn (agent-git-visit-shell buffer)
             (select-frame-set-input-focus (selected-frame)))
    (message "Agent shell %s is gone" name)))

;;;###autoload
(defun agent-git-display-in-agent-window (buffer alist)
  "Display BUFFER in a window that already shows an agent shell.
A `display-buffer' action function: switching between shells reuses
the agent window instead of splitting another one.  ALIST is the
action's alist."
  (when-let* ((window (seq-find
                       (lambda (window)
                         (provided-mode-derived-p
                          (buffer-local-value 'major-mode (window-buffer window))
                          'agent-shell-mode))
                       (window-list nil 'nomini))))
    (window--display-buffer buffer window 'reuse alist)))

(defun agent-git--start-shell (root &optional choose-agent)
  "Start a new agent shell in ROOT.
With CHOOSE-AGENT, ask which agent even when one is preferred."
  (let ((default-directory (file-name-as-directory root))
        (agent-shell-preferred-agent-config
         (if (and choose-agent (symbolp agent-shell-preferred-agent-config)
                  agent-shell-preferred-agent-config)
             (cons 'preselect agent-shell-preferred-agent-config)
           agent-shell-preferred-agent-config)))
    (agent-shell '(4))))

;;;; Pickers

(defun agent-git--col (text width)
  "Return TEXT cut or padded to WIDTH columns, plus a separating space."
  (concat (truncate-string-to-width text width nil ?\s "…") " "))

(defun agent-git--status-text (status)
  "Return STATUS as a short, colored word."
  (propertize (symbol-name status)
              'face (pcase status
                      ('blocked 'error)
                      ('busy 'warning)
                      ('ready 'success)
                      (_ 'shadow))))

(defun agent-git--read (prompt candidates)
  "Read one of CANDIDATES with PROMPT, grouped by repo, in the given order.
CANDIDATES is a list of (STRING GROUP . VALUE); return the VALUE chosen."
  (let* ((table (make-hash-table :test #'equal))
         (strings (mapcar (lambda (candidate)
                            (let ((key (car candidate)) (n 1))
                              (while (gethash (substring-no-properties key) table)
                                (setq key (format "%s <%d>" (car candidate) (cl-incf n))))
                              (puthash (substring-no-properties key) (cdr candidate) table)
                              key))
                          candidates))
         (group (lambda (string transform)
                  (if transform
                      string
                    (car (gethash (substring-no-properties string) table)))))
         (choice (completing-read
                  prompt
                  (lambda (input predicate action)
                    (if (eq action 'metadata)
                        `(metadata (category . agent-git)
                                   (group-function . ,group)
                                   (display-sort-function . identity)
                                   (cycle-sort-function . identity))
                      (complete-with-action action strings input predicate)))
                  nil t))
         (entry (gethash (substring-no-properties choice) table)))
    (unless entry
      (user-error "Nothing selected"))
    (cdr entry)))

(defun agent-git--status-rank (status)
  "Sort key for STATUS: shells waiting on you first."
  (pcase status ('blocked 0) ('ready 1) ('busy 2) (_ 3)))

;;;###autoload
(defun agent-git-switch-shell ()
  "Switch to any agent shell, across all projects and worktrees.
Each line shows status, repo:worktree, branch, session title and agent,
so typing part of a branch, ticket or title finds the shell."
  (interactive)
  (let* ((shells (or (agent-git--shells) (user-error "No agent shells running")))
         (entries
          (mapcar
           (lambda (buffer)
             (agent-git-refresh-buffer-name buffer)
             (let ((ctx (with-current-buffer buffer (agent-git-context))))
               (list :buffer buffer :ctx ctx
                     :status (agent-git--status buffer)
                     :time (agent-git--activity buffer))))
           shells))
         (sorted (sort entries
                       (lambda (a b)
                         (let ((ra (agent-git--status-rank (plist-get a :status)))
                               (rb (agent-git--status-rank (plist-get b :status))))
                           (if (= ra rb)
                               (> (plist-get a :time) (plist-get b :time))
                             (< ra rb)))))))
    (agent-git-visit-shell
     (agent-git--read
      "Agent shell: "
      (mapcar
       (lambda (entry)
         (let* ((buffer (plist-get entry :buffer))
                (ctx (plist-get entry :ctx))
                (label (if ctx (agent-git-context-label ctx)
                         (file-name-nondirectory
                          (directory-file-name (buffer-local-value 'default-directory buffer)))))
                (branch (or (and ctx (agent-git-context-branch ctx)) "")))
           (cons (concat
                  (agent-git--col (agent-git--status-text (plist-get entry :status)) 8)
                  (agent-git--col (propertize label 'face 'font-lock-keyword-face) 30)
                  (agent-git--col (propertize branch 'face 'font-lock-constant-face) 48)
                  (propertize (or (agent-git--session-title buffer) "") 'face 'font-lock-doc-face)
                  "  " (propertize (format "[%s]" (with-current-buffer buffer
                                                    (agent-git--agent-display-name
                                                     (agent-git--agent-name)
                                                     (agent-git--claude-account-here))))
                                   'face 'shadow))
                 (cons (if ctx (agent-git-context-repo ctx) "other") buffer))))
       sorted)))))

(defun agent-git--projectile-projects ()
  "Return projectile's known projects, loading projectile's list if needed.
Doom loads projectile lazily, so early in a session the list may not be
read yet."
  (when (require 'projectile nil t)
    (when (and (null projectile-known-projects)
               (fboundp 'projectile-load-known-projects))
      (projectile-load-known-projects))
    projectile-known-projects))

(defun agent-git--known-common-dirs (&optional first)
  "Return the common dirs of known repos, the repo of directory FIRST first.
Known repos are those of live shells and of projectile's known projects."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (dir (append (list first)
                         (mapcar (lambda (buffer) (buffer-local-value 'default-directory buffer))
                                 (agent-git--shells))
                         (agent-git--projectile-projects)))
      (when-let* (((stringp dir))
                  (dir (expand-file-name dir))
                  ((not (file-remote-p dir)))
                  ((file-directory-p dir))
                  (ctx (agent-git-context dir))
                  (common (agent-git-context-common-dir ctx))
                  (key (agent-git--truename common))
                  ((not (gethash key seen))))
        (puthash key t seen)
        (push common result)))
    (nreverse result)))

(defun agent-git--all-worktrees ()
  "Return contexts for every worktree of every known repo, current repo first."
  (mapcan #'agent-git--repo-worktrees
          (agent-git--known-common-dirs (agent-git--current-root))))

(defun agent-git--shells-summary (shells)
  "Describe SHELLS, e.g. \"2 shells: busy ready\"."
  (if (null shells)
      ""
    (propertize (format "%d shell%s: %s" (length shells)
                        (if (cdr shells) "s" "")
                        (mapconcat (lambda (b) (symbol-name (agent-git--status b)))
                                   shells " "))
                'face 'success)))

;;;###autoload
(defun agent-git-switch-worktree (&optional new-shell)
  "Go to a worktree of any known repo: its workspace and its agent shell.
Lines show repo:worktree, branch, running shells and path.  Reuses the
worktree's most recent shell, or starts one.  With prefix argument
NEW-SHELL, always start a new shell."
  (interactive "P")
  (let ((worktrees (or (agent-git--all-worktrees) (user-error "No git repositories known"))))
    (agent-git-open-worktree
     (agent-git--read
      "Worktree: "
      (mapcar (lambda (ctx)
                (cons (agent-git--worktree-line ctx)
                      (cons (agent-git-context-repo ctx) (agent-git-context-root ctx))))
              worktrees))
     new-shell)))

;;;###autoload
(defun agent-git-open-worktree (root &optional new-shell)
  "Go to the worktree at ROOT: switch to its project, then show its shell.
Reuses the most recently active shell there unless NEW-SHELL."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (when agent-git-switch-project-function
      (funcall agent-git-switch-project-function root))
    (if-let* (((not new-shell))
              (shell (car (agent-git--shells-in root))))
        (agent-git-visit-shell shell)
      (agent-git--start-shell root))))

;;;###autoload
(defun agent-git-toggle ()
  "Show or hide this worktree's agent shell, starting one if there is none.
Unlike `agent-shell-toggle', never falls back to another project's shell."
  (interactive)
  (if (derived-mode-p 'agent-shell-mode)
      (quit-window)
    (let ((root (agent-git--current-root)))
      (if-let* ((shell (car (agent-git--shells-in root))))
          (if-let* ((window (get-buffer-window shell)))
              (quit-window nil window)
            (agent-git-visit-shell shell))
        (agent-git--start-shell root)))))

(defun agent-git--kill-shell-buffer (buffer)
  "Kill agent shell BUFFER, and its agent, without asking.
A window showing it in this frame goes back to what it showed before,
or closes if it was opened for the shell."
  (when (buffer-live-p buffer)
    ;; Callers ask first: skip agent-shell's and the process's questions.
    (with-current-buffer buffer
      (setq-local kill-buffer-query-functions nil))
    (if-let* ((window (get-buffer-window buffer)))
        (quit-window t window)
      (kill-buffer buffer))))

(defun agent-git--kill-reasons (buffer)
  "Return what killing agent shell BUFFER would interrupt, as phrases."
  (let ((queued (length (ignore-errors
                          (map-elt (buffer-local-value 'agent-shell--state buffer)
                                   :pending-prompts)))))
    (delq nil (list (pcase (agent-git--status buffer)
                      ('busy "is working")
                      ('blocked "is waiting on you"))
                    (when (> queued 0)
                      (format "has %s queued" (agent-git--count queued "prompt")))))))

;;;###autoload
(defun agent-git-kill-shell (&optional buffer)
  "Kill agent shell BUFFER (default: this one) and its agent.
Asks first only while the agent is working, waits on you, or has
prompts queued.  The window goes back to what it showed before, or
closes if it opened for the shell.  The session itself stays: /resume
or \\[agent-git-resume-session] brings it back."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (unless (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                     'agent-shell-mode)
      (user-error "Not in an agent shell"))
    (when-let* ((reasons (agent-git--kill-reasons buffer))
                ((not (y-or-n-p (format "%s %s; kill it? " (buffer-name buffer)
                                        (string-join reasons " and "))))))
      (user-error "Kept %s" (buffer-name buffer)))
    (agent-git--kill-shell-buffer buffer)))

;;;###autoload
(defun agent-git-new-shell ()
  "Start another shell in this worktree, choosing the agent."
  (interactive)
  (agent-git--start-shell (agent-git--current-root) t))

;;;###autoload
(defun agent-git-resume-session ()
  "Pick a past session of this repo and resume it in a new shell here."
  (interactive)
  (let ((agent-shell-session-strategy 'prompt))
    (agent-git--start-shell (agent-git--current-root))))

;;;; Shells in any project

(defun agent-git--project-candidates ()
  "Return a (STRING GROUP . ROOT) candidate for each known project.
Projects are projectile's, most recent first; remote and missing ones
are left out.  Lines show repo:worktree, branch, running shells and path."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (dir (agent-git--projectile-projects))
      (when-let* (((stringp dir))
                  (root (file-name-as-directory (expand-file-name dir)))
                  ((not (file-remote-p root)))
                  ((file-directory-p root))
                  (key (agent-git--truename root))
                  ((not (gethash key seen))))
        (puthash key t seen)
        (let ((ctx (agent-git-context root)))
          (push (cons (concat
                       (agent-git--col (propertize (if ctx
                                                       (agent-git-context-label ctx)
                                                     (file-name-nondirectory
                                                      (directory-file-name root)))
                                                   'face 'font-lock-keyword-face)
                                       30)
                       (agent-git--col (propertize (or (and ctx (agent-git-context-branch ctx)) "")
                                                   'face 'font-lock-constant-face)
                                       48)
                       (agent-git--col (agent-git--shells-summary (agent-git--shells-in root)) 22)
                       (propertize (abbreviate-file-name root) 'face 'shadow))
                      (cons (if ctx (agent-git-context-repo ctx) "other") root))
                result))))
    (nreverse result)))

(defun agent-git--start-in-project (start prompt)
  "Read a known project with PROMPT, go to its workspace, then call START there.
START is a command that starts a new shell in `default-directory'."
  (let ((root (agent-git--read prompt (or (agent-git--project-candidates)
                                          (user-error "No known projects")))))
    (when agent-git-switch-project-function
      (funcall agent-git-switch-project-function root))
    (let ((default-directory root))
      (funcall start))))

;;;###autoload
(defun agent-git-new-claude-in-project ()
  "Pick any known project, go to its workspace and start a Claude shell there.
Works before any project is open, such as right after Emacs starts."
  (interactive)
  (agent-git--start-in-project #'agent-shell-anthropic-start-claude-code
                               "New Claude shell in project: "))

;;;###autoload
(defun agent-git-new-codex-in-project ()
  "Pick any known project, go to its workspace and start a Codex shell there.
Works before any project is open, such as right after Emacs starts."
  (interactive)
  (agent-git--start-in-project #'agent-shell-openai-start-codex
                               "New Codex shell in project: "))

;;;; /resume in a shell

(defun agent-git--resume-query (command)
  "Return the text after /resume when COMMAND is /resume, \"\" for none.
Returns nil for any other COMMAND."
  (when (and (stringp command)
             (string-match "\\`[ \t\n]*/resume\\(?:[ \t]+\\(.*?\\)\\)?[ \t\n]*\\'" command))
    (or (match-string 1 command) "")))

(defun agent-git--session-shell (session-id &optional except)
  "Return the live shell other than EXCEPT whose session is SESSION-ID, or nil."
  (seq-find (lambda (shell)
              (and (not (eq shell except))
                   (equal (ignore-errors
                            (map-nested-elt (buffer-local-value 'agent-shell--state shell)
                                            '(:session :id)))
                          session-id)))
            (agent-git--shells)))

(defun agent-git--session-choices (sessions &optional except)
  "Return (LABEL . SESSION) for SESSIONS, in agent-shell's picker columns.
Sessions open in a shell other than EXCEPT say so."
  (let ((widths (mapcar (lambda (column)
                          (cons column
                                (apply #'max 0 (mapcar (lambda (session)
                                                         (length (agent-shell--session-column-value
                                                                  column session)))
                                                       sessions))))
                        (agent-shell--session-selection-columns))))
    (mapcar (lambda (session)
              (cons (concat (agent-shell--session-choice-label :acp-session session
                                                               :max-widths widths)
                            (when (agent-git--session-shell (map-elt session 'sessionId) except)
                              (propertize "  [open in a shell]" 'face 'warning)))
                    session))
            sessions)))

(defun agent-git--resume-pick (buffer current sessions query)
  "Pick one of SESSIONS but CURRENT, starting from QUERY; resume it in BUFFER."
  (when (buffer-live-p buffer)
    (let ((sessions (seq-remove (lambda (session)
                                  (equal (map-elt session 'sessionId) current))
                                (agent-shell--sort-sessions-by-recency sessions))))
      (if (null sessions)
          (message "No other sessions for %s"
                   (abbreviate-file-name (buffer-local-value 'default-directory buffer)))
        (let* ((choices (agent-git--session-choices sessions buffer))
               (choice (completing-read
                        "Resume session: "
                        (lambda (string predicate action)
                          (if (eq action 'metadata)
                              '(metadata (display-sort-function . identity)
                                         (cycle-sort-function . identity))
                            (complete-with-action action choices string predicate)))
                        nil t (unless (string-empty-p query) query)))
               (session (cdr (assoc choice choices)))
               (open (and session (agent-git--session-shell
                                   (map-elt session 'sessionId) buffer))))
          (cond (open (agent-git-visit-shell open)
                      (message "That session is open in %s" (buffer-name open)))
                (session
                 (with-current-buffer buffer
                   (agent-shell-restart :session-id (map-elt session 'sessionId))))))))))

;;;###autoload
(defun agent-git-resume-in-shell (&optional buffer query)
  "Pick another session of shell BUFFER's directory and resume it there.
Like Claude Code's /resume: BUFFER (default the current shell) switches
to the chosen session, keeping its name, window and account.  QUERY is
the picker's initial input.  The sessions are the ones BUFFER's agent
lists for its directory, from tmux as well as from Emacs.  A session
another shell already runs is shown there instead of being opened twice."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        (query (or query "")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not in an agent shell"))
      (let ((state agent-shell--state)
            (current (map-nested-elt agent-shell--state '(:session :id))))
        (unless current
          (user-error "Starting agent, please wait"))
        (unless (map-elt state :supports-session-list)
          (user-error "This agent can't list its sessions"))
        (message "Looking for sessions...")
        (agent-shell--list-sessions
         :state state
         :cwd (agent-shell--resolve-path (agent-shell-cwd))
         :buffer buffer
         ;; Out of the process filter before prompting.
         :on-success (lambda (sessions)
                       (run-at-time 0 nil #'agent-git--resume-pick
                                    buffer current sessions query))
         :on-failure (lambda (error &rest _)
                       (message "Couldn't list sessions: %s"
                                (or (ignore-errors (map-elt error 'message)) error))))))))

;;;###autoload
(defun agent-git-resume-command-a (fn &rest args)
  "Around advice for `agent-shell--handle' (FN, ARGS): run /resume in Emacs.
Claude Code's /resume only exists in its terminal UI; over ACP it answers
\"isn't available in this environment\".  So a prompt that is just
/resume [QUERY] isn't sent: it ends the turn and opens
`agent-git-resume-in-shell'.  Anything else, and /resume before the
session is up, goes to FN as usual."
  (let* ((buffer (plist-get args :shell-buffer))
         (query (agent-git--resume-query (plist-get args :command))))
    (if (and query
             (buffer-live-p buffer)
             (map-nested-elt (buffer-local-value 'agent-shell--state buffer)
                             '(:session :id)))
        (with-current-buffer buffer
          (agent-shell--finish-output :config shell-maker--config :success t)
          (run-at-time 0 nil (lambda ()
                               (condition-case err
                                   (agent-git-resume-in-shell buffer query)
                                 (user-error (message "%s" (error-message-string err)))))))
      (apply fn args))))

;;;; Creating worktrees

(defun agent-git--refs (dir)
  "Return the branches of the repo at DIR as (LOCAL-NAMES . REMOTE-NAMES).
Remote names keep their remote, as in \"origin/develop\"."
  (let ((default-directory dir)
        locals remotes)
    (dolist (ref (process-lines-ignore-status
                  "git" "for-each-ref" "--format=%(refname)" "refs/heads" "refs/remotes"))
      (cond ((string-suffix-p "/HEAD" ref))
            ((string-prefix-p "refs/heads/" ref)
             (push (string-remove-prefix "refs/heads/" ref) locals))
            ((string-prefix-p "refs/remotes/" ref)
             (push (string-remove-prefix "refs/remotes/" ref) remotes))))
    (cons (nreverse locals) (nreverse remotes))))

(defun agent-git--repo-setting (alist main-root)
  "Return the value of the first ALIST entry whose regexp matches MAIN-ROOT."
  (cdr (seq-find (lambda (entry) (string-match-p (car entry) main-root)) alist)))

(defun agent-git--branch-prefix (main-root)
  "Return the branch name prefix configured for the repo at MAIN-ROOT."
  (or (agent-git--repo-setting agent-git-branch-prefix-alist main-root) ""))

(defun agent-git--read-new-branch (main-root)
  "Read a new branch name as type/PREFIXname for the repo at MAIN-ROOT."
  (let* ((type (string-trim (completing-read "Branch type (empty for none): "
                                             agent-git-branch-types)))
         (prefix (concat (unless (string-empty-p type) (concat type "/"))
                         (agent-git--branch-prefix main-root)))
         (name (replace-regexp-in-string
                "[[:space:]]+" "-"
                (string-trim (read-string (format "Branch name: %s" prefix))))))
    (when (string-empty-p name)
      (user-error "Empty branch name"))
    (concat prefix name)))

(defun agent-git--worktree-dir (main-root repo branch)
  "Return the default directory for BRANCH: a sibling of MAIN-ROOT.
For example webapp + feature/KZN-1-x gives ../webapp_feature-KZN-1-x."
  (expand-file-name (concat repo "_" (replace-regexp-in-string "[/[:space:]]+" "-" branch))
                    (file-name-directory (directory-file-name main-root))))

;;;###autoload
(defun agent-git-new-worktree (&optional existing)
  "Create a worktree on a new branch, next to the main one, and start a shell.
With prefix argument EXISTING, check out an existing branch instead; a
remote branch gets a local tracking branch of the same name."
  (interactive "P")
  (let* ((ctx (or (agent-git-context (agent-git--current-root))
                  (user-error "Not in a git repository")))
         (common (agent-git-context-common-dir ctx))
         (repo (agent-git-context-repo ctx))
         (main (or (agent-git--main-root common)
                   (user-error "Bare repositories aren't supported")))
         (refs (agent-git--refs main))
         (all-refs (append (car refs) (cdr refs)))
         (branch (if existing
                     (let ((ref (completing-read "Branch to check out: " all-refs nil t)))
                       (if (member ref (car refs))
                           ref
                         ;; origin/foo: git makes a local foo tracking it.
                         (replace-regexp-in-string "\\`[^/]+/" "" ref)))
                   (agent-git--read-new-branch main)))
         (base (unless existing
                 (completing-read (format "Start %s from: " branch) all-refs nil t nil nil
                                  (agent-git--branch common))))
         (default (agent-git--worktree-dir main repo branch))
         (path (directory-file-name
                (expand-file-name
                 (read-directory-name "Worktree directory: "
                                      (file-name-directory default) nil nil
                                      (file-name-nondirectory default))))))
    (when (file-exists-p path)
      (user-error "%s already exists" path))
    (let ((default-directory main))
      (with-temp-buffer
        (unless (zerop (apply #'process-file "git" nil t nil
                              (if existing
                                  (list "worktree" "add" path branch)
                                (list "worktree" "add" "-b" branch path base))))
          (user-error "git worktree add failed: %s" (string-trim (buffer-string))))))
    (agent-git-link-shared-files path)
    (run-hook-with-args 'agent-git-worktree-created-hook path)
    (message "Created worktree %s on %s" (abbreviate-file-name path) branch)
    (agent-git-open-worktree path t)))

;;;; Files shared from the main worktree

;;;###autoload
(defun agent-git-link-shared-files (&optional dir)
  "Link DIR's worktree to the main worktree's `agent-git-worktree-shared-files'.
Each listed file the main worktree has and DIR's linked worktree lacks
becomes a symlink to the main worktree's copy, where git ignores that
path.  DIR defaults to `default-directory'.  Returns the files linked."
  (when-let* ((ctx (agent-git-context dir))
              ((agent-git-context-worktree ctx))
              (main (agent-git--main-root (agent-git-context-common-dir ctx)))
              (files (agent-git--repo-setting agent-git-worktree-shared-files main)))
    (let ((root (agent-git-context-root ctx))
          linked)
      (dolist (file files)
        (let ((target (expand-file-name file main))
              (link (expand-file-name file root)))
          ;; `file-symlink-p' also leaves a dangling link alone.
          (when (and (file-exists-p target)
                     (not (file-symlink-p link))
                     (not (file-exists-p link))
                     (agent-git--git-ignored-p root file))
            (condition-case err
                (progn
                  (make-directory (file-name-directory link) t)
                  (make-symbolic-link target link)
                  (push file linked))
              (error (message "agent-git: couldn't link %s: %s"
                              (abbreviate-file-name link) (error-message-string err)))))))
      (nreverse linked))))

;;;; Removing worktrees

(defun agent-git--git (dir &rest args)
  "Run git with ARGS in DIR; return (EXIT . OUTPUT), OUTPUT right-trimmed."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (let ((exit (apply #'process-file "git" nil t nil args)))
        (cons exit (string-trim-right (buffer-string)))))))

(defun agent-git--git-lines (dir &rest args)
  "Return the output lines of git ARGS in DIR, or signal a `user-error'."
  (pcase-let ((`(,exit . ,output) (apply #'agent-git--git dir args)))
    (unless (eql exit 0)
      (user-error "git %s failed: %s" (car args) output))
    (split-string output "\n" t)))

(defun agent-git--verified-ref (dir ref)
  "Return REF if it names a commit in DIR's repo, else nil."
  (when (eql 0 (car (agent-git--git dir "rev-parse" "--verify" "--quiet"
                                    (concat ref "^{commit}"))))
    ref))

(defun agent-git--base-ref (dir)
  "Return the ref work in DIR's repo merges into, or nil if it doesn't exist.
See `agent-git-base-branch-alist'."
  (agent-git--verified-ref
   dir (or (agent-git--repo-setting agent-git-base-branch-alist (agent-git--repo-root dir))
           "origin/HEAD")))

(defun agent-git--commits-only-on (dir ref &rest others)
  "Return how many commits REF has that none of OTHERS has, in DIR's repo.
OTHERS that are nil or name no commit are left out.  Returns nil when
git can't tell."
  (pcase-let ((`(,exit . ,output)
               (apply #'agent-git--git dir "rev-list" "--count" ref "--not"
                      (delq nil (mapcar (lambda (other)
                                          (and other (agent-git--verified-ref dir other)))
                                        others)))))
    (when (eql exit 0)
      (string-to-number output))))

(defun agent-git--worktree-branch (root)
  "Return the branch checked out in the worktree at ROOT, or nil if detached."
  (pcase-let ((`(,exit . ,output)
               (agent-git--git root "symbolic-ref" "--quiet" "--short" "HEAD")))
    (and (eql exit 0) output)))

(defun agent-git--upstream-gone-p (root branch)
  "Return non-nil if BRANCH tracks a remote branch that no longer exists."
  (equal (car (agent-git--git-lines root "for-each-ref" "--format=%(upstream:track)"
                                    (concat "refs/heads/" branch)))
         "[gone]"))

(defun agent-git--disposable-p (path)
  "Return non-nil if a part of PATH matches `agent-git-disposable-ignored'."
  (seq-some (lambda (part)
              (seq-some (lambda (pattern) (string-match-p (wildcard-to-regexp pattern) part))
                        agent-git-disposable-ignored))
            (split-string path "/" t)))

(defun agent-git--worktree-file-buffers (root)
  "Return the live buffers visiting files or directories inside ROOT.
That is file buffers, plus Dired, Magit and terminal buffers working
there.  Other buffers are left alone, since special ones such as
*Messages* can have any `default-directory'."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (seq-filter
     (lambda (buffer)
       (let ((file (buffer-local-value 'buffer-file-name buffer)))
         (if file
             (string-prefix-p root (expand-file-name file))
           (and (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                         '(dired-mode magit-mode vterm-mode
                                           eshell-mode shell-mode compilation-mode))
                (string-prefix-p root (expand-file-name
                                       (buffer-local-value 'default-directory buffer)))))))
     (buffer-list))))

(defun agent-git--removal (ctx)
  "Return what removing CTX's linked worktree involves, as a plist.
Keys: :ctx; :root; :main, the main worktree's root; :branch, nil when
detached; :unsaved, how many of the branch's commits are in neither the
base branch nor its upstream (deleting the branch loses them); :changes,
git status lines of uncommitted changes and untracked files, except
untracked ones `agent-git-disposable-ignored' covers, such as the
.serena/ an MCP server leaves (:junk lists those); :kept, ignored paths
that would go, beyond `agent-git-disposable-ignored' and symlinks;
:shells and :busy, the shells working there."
  (let* ((root (agent-git-context-root ctx))
         (branch (agent-git--worktree-branch root))
         changes junk kept)
    (when (string-suffix-p "(rebasing)" (or (agent-git-context-branch ctx) ""))
      (user-error "%s is in the middle of a rebase" (agent-git-context-label ctx)))
    (dolist (line (agent-git--git-lines root "status" "--porcelain"
                                        "--ignored=matching" "--untracked-files=all"))
      (let ((path (substring line 3)))
        (cond ((string-prefix-p "!! " line)
               (unless (or (agent-git--disposable-p path)
                           (file-symlink-p (expand-file-name (directory-file-name path) root)))
                 (push path kept)))
              ((and (string-prefix-p "?? " line) (agent-git--disposable-p path))
               (push path junk))
              (t (push line changes)))))
    (let ((shells (agent-git--shells-in root)))
      (list :ctx ctx :root root
            :main (agent-git--main-root (agent-git-context-common-dir ctx))
            :branch branch
            :unsaved (and branch
                          (agent-git--commits-only-on
                           root (concat "refs/heads/" branch) (agent-git--base-ref root)
                           (concat branch "@{upstream}")))
            :changes (nreverse changes) :junk (nreverse junk) :kept (nreverse kept)
            :shells shells
            :busy (seq-filter (lambda (b) (memq (agent-git--status b) '(busy blocked)))
                              shells)))))

(defun agent-git--count (n singular)
  "Return N SINGULAR, pluralized with an s when N isn't 1."
  (format "%d %s%s" n singular (if (= n 1) "" "s")))

(defun agent-git--preview (items)
  "Return the first few ITEMS joined by commas, noting how many more there are."
  (concat (string-join (seq-take items 4) ", ")
          (when (nthcdr 4 items) (format " and %d more" (- (length items) 4)))))

(defun agent-git--describe-removal (removal &optional why)
  "Return a yes-or-no question for REMOVAL, from `agent-git--removal'.
WHY, when given, says why the worktree is offered."
  (let* ((ctx (plist-get removal :ctx))
         (branch (plist-get removal :branch))
         (unsaved (plist-get removal :unsaved))
         (shells (plist-get removal :shells))
         (busy (plist-get removal :busy))
         (changes (plist-get removal :changes))
         (kept (plist-get removal :kept))
         (parts
          (delq nil
                (list
                 (when shells
                   (format "closing %s%s" (agent-git--count (length shells) "shell")
                           (if busy (format " (%d busy)" (length busy)) "")))
                 (when changes
                   (format "discarding %s"
                           (agent-git--count (length changes) "uncommitted change")))
                 (when kept
                   (format "deleting ignored %s" (agent-git--preview kept)))
                 (cond ((null branch) nil)
                       ((eql unsaved 0) (format "deleting branch %s" branch))
                       (unsaved (format "keeping branch %s (%s only there)" branch
                                        (agent-git--count unsaved "commit")))
                       (t (format "keeping branch %s" branch)))))))
    (format "Remove %s%s (%s)%s? "
            (agent-git-context-label ctx)
            (if why (format " [%s]" why) "")
            (abbreviate-file-name (directory-file-name (plist-get removal :root)))
            (if parts (concat ", " (string-join parts ", ")) ""))))

(defun agent-git--remove (removal)
  "Remove the worktree REMOVAL describes; see `agent-git--removal'.
Closes its shells and buffers, runs `agent-git-remove-worktree-functions',
deletes the worktree, then its branch when that loses no commit."
  (let* ((ctx (plist-get removal :ctx))
         (root (plist-get removal :root))
         (main (plist-get removal :main))
         (branch (plist-get removal :branch))
         (dropped (and branch (eql (plist-get removal :unsaved) 0))))
    (mapc #'agent-git--kill-shell-buffer (plist-get removal :shells))
    (run-hook-with-args 'agent-git-remove-worktree-functions root)
    (let ((kill-buffer-query-functions nil))
      (mapc #'kill-buffer (agent-git--worktree-file-buffers root)))
    ;; Buffers left working there, such as a dashboard, move to the main
    ;; worktree: commands fail in a directory that's gone.
    (let ((inside (file-name-as-directory (expand-file-name root))))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p inside (expand-file-name
                                       (or (buffer-local-value 'default-directory buffer) "/")))
          (with-current-buffer buffer
            (setq default-directory (file-name-as-directory main))))))
    ;; git refuses untracked files without --force, even disposable ones.
    (apply #'agent-git--git-lines main "worktree" "remove"
           (append (when (or (plist-get removal :changes) (plist-get removal :junk))
                     '("--force"))
                   (list (directory-file-name root))))
    (when dropped
      (agent-git--git-lines main "branch" "-D" branch))
    (message "Removed %s%s" (agent-git-context-label ctx)
             (cond (dropped (format " and branch %s" branch))
                   (branch (format "; kept branch %s" branch))
                   (t "")))
    t))

(defun agent-git--check-removable (removal force)
  "Signal a `user-error' if REMOVAL shouldn't go ahead.
Uncommitted changes stop it unless FORCE; modified buffers always do."
  (let ((label (agent-git-context-label (plist-get removal :ctx)))
        (changes (plist-get removal :changes)))
    (when (and changes (not force))
      (user-error "%s has %s (%s); commit or stash them, or use C-u to discard"
                  label (agent-git--count (length changes) "uncommitted change")
                  (agent-git--preview (mapcar (lambda (line) (substring line 3)) changes))))
    (when-let* ((modified (seq-filter #'buffer-modified-p
                                      (seq-filter #'buffer-file-name
                                                  (agent-git--worktree-file-buffers
                                                   (plist-get removal :root))))))
      (user-error "Save or revert first: %s"
                  (agent-git--preview (mapcar #'buffer-name modified))))))

(defun agent-git--worktree-line (ctx)
  "Return a picker line for CTX: repo:worktree, branch, shells and path."
  (let ((root (agent-git-context-root ctx)))
    (concat
     (agent-git--col (propertize (agent-git-context-label ctx) 'face 'font-lock-keyword-face) 30)
     (agent-git--col (propertize (or (agent-git-context-branch ctx) "")
                                 'face 'font-lock-constant-face)
                     48)
     (agent-git--col (agent-git--shells-summary (agent-git--shells-in root)) 22)
     (propertize (abbreviate-file-name root) 'face 'shadow))))

(defun agent-git--linked-worktrees ()
  "Return the linked worktrees of every known repo, this worktree first."
  (let* ((here (agent-git--truename (agent-git--current-root)))
         (linked (seq-filter #'agent-git-context-worktree (agent-git--all-worktrees)))
         (current (seq-find (lambda (ctx)
                              (string-prefix-p (agent-git--truename (agent-git-context-root ctx))
                                               here))
                            linked)))
    (if current (cons current (delq current linked)) linked)))

;;;###autoload
(defun agent-git-remove-worktree (&optional force)
  "Remove a linked worktree of any known repo, this one offered first.
Closes its shells and buffers, then deletes its directory, transcripts
included; sessions stay with Claude and Codex.  Refuses while it has
uncommitted changes or untracked files, unless FORCE (prefix argument).
The question lists the shells it closes and the ignored files it deletes
beyond `agent-git-disposable-ignored'.  The branch goes too when no
commit would be lost: everything on it is in the repo's base branch (see
`agent-git-base-branch-alist') or on its upstream."
  (interactive "P")
  (let* ((ctx (agent-git--read
               "Remove worktree: "
               (mapcar (lambda (ctx)
                         (cons (agent-git--worktree-line ctx)
                               (cons (agent-git-context-repo ctx) ctx)))
                       (or (agent-git--linked-worktrees)
                           (user-error "No linked worktrees")))))
         (removal (agent-git--removal ctx)))
    (agent-git--check-removable removal force)
    (unless (yes-or-no-p (agent-git--describe-removal removal))
      (user-error "Kept %s" (agent-git-context-label ctx)))
    (agent-git--remove removal)))

(defun agent-git--finished (ctx)
  "Return why CTX's worktree looks finished, or nil.
Finished means nothing beyond the repo's base branch, or a branch whose
upstream is gone, as when a merged PR's branch was deleted."
  (let* ((root (agent-git-context-root ctx))
         (base (agent-git--base-ref root))
         (branch (agent-git--worktree-branch root)))
    (cond ((and base (eql 0 (agent-git--commits-only-on root "HEAD" base)))
           (format "nothing beyond %s" base))
          ((and branch (agent-git--upstream-gone-p root branch))
           "upstream gone"))))

;;;###autoload
(defun agent-git-clean-worktrees ()
  "Offer to remove each linked worktree whose work looks finished.
Finished means no shells, no uncommitted changes, and either nothing
beyond the repo's base branch or a branch whose upstream is gone.  Asks
about each, saying what removing it involves (y, n, ! for all, q to
stop); see `agent-git-remove-worktree'."
  (interactive)
  (let ((open 0) (removed 0) removals)
    (dolist (ctx (agent-git--linked-worktrees))
      (when-let* ((why (agent-git--finished ctx)))
        (if (agent-git--shells-in (agent-git-context-root ctx))
            (cl-incf open)
          (let ((removal (agent-git--removal ctx)))
            (unless (plist-get removal :changes)
              (push (plist-put removal :why why) removals))))))
    (if (null removals)
        (message "No finished worktrees%s"
                 (if (> open 0) (format " (%d with shells open)" open) ""))
      (map-y-or-n-p
       (lambda (removal)
         (agent-git--describe-removal removal (plist-get removal :why)))
       (lambda (removal)
         (condition-case err
             (progn (agent-git--check-removable removal nil)
                    (agent-git--remove removal)
                    (cl-incf removed))
           (user-error (message "%s" (error-message-string err)))))
       (nreverse removals)
       '("worktree" "worktrees" "remove"))
      (message "Removed %s%s" (agent-git--count removed "worktree")
               (if (> open 0) (format "; left %d with shells open" open) "")))))

;;;; Transcripts and notifications

;;;###autoload
(defun agent-git-search-transcripts ()
  "Search the agent-shell transcripts of every known worktree with ripgrep."
  (interactive)
  (require 'consult)
  (let ((dirs (seq-filter #'file-directory-p
                          (mapcar (lambda (ctx)
                                    (expand-file-name ".agent-shell/transcripts"
                                                      (agent-git-context-root ctx)))
                                  (agent-git--all-worktrees))))
        ;; Transcripts sit in an ignored, hidden directory.
        (consult-ripgrep-args (if (listp consult-ripgrep-args)
                                  (append consult-ripgrep-args '("--hidden" "--no-ignore"))
                                (concat consult-ripgrep-args " --hidden --no-ignore"))))
    (unless dirs
      (user-error "No transcripts yet"))
    (consult-ripgrep dirs)))

(defun agent-git--emacsclient ()
  "Return the emacsclient that belongs to this Emacs, or nil."
  (seq-find #'file-executable-p
            (list (expand-file-name "bin/emacsclient" invocation-directory)
                  (expand-file-name "emacsclient" invocation-directory)
                  (executable-find "emacsclient"))))

;;;###autoload
(defun agent-git-notify (buffer title body)
  "Post a macOS notification about agent shell BUFFER with TITLE and BODY.
Clicking it visits the shell.  For `agent-shell-attention-notify-function';
needs terminal-notifier."
  (when-let* ((notifier (executable-find "terminal-notifier")))
    (let* ((name (if (buffer-live-p buffer) (buffer-name buffer) title))
           (client (agent-git--emacsclient))
           (args (list "-title" title
                       "-message" (if (string-empty-p (or body "")) "Needs you" body)
                       "-group" (format "agent-git-%s" (sxhash-equal name))
                       "-activate" "org.gnu.Emacs")))
      (when (and client (bound-and-true-p server-name))
        (setq args (append args
                           (list "-execute"
                                 (mapconcat #'shell-quote-argument
                                            (list client "-s" server-name "-n" "-e"
                                                  (format "(agent-git-visit-shell-named %S)" name))
                                            " ")))))
      (apply #'call-process notifier nil 0 nil args))))

(provide 'agent-git)

;;; agent-git.el ends here
