;;; agent-hq.el --- Home base for agent-shell: worktrees, accounts, sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; Names every agent-shell buffer after the repo, worktree and branch it works
;; in and its session title, for example
;; "Claude @ webapp:worktree1 ⎇ bug/KZN-19576-max-length · Fix name length",
;; and renames it when the agent switches branches or the title changes.  The
;; name is what every buffer switcher (SPC ,, SPC b b) matches, so shells of
;; the same worktree stay apart there, and branch and title are searchable.
;;
;; Shells share one agent window: a shell shown replaces the one in the agent
;; window you're in, or else the one you were in last.  `agent-hq-beside'
;; puts the next one beside it instead.
;;
;; Claude shells can use a different account per repo, through
;; `agent-hq-claude-config-dirs' and `agent-hq-claude-environment-a'; the
;; name then shows it, as in "Claude (work) @ webapp ⎇ develop".  At an
;; account's plan limit, agent-hq-fallback.el offers to move its shells,
;; sessions and all, to another.
;;
;; Files git ignores, such as a private .claude/CLAUDE.md, live only in the
;; main worktree; `agent-hq-worktree-shared-files' links them into the
;; others as shells start there.
;;
;; Commands:
;;   `agent-hq-switch-shell'        every live shell, all projects
;;   `agent-hq-switch-worktree'     every worktree of every known repo
;;   `agent-hq-new-worktree'        new branch + sibling worktree + shell
;;   `agent-hq-remove-worktree'     close a worktree's shells, delete it and,
;;                                   if nothing would be lost, its branch
;;   `agent-hq-clean-worktrees'     offer each finished worktree for removal
;;   `agent-hq-toggle'              this worktree's shell, never another's
;;   `agent-hq-beside'              prefix: the next shell opens beside
;;   `agent-hq-kill-shell'          kill this shell, asking only if it's working
;;   `agent-hq-resume-session'      pick a past session for this worktree
;;   `agent-hq-resume-in-shell'     same, in this shell (what /resume runs)
;;   `agent-hq-find-session'        search every project's sessions, closed
;;                                   and tmux ones too, and resume one
;;   `agent-hq-new-claude-in-project', `agent-hq-new-codex-in-project'
;;                                   any known project: its workspace + a shell
;;   `agent-hq-search-transcripts'  ripgrep over every worktree's transcripts
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
(declare-function evil-normal-state "evil-states")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell--project-name "agent-shell-project")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-initialized-p "agent-shell")
(declare-function agent-shell-last-activity-time "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-restart "agent-shell")
(declare-function agent-shell-anthropic-start-claude-code "agent-shell-anthropic")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell-anthropic")
(declare-function agent-shell--start "agent-shell")
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
(declare-function agent-shell-elicitation--pending-p "agent-shell-elicitation")
(declare-function agent-shell-elicitation-jump-to-latest-form "agent-shell-elicitation")
(declare-function agent-shell-jump-to-latest-permission-button-row "agent-shell")
(defvar agent-shell--state)
(defvar agent-shell-display-action)
(defvar agent-shell-session-strategy)
(defvar agent-shell-preferred-agent-config)
(defvar agent-shell-anthropic-claude-environment)
(defvar shell-maker--config)
(defvar consult-ripgrep-args)
(defvar projectile-known-projects)
(defvar server-name)

(defgroup agent-hq nil
  "Agent-shell across repos and worktrees: shells, accounts, sessions, status line."
  :group 'tools
  :prefix "agent-hq-")

(defcustom agent-hq-branch-separator " ⎇ "
  "Text between the worktree label and the branch in shell names."
  :type 'string)

(defcustom agent-hq-title-separator " · "
  "Text between the branch and the session title in shell names."
  :type 'string)

(defcustom agent-hq-title-width 50
  "Most columns of the session title that a shell's name carries.
Longer titles are cut with \"…\".  Nil leaves the title out of names."
  :type '(choice (const :tag "No title in names" nil) natnum))

(defcustom agent-hq-beside-min-width 70
  "Fewest columns each shell keeps when `agent-hq-beside' splits sideways.
An agent window narrower than twice this splits below instead; 0 always
splits sideways."
  :type 'natnum)

(defcustom agent-hq-branch-types '("feature" "bug" "task" "fix" "chore")
  "Branch type prefixes offered by `agent-hq-new-worktree'.
An empty answer creates the branch without a type prefix."
  :type '(repeat string))

(defcustom agent-hq-branch-prefix-alist nil
  "Branch name prefixes per repo, as (DIR-REGEXP . PREFIX).
The first entry whose regexp matches the main worktree's root puts PREFIX
before the name typed in `agent-hq-new-worktree', for example
\(\"/kizen/\" . \"KZN-\") turns \"123-fix\" into \"bug/KZN-123-fix\"."
  :type '(alist :key-type regexp :value-type string))

(defcustom agent-hq-switch-project-function nil
  "Function called with a worktree root to make it the current project.
Doom points this at a function that switches to (or creates) the
project's workspace.  When nil, nothing is switched."
  :type '(choice (const :tag "Don't switch" nil) function))

(defcustom agent-hq-worktree-created-hook nil
  "Hook run with the new worktree's root after `agent-hq-new-worktree'.
Use it to copy untracked files such as .env into the worktree."
  :type 'hook)

(defcustom agent-hq-worktree-shared-files nil
  "Files kept out of git that every worktree of a repo should have, per repo.
An alist of (DIR-REGEXP . FILES).  The first entry whose regexp matches
the main worktree's root names FILES, relative to the worktree root.  A
linked worktree that lacks one gets a symlink to the main worktree's
copy, where git ignores that path, when a shell starts in it or
`agent-hq-new-worktree' creates it.  For example
\(\"/kizen/webapp/\\\\='\" \".claude/CLAUDE.md\" \"AGENTS.md\")."
  :type '(alist :key-type regexp :value-type (repeat string)))

(defcustom agent-hq-base-branch-alist nil
  "The branch work merges into, per repo, as (DIR-REGEXP . REF).
The first entry whose regexp matches the main worktree's root gives REF,
such as \"origin/develop\"; other repos use origin/HEAD.  Removing a
worktree deletes its branch only when every commit on it is in REF or on
its upstream, and `agent-hq-clean-worktrees' offers worktrees with
nothing beyond REF."
  :type '(alist :key-type regexp :value-type string))

(defcustom agent-hq-disposable-ignored
  '("__pycache__" "*.pyc" "node_modules" ".DS_Store" ".ruff_cache" ".pytest_cache"
    ".mypy_cache" ".venv" ".next" ".turbo" ".agent-shell" ".claude" ".serena" ".tokensave")
  "Ignored files that removing a worktree deletes without mentioning them.
Wildcards, matched against each part of an ignored path.  Before removing
a worktree, `agent-hq-remove-worktree' lists any other ignored file it
would delete, such as notes or local settings."
  :type '(repeat string))

(defcustom agent-hq-remove-worktree-functions nil
  "Functions called with a worktree's root just before it is removed.
They run once its shells are closed, while the directory still exists.
Doom uses this to close the worktree's workspace."
  :type 'hook)

(defcustom agent-hq-claude-config-dirs nil
  "Claude Code config dirs per repo, as (DIR-REGEXP . CONFIG-DIR).
The first entry whose regexp matches the main worktree's root runs
Claude shells in every worktree of that repo with CLAUDE_CONFIG_DIR set
to CONFIG-DIR, and so with the account logged in there.  Outside git the
shell's own directory is matched.  Shells nothing matches leave
CLAUDE_CONFIG_DIR unset and use ~/.claude.

For example, (\"/kizen/webapp/\\\\='\" . \"~/.claude-work\").  Log in to a
new CONFIG-DIR once with CLAUDE_CONFIG_DIR=CONFIG-DIR claude, then /login.

Takes effect through `agent-hq-claude-environment-a'."
  :type '(alist :key-type regexp :value-type directory))

(defcustom agent-hq-claude-shared-memory t
  "Whether repos on another Claude account keep using ~/.claude's memory.
When non-nil, starting a Claude shell in a repo that
`agent-hq-claude-config-dirs' maps sets `autoMemoryDirectory' in the
repo's .claude/settings.local.json to its memory dir under ~/.claude, so
both accounts read and write that repo's memory, in every worktree and
in tmux too.  The other account sees no other project's memory.  See
`agent-hq-claude-share-memory'."
  :type 'boolean)

;;;; Reading git state from disk

(cl-defstruct (agent-hq-context (:constructor agent-hq--make-context)
                                 (:copier nil))
  "Where a directory sits in git: its repo, worktree and branch."
  repo worktree branch root common-dir)

(defun agent-hq--read-first-line (file)
  "Return FILE's first line, trimmed, or nil if FILE can't be read.
Read as UTF-8, with no coding detection: that runs `find-auto-coding',
which editorconfig advises to ask its executable about every file, and
the pickers and the dashboard read these git files for every shell."
  (when (file-readable-p file)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents file nil 0 4096))
      (string-trim (buffer-substring-no-properties
                    (point-min) (progn (goto-char (point-min))
                                       (line-end-position)))))))

(defun agent-hq--git-dir (root)
  "Return the git dir of the worktree at ROOT, or nil.
For a linked worktree, .git is a file naming the real git dir."
  (let ((dotgit (expand-file-name ".git" root)))
    (cond ((file-directory-p dotgit) (file-name-as-directory dotgit))
          ((file-regular-p dotgit)
           (when-let* ((line (agent-hq--read-first-line dotgit))
                       ((string-match "\\`gitdir: *\\(.+\\)\\'" line)))
             (file-name-as-directory
              (expand-file-name (match-string 1 line) root)))))))

(defun agent-hq--common-dir (git-dir)
  "Return the directory GIT-DIR shares with its repo's other worktrees."
  (if-let* ((rel (agent-hq--read-first-line (expand-file-name "commondir" git-dir)))
            ((not (string-empty-p rel))))
      (file-name-as-directory (expand-file-name rel git-dir))
    git-dir))

(defun agent-hq--main-root (common-dir)
  "Return the main worktree root for COMMON-DIR, or nil for a bare repo."
  (let ((dir (directory-file-name common-dir)))
    (when (equal (file-name-nondirectory dir) ".git")
      (file-name-directory dir))))

(defun agent-hq--repo-name (common-dir)
  "Return the repo name for COMMON-DIR: the main worktree's directory name."
  (if-let* ((main (agent-hq--main-root common-dir)))
      (file-name-nondirectory (directory-file-name main))
    (string-remove-suffix ".git" (file-name-nondirectory
                                  (directory-file-name common-dir)))))

(defun agent-hq--branch (git-dir)
  "Return the branch checked out according to GIT-DIR's HEAD.
A rebase in progress shows the branch being rebased; any other
detached HEAD shows as \"detached@\" and the short hash."
  (when-let* ((head (agent-hq--read-first-line (expand-file-name "HEAD" git-dir))))
    (if (string-match "\\`ref: refs/heads/\\(.+\\)\\'" head)
        (match-string 1 head)
      (if-let* ((name (seq-some (lambda (file)
                                  (agent-hq--read-first-line
                                   (expand-file-name file git-dir)))
                                '("rebase-merge/head-name" "rebase-apply/head-name")))
                ((string-match "\\`refs/heads/\\(.+\\)\\'" name)))
          (format "%s (rebasing)" (match-string 1 name))
        (format "detached@%s" (substring head 0 (min 8 (length head))))))))

(defun agent-hq--worktree-label (repo root main-root)
  "Return a short name for the worktree at ROOT of REPO, or nil for MAIN-ROOT.
The repo name is dropped from the front, so webapp_worktree1 becomes
\"worktree1\" and gymmers-app_w1 becomes \"w1\".  A directory named just
like the repo, as in Codex's ~/.codex/worktrees/08f5/triply, is named
after its parent instead (\"08f5\")."
  (unless (and main-root (string= (agent-hq--truename root)
                                  (agent-hq--truename main-root)))
    (let* ((dir (directory-file-name root))
           (base (file-name-nondirectory dir)))
      (cond ((string-match (concat "\\`" (regexp-quote repo) "[-_.]+\\(.+\\)\\'") base)
             (match-string 1 base))
            ((string= base repo)
             (file-name-nondirectory (directory-file-name (file-name-directory dir))))
            (t base)))))

(defun agent-hq--truename (dir)
  "Return DIR as an absolute, symlink-free directory name."
  (file-name-as-directory (file-truename (expand-file-name dir))))

(defun agent-hq-context (&optional dir)
  "Return the git context of DIR (default `default-directory').
Returns nil outside a git worktree and for remote directories."
  (let ((dir (expand-file-name (or dir default-directory))))
    (unless (file-remote-p dir)
      (when-let* ((found (locate-dominating-file dir ".git"))
                  (root (file-name-as-directory (expand-file-name found)))
                  (git-dir (agent-hq--git-dir root)))
        (let* ((common (agent-hq--common-dir git-dir))
               (repo (agent-hq--repo-name common)))
          (agent-hq--make-context
           :repo repo
           :worktree (agent-hq--worktree-label repo root (agent-hq--main-root common))
           :branch (agent-hq--branch git-dir)
           :root root
           :common-dir common))))))

(defun agent-hq-context-label (ctx)
  "Return \"repo:worktree\" for CTX, or just \"repo\" for the main worktree."
  (if-let* ((worktree (agent-hq-context-worktree ctx)))
      (format "%s:%s" (agent-hq-context-repo ctx) worktree)
    (agent-hq-context-repo ctx)))

(defun agent-hq--repo-worktrees (common-dir)
  "Return a context for each worktree of the repo at COMMON-DIR, main first.
Worktrees whose directory is gone are left out."
  (let* ((repo (agent-hq--repo-name common-dir))
         (main (agent-hq--main-root common-dir))
         (admin-root (expand-file-name "worktrees" common-dir))
         (linked
          (when (file-directory-p admin-root)
            (delq nil
                  (mapcar
                   (lambda (name)
                     (let* ((admin (file-name-as-directory (expand-file-name name admin-root)))
                            (gitfile (agent-hq--read-first-line
                                      (expand-file-name "gitdir" admin)))
                            (root (and gitfile
                                       (file-name-directory (expand-file-name gitfile admin)))))
                       (when (and root (file-directory-p root))
                         (agent-hq--make-context
                          :repo repo
                          :worktree (agent-hq--worktree-label repo root main)
                          :branch (agent-hq--branch admin)
                          :root root
                          :common-dir common-dir))))
                   (directory-files admin-root nil "\\`[^.]"))))))
    (append (when (and main (file-directory-p main))
              (list (agent-hq--make-context
                     :repo repo :worktree nil
                     :branch (agent-hq--branch common-dir)
                     :root main :common-dir common-dir)))
            (sort linked (lambda (a b) (string< (agent-hq-context-worktree a)
                                                (agent-hq-context-worktree b)))))))

;;;; Claude accounts

(defun agent-hq--repo-root (dir)
  "Return the main worktree root of DIR's repo, or DIR itself outside git.
A bare repo's worktrees fall back to their own root."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (if-let* ((ctx (agent-hq-context dir)))
        (or (agent-hq--main-root (agent-hq-context-common-dir ctx))
            (agent-hq-context-root ctx))
      dir)))

(defun agent-hq-claude-config-dir (&optional dir)
  "Return the CLAUDE_CONFIG_DIR for Claude shells in DIR, or nil for ~/.claude.
DIR defaults to `default-directory'.  See `agent-hq-claude-config-dirs'."
  (when agent-hq-claude-config-dirs
    (let ((root (agent-hq--repo-root (or dir default-directory))))
      (when-let* ((entry (seq-find (lambda (entry) (string-match-p (car entry) root))
                                   agent-hq-claude-config-dirs)))
        (file-name-as-directory (expand-file-name (cdr entry)))))))

(defun agent-hq--account-name (config-dir)
  "Return a short name for CONFIG-DIR: ~/.claude-work gives \"work\"."
  (let ((base (file-name-nondirectory (directory-file-name config-dir))))
    (if (string-match "\\`\\.?claude[-_.]+\\(.+\\)\\'" base)
        (match-string 1 base)
      base)))

(defun agent-hq-claude-account (&optional dir)
  "Return a short name for the Claude account used in DIR, or nil for ~/.claude.
The name comes from the config dir: ~/.claude-work gives \"work\"."
  (when-let* ((config (agent-hq-claude-config-dir dir)))
    (agent-hq--account-name config)))

(defun agent-hq--claude-project-key (dir)
  "Return the name Claude Code gives DIR's folder under projects/.
Every character but ASCII letters and digits becomes \"-\".  Returns nil
for paths so long that Claude Code shortens them with a hash instead."
  (let ((key (replace-regexp-in-string
              "[^a-zA-Z0-9]" "-"
              (directory-file-name (file-truename (expand-file-name dir))))))
    (when (<= (length key) 200)
      key)))

(defun agent-hq-claude-shared-memory-dir (&optional dir)
  "Return the auto memory dir that ~/.claude keeps for DIR's repo, or nil.
Claude Code keeps one memory per repo, under its main worktree's root."
  (when-let* ((key (agent-hq--claude-project-key
                    (agent-hq--repo-root (or dir default-directory)))))
    (expand-file-name (concat "projects/" key "/memory")
                      (or (getenv "CLAUDE_CONFIG_DIR") "~/.claude"))))

(defun agent-hq--read-json (file)
  "Return FILE's JSON as an alist; an empty FILE gives nil.
JSON is UTF-8: read so, without coding detection (see
`agent-hq--read-first-line')."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (unless (string-blank-p (buffer-string))
      (json-parse-buffer :object-type 'alist :null-object :null :false-object :false))))

(defun agent-hq--write-json (file data)
  "Write DATA, an alist from `agent-hq--read-json', to FILE as indented JSON."
  (require 'json)
  (with-temp-buffer
    (insert (json-serialize data :null-object :null :false-object :false))
    (json-pretty-print-buffer)
    (goto-char (point-max))
    (insert "\n")
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region nil nil file nil 'silent))))

(defun agent-hq--git-ignored-p (root file)
  "Return non-nil if git ignores FILE, relative to the worktree at ROOT."
  (let ((default-directory root))
    (eql 0 (ignore-errors (process-file "git" nil nil nil "check-ignore" "-q" file)))))

(defun agent-hq-claude-share-memory (&optional dir)
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
  (let* ((root (and (agent-hq-context dir)
                    (agent-hq--repo-root (or dir default-directory))))
         (memory (and root (agent-hq-claude-shared-memory-dir root)))
         (rel ".claude/settings.local.json")
         (file (and root (expand-file-name rel root))))
    (when (and memory
               (or (file-exists-p file) (agent-hq--git-ignored-p root rel)))
      (condition-case err
          (let ((settings (and (file-exists-p file) (agent-hq--read-json file))))
            (unless (equal (alist-get 'autoMemoryDirectory settings) memory)
              (make-directory (file-name-directory file) t)
              (agent-hq--write-json
               file (if (assq 'autoMemoryDirectory settings)
                        (cons (cons 'autoMemoryDirectory memory)
                              (assq-delete-all 'autoMemoryDirectory settings))
                      (append settings (list (cons 'autoMemoryDirectory memory))))))
            memory)
        (error (message "agent-hq: left %s alone: %s"
                        (abbreviate-file-name file) (error-message-string err))
               nil)))))

(defvar-local agent-hq--claude-started-with nil
  "The CLAUDE_CONFIG_DIR this shell's Claude client was made with.
`default' when it was left unset; nil before any client was made.")

(defvar agent-hq--claude-config-override nil
  "When non-nil, the Claude config dir for new shells, whatever their repo.
A directory, or `default' for ~/.claude.  Bound while a session found
under one account's config dir is resumed, since only that account
can load it.")

(defvar agent-hq-claude-config-function nil
  "When non-nil, picks the account of new Claude shells.
Called with the config dir that `agent-hq-claude-config-dirs' gives the
shell's repo, or `default' for ~/.claude, it returns the one to use.
Not called for a session resumed on the account that stores it.
`agent-hq-fallback-setup' sets it, to start shells on the fallback
account while their own is at its limit.")

(defun agent-hq--new-claude-config ()
  "Return the config dir a Claude client made here now gets, or `default'."
  (or agent-hq--claude-config-override
      (let ((config (or (agent-hq-claude-config-dir) 'default)))
        (if agent-hq-claude-config-function
            (funcall agent-hq-claude-config-function config)
          config))))

(defun agent-hq--config-account (config)
  "Return the account name of CONFIG, a config dir or `default'."
  (unless (eq config 'default)
    (agent-hq--account-name config)))

(defun agent-hq--claude-account-here ()
  "Return the account name of the Claude shell in this buffer, or nil.
Once the shell has a client this is the account it was started with, so
editing `agent-hq-claude-config-dirs' doesn't relabel running shells."
  (agent-hq--config-account (or agent-hq--claude-started-with
                                 (agent-hq--new-claude-config))))

;;;###autoload
(defun agent-hq-claude-environment-a (fn &rest args)
  "Run FN, a Claude client maker, with the account its shell's repo uses.
Around advice for `agent-shell-anthropic-make-claude-client': adds
CLAUDE_CONFIG_DIR for repos in `agent-hq-claude-config-dirs' and leaves
it out elsewhere.  ARGS holds :buffer, the shell the client is for.
With `agent-hq-claude-shared-memory', such repos also keep ~/.claude's
project memory; see `agent-hq-claude-share-memory'."
  (let* ((buffer (or (plist-get args :buffer) (current-buffer)))
         (config (with-current-buffer buffer
                   (setq agent-hq--claude-started-with (agent-hq--new-claude-config)))))
    (when (and agent-hq-claude-shared-memory (not (eq config 'default)))
      (with-current-buffer buffer
        (agent-hq-claude-share-memory)))
    (if (eq config 'default)
        (apply fn args)
      (let ((agent-shell-anthropic-claude-environment
             (cons (concat "CLAUDE_CONFIG_DIR=" (directory-file-name config))
                   agent-shell-anthropic-claude-environment)))
        (apply fn args)))))

;;;; Shell names

(defun agent-hq--agent-display-name (agent-name account)
  "Return AGENT-NAME, plus ACCOUNT when Claude runs on a non-default one.
For example \"Claude (work)\" for a repo mapped to ~/.claude-work."
  (if (and account (string-prefix-p "Claude" agent-name))
      (format "%s (%s)" agent-name account)
    agent-name))

(defun agent-hq--format-name (agent-name account ctx fallback &optional title)
  "Return a shell name for AGENT-NAME on ACCOUNT in CTX, or FALLBACK outside git.
The session TITLE, if any, goes last, cut to `agent-hq-title-width'."
  (concat (agent-hq--agent-display-name agent-name account) " @ "
          (if ctx
              (concat (agent-hq-context-label ctx)
                      (when-let* ((branch (agent-hq-context-branch ctx)))
                        (concat agent-hq-branch-separator branch)))
            fallback)
          (when (and title agent-hq-title-width (not (string-empty-p title)))
            (concat agent-hq-title-separator
                    (truncate-string-to-width title agent-hq-title-width nil nil "…")))))

;;;###autoload
(defun agent-hq-buffer-name (agent-name project-name)
  "Name a new shell after AGENT-NAME and its repo, worktree and branch.
For `agent-shell-buffer-name-format'.  Uses PROJECT-NAME outside git."
  ;; The account comes from the directory: the current buffer may be another
  ;; shell, on another account.
  (agent-hq--format-name agent-name (agent-hq--config-account (agent-hq--new-claude-config))
                          (agent-hq-context) project-name))

(defun agent-hq--strip-uniquifier (name)
  "Return NAME without a trailing <N> that `rename-buffer' adds."
  (replace-regexp-in-string "<[0-9]+>\\'" "" name))

(defun agent-hq--agent-name (&optional buffer)
  "Return the agent's display name (\"Claude\", \"Codex\") in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (ignore-errors (map-nested-elt agent-shell--state '(:agent-config :buffer-name)))
        "Agent")))

(defun agent-hq--fallback-name ()
  "Return what a shell outside git is named after: its project or directory.
The project name is the one agent-shell named the shell after."
  (or (ignore-errors (agent-shell--project-name))
      (file-name-nondirectory (directory-file-name default-directory))))

(defun agent-hq-refresh-buffer-name (&optional buffer)
  "Rename agent shell BUFFER if its branch or session title changed."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((ctx (agent-hq-context))
               (old (buffer-name))
               (new (agent-hq--format-name (agent-hq--agent-name)
                                            (agent-hq--claude-account-here) ctx
                                            (unless ctx (agent-hq--fallback-name))
                                            (agent-hq--session-title buffer))))
          (unless (string= (agent-hq--strip-uniquifier old) new)
            (let ((viewport (get-buffer (concat old " [viewport]"))))
              (shell-maker-set-buffer-name buffer new)
              (when (buffer-live-p viewport)
                (with-current-buffer viewport
                  (rename-buffer (concat (buffer-name buffer) " [viewport]") t))))))))))

(defvar agent-hq--shells-started 0
  "How many shells this Emacs has started, for `agent-hq--shell-number'.")

(defvar-local agent-hq--shell-number nil
  "When this shell started among this Emacs's shells: 1 for the first.
Shells keep it while their name changes with branch and title, so it
orders them (\\[agent-hq-next-shell]) and finds them (`agent-hq-notify').")

(defun agent-hq--shell-id (buffer)
  "Return an id for shell BUFFER that no other shell, now or later, has."
  (when-let* ((number (buffer-local-value 'agent-hq--shell-number buffer)))
    (format "%d.%d" (emacs-pid) number)))

;;;###autoload
(defun agent-hq-setup-shell-h ()
  "Rename this shell whenever a turn ends on another branch or its title changes.
Also links the worktree's `agent-hq-worktree-shared-files', before the
agent starts, and puts point at the prompt whenever the shell comes on
screen (`agent-hq--shown-h').  For `agent-shell-mode-hook'."
  (setq agent-hq--shell-number (cl-incf agent-hq--shells-started))
  (add-hook 'window-buffer-change-functions #'agent-hq--shown-h nil t)
  (with-demoted-errors "agent-hq: %S"
    (agent-hq-link-shared-files))
  (let ((buffer (current-buffer)))
    (dolist (event '(turn-complete session-title-changed))
      (agent-shell-subscribe-to
       :shell-buffer buffer
       :event event
       :on-event (lambda (_event)
                   (with-demoted-errors "agent-hq: %S"
                     (agent-hq-refresh-buffer-name buffer)))))))

;;;; Finding and showing shells

(defun agent-hq--shells ()
  "Return all live agent shell buffers."
  (when (featurep 'agent-shell)
    (seq-filter #'buffer-live-p (agent-shell-buffers))))

(defun agent-hq--activity (buffer)
  "Return BUFFER's last activity as a float time, 0 if none yet."
  (if-let* ((time (ignore-errors (agent-shell-last-activity-time :shell-buffer buffer))))
      (float-time time)
    0))

(defun agent-hq--shells-in (root)
  "Return the shells working in ROOT, most recently active first."
  (let ((root (agent-hq--truename root)))
    (sort (seq-filter (lambda (buffer)
                        (string= (agent-hq--truename
                                  (buffer-local-value 'default-directory buffer))
                                 root))
                      (agent-hq--shells))
          (lambda (a b) (> (agent-hq--activity a) (agent-hq--activity b))))))

(defvar-local agent-hq-shell-asleep nil
  "Non-nil in a shell whose agent hasn't started yet: it starts once shown.
agent-hq-restore leaves the shells it restores so.")

(defun agent-hq--status (buffer)
  "Return BUFFER's status.
That is `blocked', `busy', `ready', `starting', `asleep' (see
`agent-hq-shell-asleep') or `unknown'."
  (condition-case nil
      (cond ((buffer-local-value 'agent-hq-shell-asleep buffer) 'asleep)
            ((agent-shell-initialized-p :shell-buffer buffer)
             (agent-shell-status :shell-buffer buffer))
            (t 'starting))
    (error 'unknown)))

(defun agent-hq--session-title (buffer)
  "Return the session title of BUFFER's agent, or nil."
  (when-let* ((title (ignore-errors
                       (map-nested-elt (buffer-local-value 'agent-shell--state buffer)
                                       '(:session :title))))
              ((stringp title)))
    (car (split-string title "\n" t))))

(defun agent-hq--current-root ()
  "Return the root of the worktree a new shell here would work in."
  (file-name-as-directory
   (or (and (derived-mode-p 'agent-shell-mode) default-directory)
       (and (require 'agent-shell nil t)
            (ignore-errors (agent-shell-cwd)))
       default-directory)))

(defun agent-hq-switch-to-workspace-of (buffer)
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
(defun agent-hq-visit-shell (buffer)
  "Show agent shell BUFFER, switching to the workspace that holds it.
Point goes where you act next (see `agent-hq--goto-prompt'), also when
BUFFER is already on screen."
  (agent-hq-switch-to-workspace-of buffer)
  (if-let* ((window (get-buffer-window buffer)))
      (progn (select-window window)
             (agent-hq--goto-prompt window))
    (pop-to-buffer buffer (bound-and-true-p agent-shell-display-action))))

(defvar-local agent-hq-keep-point nil
  "A window to show this shell in next with point left where it was.
`agent-hq--shown-h' takes it from there once.  For a command that goes
back to a place in the shell, as closing a code block's copy does.")

(defun agent-hq--goto-prompt (window)
  "Move point in WINDOW, which shows a shell, to where you act next.
That is a question or permission request waiting on you, else the end:
the prompt, or during a turn its newest output, which point at the end
follows.  Jumping to a question or request moves point in every window
showing the shell, as agent-shell does when one arrives."
  (with-current-buffer (window-buffer window)
    (or (and (fboundp 'agent-shell-elicitation--pending-p)
             (agent-shell-elicitation--pending-p)
             (agent-shell-elicitation-jump-to-latest-form))
        (and (fboundp 'agent-shell-jump-to-latest-permission-button-row)
             (agent-shell-jump-to-latest-permission-button-row))
        (set-window-point window (point-max)))))

(defun agent-hq--shown-h (window)
  "Move point to the prompt of this shell, which WINDOW has just come to show.
Else point goes back where it was when WINDOW last showed the shell,
above all the output since, and the shell stops following its output.
See `agent-hq--goto-prompt', and `agent-hq-keep-point' to stay put.
For a shell's `window-buffer-change-functions', which run with the shell
current, and since Emacs 31 also once it has left WINDOW for another."
  (when (eq (window-buffer window) (current-buffer))
    (if (eq agent-hq-keep-point window)
        (setq agent-hq-keep-point nil)
      (agent-hq--goto-prompt window))))

;;;###autoload
(defun agent-hq-visit-shell-named (name &optional id)
  "Visit the agent shell with ID, else the one named NAME, and raise its frame.
Notifications call this through emacsclient when clicked.  ID (see
`agent-hq--shell-id') still finds a shell renamed since."
  (if-let* ((buffer (or (and id (seq-find (lambda (shell)
                                            (equal (agent-hq--shell-id shell) id))
                                          (agent-hq--shells)))
                        (get-buffer name))))
      (progn (agent-hq-visit-shell buffer)
             (select-frame-set-input-focus (selected-frame)))
    (message "Agent shell %s is gone" name)))

(defun agent-hq--agent-window-p (window)
  "Return non-nil if WINDOW shows an agent shell or what stands in for one.
That is its viewport or a subagent's transcript, which go back to the
shell in the same window."
  (provided-mode-derived-p (buffer-local-value 'major-mode (window-buffer window))
                           '(agent-shell-mode
                             agent-shell-viewport-view-mode
                             agent-shell-viewport-edit-mode
                             agent-hq-subagents-view-mode)))

(defun agent-hq--agent-window ()
  "Return the agent window of this frame a shell should go to, or nil.
The selected window when it is one, else the one used most recently."
  (if (agent-hq--agent-window-p (selected-window))
      (selected-window)
    (car (sort (seq-filter #'agent-hq--agent-window-p (window-list nil 'nomini))
               (lambda (a b) (> (window-use-time a) (window-use-time b)))))))

;;;###autoload
(defun agent-hq-display-in-agent-window (buffer alist)
  "Display BUFFER in a window that already shows an agent shell.
A `display-buffer' action function: switching between shells reuses
the agent window instead of splitting another one.  With shells side
by side, that is the one you are in, or else the one you were in last.
ALIST is the action's alist."
  (when-let* ((window (agent-hq--agent-window)))
    (window--display-buffer buffer window 'reuse alist)))

(defun agent-hq--display-beside (buffer alist)
  "Display BUFFER in a new window split off the agent window.
Sideways when both halves keep `agent-hq-beside-min-width' columns,
else below.  Return the window, or nil when there's no agent window or
no room.  ALIST is the action's alist."
  (when-let* ((window (agent-hq--agent-window))
              (new (ignore-errors
                     (split-window window nil
                                   (if (>= (window-total-width window)
                                           (* 2 agent-hq-beside-min-width))
                                       'right
                                     'below)))))
    (window--display-buffer buffer new 'window alist)))

;;;###autoload
(defun agent-hq-beside ()
  "Make the next command show its agent shell beside the one on screen.
A prefix, like \\[other-window-prefix]: follow it with any command that
shows a shell, new or existing (\\[agent-shell-anthropic-start-claude-code], \
\\[agent-hq-switch-shell], \\[agent-hq-resume-session]...).
The agent window splits in two, sideways if it is wide enough (see
`agent-hq-beside-min-width'), and the shell goes in the new half.  A
shell already on screen is just selected.  Quitting or killing the
shell closes its half again.

Without this, a shell replaces the one in the agent window you are in,
or the one you were in last; split one yourself to choose the place."
  (interactive)
  (let* ((done nil)
         (action (lambda (buffer alist)
                   (unless done
                     (when (provided-mode-derived-p
                            (buffer-local-value 'major-mode buffer)
                            '(agent-shell-mode agent-shell-viewport-view-mode
                              agent-shell-viewport-edit-mode))
                       (setq done (agent-hq--display-beside buffer alist))))))
         (depth (minibuffer-depth))
         (command this-command)
         (cleanup (make-symbol "agent-hq--beside-cleanup")))
    ;; Only shells: unlike `display-buffer-override-next-command', which
    ;; which-key's popup would use up while you type the next keys.  Gone
    ;; once the next command is done.  A shell it shows later, once you
    ;; pick a session, still goes beside: agent-shell keeps the override
    ;; for that.
    (fset cleanup
          (lambda ()
            (unless (or (> (minibuffer-depth) depth) (eq this-command command))
              (setcar display-buffer-overriding-action
                      (delq action (car display-buffer-overriding-action)))
              (remove-hook 'post-command-hook cleanup))))
    (push action (car display-buffer-overriding-action))
    (add-hook 'post-command-hook cleanup)
    (message "Next agent shell opens beside")))

(defun agent-hq--start-shell (root &optional choose-agent)
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

(defun agent-hq--col (text width)
  "Return TEXT cut or padded to WIDTH columns, plus a separating space."
  (concat (truncate-string-to-width text width nil ?\s "…") " "))

(defun agent-hq--status-text (status)
  "Return STATUS as a short, colored word."
  (propertize (symbol-name status)
              'face (pcase status
                      ('blocked 'error)
                      ('busy 'warning)
                      ('ready 'success)
                      (_ 'shadow))))

(defun agent-hq--read (prompt candidates)
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
                        `(metadata (category . agent-hq)
                                   (group-function . ,group)
                                   (display-sort-function . identity)
                                   (cycle-sort-function . identity))
                      (complete-with-action action strings input predicate)))
                  nil t))
         (entry (gethash (substring-no-properties choice) table)))
    (unless entry
      (user-error "Nothing selected"))
    (cdr entry)))

(defun agent-hq--status-rank (status)
  "Sort key for STATUS: shells waiting on you first."
  (pcase status ('blocked 0) ('ready 1) ('busy 2) (_ 3)))

(defun agent-hq--shell-label (buffer ctx)
  "Return where shell BUFFER works: CTX's repo:worktree, else its directory."
  (if ctx
      (agent-hq-context-label ctx)
    (file-name-nondirectory
     (directory-file-name (buffer-local-value 'default-directory buffer)))))

(defun agent-hq--pick-shell (prompt shells group &optional first)
  "Read one of SHELLS with PROMPT and return it.
Each line shows status, repo:worktree, branch, session title and agent,
so typing part of a branch, ticket or title finds the shell.  Shells
waiting on you come first, then the most recently active.  GROUP, called
with a shell's buffer and git context, names its group.  Shells FIRST
returns non-nil for, called the same way, come before all others."
  (let* ((entries
          (mapcar
           (lambda (buffer)
             (agent-hq-refresh-buffer-name buffer)
             (let ((ctx (with-current-buffer buffer (agent-hq-context))))
               (list :buffer buffer :ctx ctx
                     :first (and first (funcall first buffer ctx) t)
                     :status (agent-hq--status buffer)
                     :time (agent-hq--activity buffer))))
           shells))
         (sorted (sort entries
                       (lambda (a b)
                         (let ((ra (agent-hq--status-rank (plist-get a :status)))
                               (rb (agent-hq--status-rank (plist-get b :status))))
                           (cond ((not (eq (plist-get a :first) (plist-get b :first)))
                                  (plist-get a :first))
                                 ((= ra rb) (> (plist-get a :time) (plist-get b :time)))
                                 (t (< ra rb))))))))
    (agent-hq--read
     prompt
     (mapcar
      (lambda (entry)
        (let* ((buffer (plist-get entry :buffer))
               (ctx (plist-get entry :ctx))
               (branch (or (and ctx (agent-hq-context-branch ctx)) "")))
          (cons (concat
                 (agent-hq--col (agent-hq--status-text (plist-get entry :status)) 8)
                 (agent-hq--col (propertize (agent-hq--shell-label buffer ctx)
                                             'face 'font-lock-keyword-face)
                                 30)
                 (agent-hq--col (propertize branch 'face 'font-lock-constant-face) 48)
                 (propertize (or (agent-hq--session-title buffer) "") 'face 'font-lock-doc-face)
                 "  " (propertize (format "[%s]" (with-current-buffer buffer
                                                   (agent-hq--agent-display-name
                                                    (agent-hq--agent-name)
                                                    (agent-hq--claude-account-here))))
                                  'face 'shadow))
                (cons (funcall group buffer ctx) buffer))))
      sorted))))

;;;###autoload
(defun agent-hq-switch-shell ()
  "Switch to any agent shell, across all projects and worktrees.
Each line shows status, repo:worktree, branch, session title and agent,
so typing part of a branch, ticket or title finds the shell."
  (interactive)
  (agent-hq-visit-shell
   (agent-hq--pick-shell "Agent shell: "
                          (or (agent-hq--shells) (user-error "No agent shells running"))
                          (lambda (_ ctx) (if ctx (agent-hq-context-repo ctx) "other")))))

(defun agent-hq--repo-shells (&optional dir)
  "Return the shells working in any worktree of DIR's repo, in a fixed order.
DIR defaults to where a shell here would work; outside git, only shells
working in DIR itself count.  The main worktree's shells come first, then
each other worktree's by name, and within a worktree shells go in the
order they started, so the order stays put as shells get busy and go
idle, and as their names change with branch and title."
  (let* ((dir (or dir (agent-hq--current-root)))
         (ctx (agent-hq-context dir))
         (common (and ctx (agent-hq--truename (agent-hq-context-common-dir ctx))))
         (keyed
          (delq nil
                (mapcar
                 (lambda (buffer)
                   (let ((other (with-current-buffer buffer (agent-hq-context))))
                     (when (if common
                               (and other
                                    (string= (agent-hq--truename
                                              (agent-hq-context-common-dir other))
                                             common))
                             (string= (agent-hq--truename
                                       (buffer-local-value 'default-directory buffer))
                                      (agent-hq--truename dir)))
                       (let ((worktree (and other (agent-hq-context-worktree other))))
                         (cons (concat (if worktree "1" "0") worktree "\0"
                                       (if-let* ((number (buffer-local-value
                                                          'agent-hq--shell-number buffer)))
                                           (format "%09d" number)
                                         "~")
                                       "\0" (buffer-name buffer))
                               buffer)))))
                 (agent-hq--shells)))))
    (mapcar #'cdr (sort keyed (lambda (a b) (string< (car a) (car b)))))))

(defun agent-hq--repo-label (&optional dir)
  "Return the name of DIR's repo, or DIR itself outside git."
  (let ((dir (or dir (agent-hq--current-root))))
    (if-let* ((ctx (agent-hq-context dir)))
        (agent-hq-context-repo ctx)
      (abbreviate-file-name dir))))

;;;###autoload
(defun agent-hq-switch-repo-shell ()
  "Switch to an agent shell of this repo, in any of its worktrees.
Lines are those of `agent-hq-switch-shell', grouped by worktree, with
this worktree's shells first."
  (interactive)
  (let* ((root (agent-hq--current-root))
         (ctx (agent-hq-context root))
         (here (agent-hq--truename (if ctx (agent-hq-context-root ctx) root))))
    (agent-hq-visit-shell
     (agent-hq--pick-shell
      (format "Agent shell (%s): " (agent-hq--repo-label root))
      (or (agent-hq--repo-shells root)
          (user-error "No agent shells in %s" (agent-hq--repo-label root)))
      #'agent-hq--shell-label
      (lambda (buffer shell-ctx)
        (string= (agent-hq--truename (if shell-ctx
                                          (agent-hq-context-root shell-ctx)
                                        (buffer-local-value 'default-directory buffer)))
                 here))))))

(defun agent-hq--cycle-shell (n)
  "Show the shell N places after this one among this repo's shells.
Negative N goes back.  The order is `agent-hq--repo-shells', and wraps.
From a shell, shells that other windows show are skipped: with shells
side by side, each window steps through the rest in place.
Run from evil's normal state, the shell shown is put in normal state too,
so the same key moves on again instead of typing into its prompt."
  (let* ((normal (eq (bound-and-true-p evil-state) 'normal))
         (root (agent-hq--current-root))
         (all (or (agent-hq--repo-shells root)
                  (user-error "No agent shells in %s" (agent-hq--repo-label root))))
         (shells (if (derived-mode-p 'agent-shell-mode)
                     (seq-remove (lambda (shell)
                                   (and (not (eq shell (current-buffer)))
                                        (get-buffer-window shell)))
                                 all)
                   all))
         (pos (seq-position shells (current-buffer)))
         ;; From outside a shell, the first step lands on the first or last.
         (index (mod (cond (pos (+ pos n)) ((> n 0) (1- n)) (t n))
                     (length shells)))
         (shell (nth index shells)))
    (when (eq shell (current-buffer))
      (user-error (if (cdr all)
                      "The other agent shells of %s are on screen"
                    "No other agent shell in %s")
                  (agent-hq--repo-label root)))
    (agent-hq-visit-shell shell)
    (when (and normal (fboundp 'evil-normal-state))
      (with-current-buffer shell
        (evil-normal-state)))
    (message "Shell %d/%d: %s" (1+ (seq-position all shell)) (length all)
             (buffer-name shell))))

;;;###autoload
(defun agent-hq-next-shell (&optional n)
  "Show the next agent shell of this repo, across its worktrees.
With N, move N shells.  The last shell is followed by the first."
  (interactive "p")
  (agent-hq--cycle-shell (or n 1)))

;;;###autoload
(defun agent-hq-previous-shell (&optional n)
  "Show the previous agent shell of this repo, across its worktrees.
With N, move back N shells.  The first shell is preceded by the last."
  (interactive "p")
  (agent-hq--cycle-shell (- (or n 1))))

(defun agent-hq--projectile-projects ()
  "Return projectile's known projects, loading projectile's list if needed.
Doom loads projectile lazily, so early in a session the list may not be
read yet."
  (when (require 'projectile nil t)
    (when (and (null projectile-known-projects)
               (fboundp 'projectile-load-known-projects))
      (projectile-load-known-projects))
    projectile-known-projects))

(defun agent-hq--known-common-dirs (&optional first)
  "Return the common dirs of known repos, the repo of directory FIRST first.
Known repos are those of live shells and of projectile's known projects."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (dir (append (list first)
                         (mapcar (lambda (buffer) (buffer-local-value 'default-directory buffer))
                                 (agent-hq--shells))
                         (agent-hq--projectile-projects)))
      (when-let* (((stringp dir))
                  (dir (expand-file-name dir))
                  ((not (file-remote-p dir)))
                  ((file-directory-p dir))
                  (ctx (agent-hq-context dir))
                  (common (agent-hq-context-common-dir ctx))
                  (key (agent-hq--truename common))
                  ((not (gethash key seen))))
        (puthash key t seen)
        (push common result)))
    (nreverse result)))

(defun agent-hq--all-worktrees ()
  "Return contexts for every worktree of every known repo, current repo first."
  (mapcan #'agent-hq--repo-worktrees
          (agent-hq--known-common-dirs (agent-hq--current-root))))

(defun agent-hq--shells-summary (shells)
  "Describe SHELLS, e.g. \"2 shells: busy ready\"."
  (if (null shells)
      ""
    (propertize (format "%d shell%s: %s" (length shells)
                        (if (cdr shells) "s" "")
                        (mapconcat (lambda (b) (symbol-name (agent-hq--status b)))
                                   shells " "))
                'face 'success)))

;;;###autoload
(defun agent-hq-switch-worktree (&optional new-shell)
  "Go to a worktree of any known repo: its workspace and its agent shell.
Lines show repo:worktree, branch, running shells and path.  Reuses the
worktree's most recent shell, or starts one.  With prefix argument
NEW-SHELL, always start a new shell."
  (interactive "P")
  (let ((worktrees (or (agent-hq--all-worktrees) (user-error "No git repositories known"))))
    (agent-hq-open-worktree
     (agent-hq--read
      "Worktree: "
      (mapcar (lambda (ctx)
                (cons (agent-hq--worktree-line ctx)
                      (cons (agent-hq-context-repo ctx) (agent-hq-context-root ctx))))
              worktrees))
     new-shell)))

;;;###autoload
(defun agent-hq-open-worktree (root &optional new-shell)
  "Go to the worktree at ROOT: switch to its project, then show its shell.
Reuses the most recently active shell there unless NEW-SHELL."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (when agent-hq-switch-project-function
      (funcall agent-hq-switch-project-function root))
    (if-let* (((not new-shell))
              (shell (car (agent-hq--shells-in root))))
        (agent-hq-visit-shell shell)
      (agent-hq--start-shell root))))

;;;###autoload
(defun agent-hq-toggle ()
  "Show or hide this worktree's agent shell, starting one if there is none.
Unlike `agent-shell-toggle', never falls back to another project's shell."
  (interactive)
  (if (derived-mode-p 'agent-shell-mode)
      (quit-window)
    (let ((root (agent-hq--current-root)))
      (if-let* ((shell (car (agent-hq--shells-in root))))
          (if-let* ((window (get-buffer-window shell)))
              (quit-window nil window)
            (agent-hq-visit-shell shell))
        (agent-hq--start-shell root)))))

(defun agent-hq--kill-shell-buffer (buffer)
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

(defun agent-hq--kill-reasons (buffer)
  "Return what killing agent shell BUFFER would interrupt, as phrases."
  (let ((queued (length (ignore-errors
                          (map-elt (buffer-local-value 'agent-shell--state buffer)
                                   :pending-prompts)))))
    (delq nil (list (pcase (agent-hq--status buffer)
                      ('busy "is working")
                      ('blocked "is waiting on you"))
                    (when (> queued 0)
                      (format "has %s queued" (agent-hq--count queued "prompt")))))))

;;;###autoload
(defun agent-hq-kill-shell (&optional buffer)
  "Kill agent shell BUFFER (default: this one) and its agent.
Asks first only while the agent is working, waits on you, or has
prompts queued.  The window goes back to what it showed before, or
closes if it opened for the shell.  The session itself stays: /resume
or \\[agent-hq-resume-session] brings it back."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (unless (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                     'agent-shell-mode)
      (user-error "Not in an agent shell"))
    (when-let* ((reasons (agent-hq--kill-reasons buffer))
                ((not (y-or-n-p (format "%s %s; kill it? " (buffer-name buffer)
                                        (string-join reasons " and "))))))
      (user-error "Kept %s" (buffer-name buffer)))
    (agent-hq--kill-shell-buffer buffer)))

;;;###autoload
(defun agent-hq-new-shell ()
  "Start another shell in this worktree, choosing the agent."
  (interactive)
  (agent-hq--start-shell (agent-hq--current-root) t))

;;;###autoload
(defun agent-hq-resume-session ()
  "Pick a past session of this repo and resume it in a new shell here."
  (interactive)
  (let ((agent-shell-session-strategy 'prompt))
    (agent-hq--start-shell (agent-hq--current-root))))

;;;; Shells in any project

(defun agent-hq--project-candidates ()
  "Return a (STRING GROUP . ROOT) candidate for each known project.
Projects are projectile's, most recent first; remote and missing ones
are left out.  Lines show repo:worktree, branch, running shells and path."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (dir (agent-hq--projectile-projects))
      (when-let* (((stringp dir))
                  (root (file-name-as-directory (expand-file-name dir)))
                  ((not (file-remote-p root)))
                  ((file-directory-p root))
                  (key (agent-hq--truename root))
                  ((not (gethash key seen))))
        (puthash key t seen)
        (let ((ctx (agent-hq-context root)))
          (push (cons (concat
                       (agent-hq--col (propertize (if ctx
                                                       (agent-hq-context-label ctx)
                                                     (file-name-nondirectory
                                                      (directory-file-name root)))
                                                   'face 'font-lock-keyword-face)
                                       30)
                       (agent-hq--col (propertize (or (and ctx (agent-hq-context-branch ctx)) "")
                                                   'face 'font-lock-constant-face)
                                       48)
                       (agent-hq--col (agent-hq--shells-summary (agent-hq--shells-in root)) 22)
                       (propertize (abbreviate-file-name root) 'face 'shadow))
                      (cons (if ctx (agent-hq-context-repo ctx) "other") root))
                result))))
    (nreverse result)))

(defun agent-hq--start-in-project (start prompt)
  "Read a known project with PROMPT, go to its workspace, then call START there.
START is a command that starts a new shell in `default-directory'."
  (let ((root (agent-hq--read prompt (or (agent-hq--project-candidates)
                                          (user-error "No known projects")))))
    (when agent-hq-switch-project-function
      (funcall agent-hq-switch-project-function root))
    (let ((default-directory root))
      (funcall start))))

;;;###autoload
(defun agent-hq-new-claude-in-project ()
  "Pick any known project, go to its workspace and start a Claude shell there.
Works before any project is open, such as right after Emacs starts."
  (interactive)
  (agent-hq--start-in-project #'agent-shell-anthropic-start-claude-code
                               "New Claude shell in project: "))

;;;###autoload
(defun agent-hq-new-codex-in-project ()
  "Pick any known project, go to its workspace and start a Codex shell there.
Works before any project is open, such as right after Emacs starts."
  (interactive)
  (agent-hq--start-in-project #'agent-shell-openai-start-codex
                               "New Codex shell in project: "))

;;;; /resume in a shell

(defun agent-hq--resume-query (command)
  "Return the text after /resume when COMMAND is /resume, \"\" for none.
Returns nil for any other COMMAND."
  (when (and (stringp command)
             (string-match "\\`[ \t\n]*/resume\\(?:[ \t]+\\(.*?\\)\\)?[ \t\n]*\\'" command))
    (or (match-string 1 command) "")))

(defun agent-hq--session-shell (session-id &optional except)
  "Return the live shell other than EXCEPT whose session is SESSION-ID, or nil."
  (seq-find (lambda (shell)
              (and (not (eq shell except))
                   (equal (ignore-errors
                            (map-nested-elt (buffer-local-value 'agent-shell--state shell)
                                            '(:session :id)))
                          session-id)))
            (agent-hq--shells)))

(defun agent-hq--session-choices (sessions &optional except)
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
                            (when (agent-hq--session-shell (map-elt session 'sessionId) except)
                              (propertize "  [open in a shell]" 'face 'warning)))
                    session))
            sessions)))

(defun agent-hq--resume-pick (buffer current sessions query)
  "Pick one of SESSIONS but CURRENT, starting from QUERY; resume it in BUFFER."
  (when (buffer-live-p buffer)
    (let ((sessions (seq-remove (lambda (session)
                                  (equal (map-elt session 'sessionId) current))
                                (agent-shell--sort-sessions-by-recency sessions))))
      (if (null sessions)
          (message "No other sessions for %s"
                   (abbreviate-file-name (buffer-local-value 'default-directory buffer)))
        (let* ((choices (agent-hq--session-choices sessions buffer))
               (choice (completing-read
                        "Resume session: "
                        (lambda (string predicate action)
                          (if (eq action 'metadata)
                              '(metadata (display-sort-function . identity)
                                         (cycle-sort-function . identity))
                            (complete-with-action action choices string predicate)))
                        nil t (unless (string-empty-p query) query)))
               (session (cdr (assoc choice choices)))
               (open (and session (agent-hq--session-shell
                                   (map-elt session 'sessionId) buffer))))
          (cond (open (agent-hq-visit-shell open)
                      (message "That session is open in %s" (buffer-name open)))
                (session
                 (with-current-buffer buffer
                   (agent-shell-restart :session-id (map-elt session 'sessionId))))))))))

;;;###autoload
(defun agent-hq-resume-in-shell (&optional buffer query)
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
                       (run-at-time 0 nil #'agent-hq--resume-pick
                                    buffer current sessions query))
         :on-failure (lambda (error &rest _)
                       (message "Couldn't list sessions: %s"
                                (or (ignore-errors (map-elt error 'message)) error))))))))

;;;###autoload
(defun agent-hq-resume-command-a (fn &rest args)
  "Around advice for `agent-shell--handle' (FN, ARGS): run /resume in Emacs.
Claude Code's /resume only exists in its terminal UI; over ACP it answers
\"isn't available in this environment\".  So a prompt that is just
/resume [QUERY] isn't sent: it ends the turn and opens
`agent-hq-resume-in-shell'.  Anything else, and /resume before the
session is up, goes to FN as usual."
  (let* ((buffer (plist-get args :shell-buffer))
         (query (agent-hq--resume-query (plist-get args :command))))
    (if (and query
             (buffer-live-p buffer)
             (map-nested-elt (buffer-local-value 'agent-shell--state buffer)
                             '(:session :id)))
        (with-current-buffer buffer
          (agent-shell--finish-output :config shell-maker--config :success t)
          (run-at-time 0 nil (lambda ()
                               (condition-case err
                                   (agent-hq-resume-in-shell buffer query)
                                 (user-error (message "%s" (error-message-string err)))))))
      (apply fn args))))

;;;; Sessions of every project

;; Claude Code keeps every session in <config>/projects/<dir key>/<id>.jsonl,
;; whether it ran here, in tmux or in the desktop app, and keeps it after its
;; worktree goes.  Every message line names its cwd and git branch, and
;; Claude Code appends the session's title and last prompt as lines of their
;; own.  `agent-hq-find-session' lists and searches these files, not
;; agent-shell's transcripts, which only shells started here write and which
;; go with their worktree.

(defcustom agent-hq-session-ignored-dirs
  "\\`\\(?:/private\\)?/\\(?:tmp\\|var/folders\\)/"
  "Regexp of session directories that `agent-hq-find-session' leaves out.
The default leaves out temporary directories, where scripts and evals
run Claude.  Nil leaves nothing out."
  :type '(choice (const :tag "None" nil) regexp))

(defvar agent-hq--session-cache (make-hash-table :test #'equal)
  "Session files read so far, as FILE -> (SIZE MTIME . SESSION).
SESSION is nil for a file that names no directory or prompt.")

(defvar agent-hq--find-session-history nil
  "Searches typed in `agent-hq-find-session'.")

(defun agent-hq--claude-default-config ()
  "Return the config dir Claude uses when CLAUDE_CONFIG_DIR is left unset."
  (file-name-as-directory (expand-file-name (or (getenv "CLAUDE_CONFIG_DIR") "~/.claude"))))

(defun agent-hq--claude-configs ()
  "Return every Claude config dir sessions can be in, the default first."
  (delete-dups (cons (agent-hq--claude-default-config)
                     (mapcar (lambda (entry)
                               (file-name-as-directory (expand-file-name (cdr entry))))
                             agent-hq-claude-config-dirs))))

(defun agent-hq--claude-session-files ()
  "Return (FILE SIZE MTIME CONFIG) for every session file of every account.
Subagent sessions, a level further down, are left out."
  (let (files)
    (dolist (config (agent-hq--claude-configs))
      (let ((projects (expand-file-name "projects" config)))
        (when (file-directory-p projects)
          (dolist (dir (directory-files projects t "\\`[^.]" t))
            (when (file-directory-p dir)
              (dolist (entry (directory-files-and-attributes dir t "\\.jsonl\\'" t))
                (let ((attributes (cdr entry)))
                  ;; A nil type is a regular file.
                  (unless (file-attribute-type attributes)
                    (push (list (car entry)
                                (file-attribute-size attributes)
                                (float-time (file-attribute-modification-time attributes))
                                config)
                          files)))))))))
    files))

(defun agent-hq--rg-insert (args files)
  "Insert the output of ripgrep with ARGS over FILES at point.
FILES go in batches, so any number of them fits on a command line."
  (let ((coding-system-for-read 'utf-8-unix))
    (dolist (batch (seq-split files 1000))
      (apply #'call-process "rg" nil t nil
             (append '("--no-config" "--no-messages" "--no-ignore") args (cons "--" batch))))))

(defun agent-hq--rg (args files)
  "Return the output lines of ripgrep with ARGS over FILES."
  (with-temp-buffer
    (agent-hq--rg-insert args files)
    (split-string (buffer-string) "\n" t)))

(defconst agent-hq--session-field-regexp
  (concat "^\\([^\0\n]+\\)\0[^\n]*?"
          "\"\\(cwd\\|gitBranch\\|aiTitle\\|customTitle\\|lastPrompt\\)\":\\(\"[^\n]*\"\\)$")
  "Matches a FILE NUL MATCH line of ripgrep's output, for its FILE, KEY and JSON.")

(defun agent-hq--scan-session-files (files)
  "Read what sessions are listed by from FILES, with two passes of ripgrep.
Return a hash table of FILE -> alist of (KEY . VALUE), latest first, KEY
one of `cwd', `gitBranch', `aiTitle', `customTitle' and `lastPrompt'.
The cwd and branch come from a file's first message, which names both."
  (let ((fields (make-hash-table :test #'equal))
        (output '("--null" "--with-filename" "--no-line-number" "--no-heading"
                  "--only-matching"))
        (string "\"(?:[^\"\\\\]|\\\\.)*\""))
    (with-temp-buffer
      (agent-hq--rg-insert (append output (list "--max-count" "1" "-e"
                                                 (concat "\"(?:cwd|gitBranch)\":" string)))
                            files)
      (agent-hq--rg-insert (append output (list "-e" (concat "^\\{\"type\":\"(?:ai-title|"
                                                              "custom-title|last-prompt)\",\""
                                                              "(?:aiTitle|customTitle|lastPrompt)\":"
                                                              string)))
                            files)
      (goto-char (point-min))
      (while (re-search-forward agent-hq--session-field-regexp nil t)
        (let ((file (match-string 1))
              (key (intern (match-string 2)))
              (value (ignore-errors (json-parse-string (match-string 3)))))
          (when (stringp value)
            (push (cons key value) (gethash file fields))))))
    fields))

(defun agent-hq--make-claude-session (file mtime config fields)
  "Return the session in FILE, last written at MTIME, of account CONFIG.
FIELDS is what `agent-hq--scan-session-files' read from FILE.  Nil
when FILE names no directory, or neither a title nor a prompt."
  (let ((cwd (alist-get 'cwd fields))
        (title (or (alist-get 'customTitle fields) (alist-get 'aiTitle fields)))
        (prompt (alist-get 'lastPrompt fields)))
    (when (and cwd (or title prompt))
      (list :id (file-name-base file) :file file :config config
            :cwd (file-name-as-directory cwd) :branch (alist-get 'gitBranch fields)
            :title title :prompt prompt :time mtime))))

(defun agent-hq--claude-sessions ()
  "Return the Claude sessions of every account, most recent first.
Each is a plist of :id, :file, :config (the account's config dir),
:cwd, :branch, :title, :prompt and :time.  A file read before is only
read again once it changes."
  (let* ((files (agent-hq--claude-session-files))
         (stale (seq-remove (lambda (file)
                              (equal (take 2 (gethash (car file) agent-hq--session-cache))
                                     (list (nth 1 file) (nth 2 file))))
                            files))
         (present (make-hash-table :test #'equal))
         (by-id (make-hash-table :test #'equal))
         gone)
    (when stale
      (when (> (length stale) 50)
        (message "Reading %d sessions..." (length stale)))
      (let ((fields (agent-hq--scan-session-files (mapcar #'car stale))))
        (pcase-dolist (`(,file ,size ,mtime ,config) stale)
          (puthash file (cons size (cons mtime (agent-hq--make-claude-session
                                                file mtime config (gethash file fields))))
                   agent-hq--session-cache))))
    (dolist (file files)
      (puthash (car file) t present))
    (maphash (lambda (file _) (unless (gethash file present) (push file gone)))
             agent-hq--session-cache)
    (dolist (file gone)
      (remhash file agent-hq--session-cache))
    ;; A session copied into another directory shows once, as its latest copy.
    (dolist (file files)
      (when-let* ((session (cddr (gethash (car file) agent-hq--session-cache)))
                  (id (plist-get session :id))
                  ((let ((seen (gethash id by-id)))
                     (or (null seen) (> (plist-get session :time) (plist-get seen :time))))))
        (puthash id session by-id)))
    (sort (hash-table-values by-id)
          (lambda (a b) (> (plist-get a :time) (plist-get b :time))))))

(defun agent-hq--session-listed-p (session)
  "Return non-nil if SESSION's directory exists and isn't ignored.
Claude resumes a session only in the directory it ran in."
  (let ((cwd (plist-get session :cwd)))
    (and (not (and agent-hq-session-ignored-dirs
                   (string-match-p agent-hq-session-ignored-dirs cwd)))
         (file-directory-p cwd))))

(defun agent-hq--sessions-containing (sessions words)
  "Return those of SESSIONS whose files contain each of WORDS, in any order.
A word matches regardless of case unless it has upper case."
  (let ((files (mapcar (lambda (session) (plist-get session :file)) sessions))
        (found (make-hash-table :test #'equal)))
    (dolist (word words)
      (when files
        (setq files (agent-hq--rg (list "--files-with-matches" "--smart-case"
                                         "--fixed-strings" "-e" word)
                                   files))))
    (dolist (file files)
      (puthash file t found))
    (seq-filter (lambda (session) (gethash (plist-get session :file) found)) sessions)))

(defun agent-hq--session-config (session)
  "Return SESSION's account as a config dir, or `default' for ~/.claude."
  (let ((config (plist-get session :config)))
    (if (equal config (agent-hq--claude-default-config)) 'default config)))

(defun agent-hq--one-line (text width)
  "Return TEXT on one line, cut to WIDTH columns."
  (truncate-string-to-width (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " (or text "")))
                            width nil nil "…"))

(defun agent-hq--found-session-candidates (sessions)
  "Return a (STRING GROUP . SESSION) line of `agent-hq--read' per SESSIONS.
Lines show the date, repo:worktree, the session's branch, title, last
prompt and, off ~/.claude, the account."
  (let ((contexts (make-hash-table :test #'equal)))
    (mapcar
     (lambda (session)
       (let* ((cwd (plist-get session :cwd))
              (ctx (with-memoization (gethash cwd contexts) (agent-hq-context cwd)))
              (title (or (plist-get session :title) (plist-get session :prompt)))
              (prompt (plist-get session :prompt))
              (account (agent-hq--config-account (agent-hq--session-config session))))
         (cons (concat
                (agent-hq--col (propertize (format-time-string "%Y-%m-%d %H:%M"
                                                                (plist-get session :time))
                                            'face 'shadow)
                                16)
                (agent-hq--col (propertize (if ctx
                                                (agent-hq-context-label ctx)
                                              (file-name-nondirectory (directory-file-name cwd)))
                                            'face 'font-lock-keyword-face)
                                24)
                (agent-hq--col (propertize (or (plist-get session :branch) "")
                                            'face 'font-lock-constant-face)
                                32)
                (agent-hq--col (propertize (agent-hq--one-line title 60) 'face 'font-lock-doc-face)
                                60)
                (unless (equal prompt title)
                  (propertize (agent-hq--one-line prompt 80) 'face 'shadow))
                (when account
                  (propertize (format "  [%s]" account) 'face 'shadow))
                (when (agent-hq--session-shell (plist-get session :id))
                  (propertize "  [open in a shell]" 'face 'warning)))
               (cons (if ctx (agent-hq-context-repo ctx) "other") session))))
     sessions)))

(defun agent-hq--claude-config-for (config agent-config)
  "Return Claude AGENT-CONFIG with every client it makes on account CONFIG.
CONFIG is a config dir or `default'.  The shell's restarts and reloads
make their clients from it too, so they stay on that account.  An
AGENT-CONFIG made by this function before changes account: its own
maker is wrapped, not the one that picks the old account, whose binding
would win."
  (let ((maker (or (map-elt agent-config :agent-hq-client-maker)
                   ;; Wrapped before `:agent-hq-client-maker' was kept: each
                   ;; wrapper went in front, so the last maker is the base.
                   (cdr (assq :client-maker (reverse agent-config))))))
    (append (list (cons :client-maker
                        (lambda (buffer)
                          (let ((agent-hq--claude-config-override config))
                            (funcall maker buffer))))
                  (cons :agent-hq-client-maker maker))
            (seq-remove (lambda (entry)
                          (memq (car-safe entry) '(:client-maker :agent-hq-client-maker)))
                        agent-config))))

(defun agent-hq--resume-found-session (session)
  "Resume SESSION in a new Claude shell, in its directory and workspace.
The shell runs on the account that stores SESSION, which can differ
from the one `agent-hq-claude-config-dirs' gives its repo now.  A
session that a shell already runs is shown there instead."
  (let ((id (plist-get session :id))
        (cwd (plist-get session :cwd)))
    (if-let* ((open (agent-hq--session-shell id)))
        (progn (agent-hq-visit-shell open)
               (message "That session is open in %s" (buffer-name open)))
      (when agent-hq-switch-project-function
        (funcall agent-hq-switch-project-function
                 (if-let* ((ctx (agent-hq-context cwd))) (agent-hq-context-root ctx) cwd)))
      (require 'agent-shell nil t)
      (let* ((config (agent-hq--session-config session))
             ;; For the shell's name, which is made before its client.
             (agent-hq--claude-config-override config)
             (default-directory cwd))
        (agent-shell--start :config (agent-hq--claude-config-for
                                     config (agent-shell-anthropic-make-claude-code-config))
                            :session-id id
                            :new-session t
                            :cwd cwd)))))

;;;###autoload
(defun agent-hq-find-session (query)
  "Find a Claude session in any project by what it said, and resume it.
QUERY's words must all appear in a session, anywhere in its prompts,
answers or tool output; case matters only for words with upper case.
An empty QUERY lists every session.  The picker shows the newest first,
grouped by repo, and filters by date, worktree, branch, title and last
prompt.

Sessions come from Claude Code's own files under ~/.claude and the
config dirs of `agent-hq-claude-config-dirs', so the ones run in tmux
or the desktop app, and ones whose shell was killed, are there too.
Those whose directory is gone (a removed worktree) or matches
`agent-hq-session-ignored-dirs' are left out.  The chosen session
opens in a new shell in its directory's workspace, on the account that
stores it."
  (interactive (list (read-string "Sessions containing (RET for all): " nil
                                  'agent-hq--find-session-history)))
  (unless (executable-find "rg")
    (user-error "Finding sessions needs ripgrep (rg)"))
  (let* ((words (split-string query nil t))
         (sessions (seq-filter #'agent-hq--session-listed-p (agent-hq--claude-sessions)))
         (sessions (if words (agent-hq--sessions-containing sessions words) sessions)))
    (unless sessions
      (user-error "%s" (if words
                           (format "No session contains %s" (string-join words " and "))
                         "No sessions yet")))
    (agent-hq--resume-found-session
     (agent-hq--read (if words (format "Resume session (%d with %s): " (length sessions) query)
                        "Resume session: ")
                      (agent-hq--found-session-candidates sessions)))))

;;;; Creating worktrees

(defun agent-hq--refs (dir)
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

(defun agent-hq--repo-setting (alist main-root)
  "Return the value of the first ALIST entry whose regexp matches MAIN-ROOT."
  (cdr (seq-find (lambda (entry) (string-match-p (car entry) main-root)) alist)))

(defun agent-hq--branch-prefix (main-root)
  "Return the branch name prefix configured for the repo at MAIN-ROOT."
  (or (agent-hq--repo-setting agent-hq-branch-prefix-alist main-root) ""))

(defun agent-hq--read-new-branch (main-root)
  "Read a new branch name as type/PREFIXname for the repo at MAIN-ROOT."
  (let* ((type (string-trim (completing-read "Branch type (empty for none): "
                                             agent-hq-branch-types)))
         (prefix (concat (unless (string-empty-p type) (concat type "/"))
                         (agent-hq--branch-prefix main-root)))
         (name (replace-regexp-in-string
                "[[:space:]]+" "-"
                (string-trim (read-string (format "Branch name: %s" prefix))))))
    (when (string-empty-p name)
      (user-error "Empty branch name"))
    (concat prefix name)))

(defun agent-hq--worktree-dir (main-root repo branch)
  "Return the default directory for BRANCH: a sibling of MAIN-ROOT.
For example webapp + feature/KZN-1-x gives ../webapp_feature-KZN-1-x."
  (expand-file-name (concat repo "_" (replace-regexp-in-string "[/[:space:]]+" "-" branch))
                    (file-name-directory (directory-file-name main-root))))

;;;###autoload
(defun agent-hq-new-worktree (&optional existing)
  "Create a worktree on a new branch, next to the main one, and start a shell.
With prefix argument EXISTING, check out an existing branch instead; a
remote branch gets a local tracking branch of the same name."
  (interactive "P")
  (let* ((ctx (or (agent-hq-context (agent-hq--current-root))
                  (user-error "Not in a git repository")))
         (common (agent-hq-context-common-dir ctx))
         (repo (agent-hq-context-repo ctx))
         (main (or (agent-hq--main-root common)
                   (user-error "Bare repositories aren't supported")))
         (refs (agent-hq--refs main))
         (all-refs (append (car refs) (cdr refs)))
         (branch (if existing
                     (let ((ref (completing-read "Branch to check out: " all-refs nil t)))
                       (if (member ref (car refs))
                           ref
                         ;; origin/foo: git makes a local foo tracking it.
                         (replace-regexp-in-string "\\`[^/]+/" "" ref)))
                   (agent-hq--read-new-branch main)))
         (base (unless existing
                 (completing-read (format "Start %s from: " branch) all-refs nil t nil nil
                                  (agent-hq--branch common))))
         (default (agent-hq--worktree-dir main repo branch))
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
    (agent-hq-link-shared-files path)
    (run-hook-with-args 'agent-hq-worktree-created-hook path)
    (message "Created worktree %s on %s" (abbreviate-file-name path) branch)
    (agent-hq-open-worktree path t)))

;;;; Files shared from the main worktree

;;;###autoload
(defun agent-hq-link-shared-files (&optional dir)
  "Link DIR's worktree to the main worktree's `agent-hq-worktree-shared-files'.
Each listed file the main worktree has and DIR's linked worktree lacks
becomes a symlink to the main worktree's copy, where git ignores that
path.  DIR defaults to `default-directory'.  Returns the files linked."
  (when-let* ((ctx (agent-hq-context dir))
              ((agent-hq-context-worktree ctx))
              (main (agent-hq--main-root (agent-hq-context-common-dir ctx)))
              (files (agent-hq--repo-setting agent-hq-worktree-shared-files main)))
    (let ((root (agent-hq-context-root ctx))
          linked)
      (dolist (file files)
        (let ((target (expand-file-name file main))
              (link (expand-file-name file root)))
          ;; `file-symlink-p' also leaves a dangling link alone.
          (when (and (file-exists-p target)
                     (not (file-symlink-p link))
                     (not (file-exists-p link))
                     (agent-hq--git-ignored-p root file))
            (condition-case err
                (progn
                  (make-directory (file-name-directory link) t)
                  (make-symbolic-link target link)
                  (push file linked))
              (error (message "agent-hq: couldn't link %s: %s"
                              (abbreviate-file-name link) (error-message-string err)))))))
      (nreverse linked))))

;;;; Removing worktrees

(defun agent-hq--git (dir &rest args)
  "Run git with ARGS in DIR; return (EXIT . OUTPUT), OUTPUT right-trimmed."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (let ((exit (apply #'process-file "git" nil t nil args)))
        (cons exit (string-trim-right (buffer-string)))))))

(defun agent-hq--git-lines (dir &rest args)
  "Return the output lines of git ARGS in DIR, or signal a `user-error'."
  (pcase-let ((`(,exit . ,output) (apply #'agent-hq--git dir args)))
    (unless (eql exit 0)
      (user-error "git %s failed: %s" (car args) output))
    (split-string output "\n" t)))

(defun agent-hq--verified-ref (dir ref)
  "Return REF if it names a commit in DIR's repo, else nil."
  (when (eql 0 (car (agent-hq--git dir "rev-parse" "--verify" "--quiet"
                                    (concat ref "^{commit}"))))
    ref))

(defun agent-hq--base-ref (dir)
  "Return the ref work in DIR's repo merges into, or nil if it doesn't exist.
See `agent-hq-base-branch-alist'."
  (agent-hq--verified-ref
   dir (or (agent-hq--repo-setting agent-hq-base-branch-alist (agent-hq--repo-root dir))
           "origin/HEAD")))

(defun agent-hq--commits-only-on (dir ref &rest others)
  "Return how many commits REF has that none of OTHERS has, in DIR's repo.
OTHERS that are nil or name no commit are left out.  Returns nil when
git can't tell."
  (pcase-let ((`(,exit . ,output)
               (apply #'agent-hq--git dir "rev-list" "--count" ref "--not"
                      (delq nil (mapcar (lambda (other)
                                          (and other (agent-hq--verified-ref dir other)))
                                        others)))))
    (when (eql exit 0)
      (string-to-number output))))

(defun agent-hq--worktree-branch (root)
  "Return the branch checked out in the worktree at ROOT, or nil if detached."
  (pcase-let ((`(,exit . ,output)
               (agent-hq--git root "symbolic-ref" "--quiet" "--short" "HEAD")))
    (and (eql exit 0) output)))

(defun agent-hq--upstream-gone-p (root branch)
  "Return non-nil if BRANCH tracks a remote branch that no longer exists."
  (equal (car (agent-hq--git-lines root "for-each-ref" "--format=%(upstream:track)"
                                    (concat "refs/heads/" branch)))
         "[gone]"))

(defun agent-hq--disposable-p (path)
  "Return non-nil if a part of PATH matches `agent-hq-disposable-ignored'."
  (seq-some (lambda (part)
              (seq-some (lambda (pattern) (string-match-p (wildcard-to-regexp pattern) part))
                        agent-hq-disposable-ignored))
            (split-string path "/" t)))

(defun agent-hq--worktree-file-buffers (root)
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

(defun agent-hq--removal (ctx)
  "Return what removing CTX's linked worktree involves, as a plist.
Keys: :ctx; :root; :main, the main worktree's root; :branch, nil when
detached; :unsaved, how many of the branch's commits are in neither the
base branch nor its upstream (deleting the branch loses them); :changes,
git status lines of uncommitted changes and untracked files, except
untracked ones `agent-hq-disposable-ignored' covers, such as the
.serena/ an MCP server leaves (:junk lists those); :kept, ignored paths
that would go, beyond `agent-hq-disposable-ignored' and symlinks;
:shells and :busy, the shells working there."
  (let* ((root (agent-hq-context-root ctx))
         (branch (agent-hq--worktree-branch root))
         changes junk kept)
    (when (string-suffix-p "(rebasing)" (or (agent-hq-context-branch ctx) ""))
      (user-error "%s is in the middle of a rebase" (agent-hq-context-label ctx)))
    (dolist (line (agent-hq--git-lines root "status" "--porcelain"
                                        "--ignored=matching" "--untracked-files=all"))
      (let ((path (substring line 3)))
        (cond ((string-prefix-p "!! " line)
               (unless (or (agent-hq--disposable-p path)
                           (file-symlink-p (expand-file-name (directory-file-name path) root)))
                 (push path kept)))
              ((and (string-prefix-p "?? " line) (agent-hq--disposable-p path))
               (push path junk))
              (t (push line changes)))))
    (let ((shells (agent-hq--shells-in root)))
      (list :ctx ctx :root root
            :main (agent-hq--main-root (agent-hq-context-common-dir ctx))
            :branch branch
            :unsaved (and branch
                          (agent-hq--commits-only-on
                           root (concat "refs/heads/" branch) (agent-hq--base-ref root)
                           (concat branch "@{upstream}")))
            :changes (nreverse changes) :junk (nreverse junk) :kept (nreverse kept)
            :shells shells
            :busy (seq-filter (lambda (b) (memq (agent-hq--status b) '(busy blocked)))
                              shells)))))

(defun agent-hq--count (n singular)
  "Return N SINGULAR, pluralized with an s when N isn't 1."
  (format "%d %s%s" n singular (if (= n 1) "" "s")))

(defun agent-hq--preview (items)
  "Return the first few ITEMS joined by commas, noting how many more there are."
  (concat (string-join (seq-take items 4) ", ")
          (when (nthcdr 4 items) (format " and %d more" (- (length items) 4)))))

(defun agent-hq--describe-removal (removal &optional why)
  "Return a yes-or-no question for REMOVAL, from `agent-hq--removal'.
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
                   (format "closing %s%s" (agent-hq--count (length shells) "shell")
                           (if busy (format " (%d busy)" (length busy)) "")))
                 (when changes
                   (format "discarding %s"
                           (agent-hq--count (length changes) "uncommitted change")))
                 (when kept
                   (format "deleting ignored %s" (agent-hq--preview kept)))
                 (cond ((null branch) nil)
                       ((eql unsaved 0) (format "deleting branch %s" branch))
                       (unsaved (format "keeping branch %s (%s only there)" branch
                                        (agent-hq--count unsaved "commit")))
                       (t (format "keeping branch %s" branch)))))))
    (format "Remove %s%s (%s)%s? "
            (agent-hq-context-label ctx)
            (if why (format " [%s]" why) "")
            (abbreviate-file-name (directory-file-name (plist-get removal :root)))
            (if parts (concat ", " (string-join parts ", ")) ""))))

(defun agent-hq--remove (removal)
  "Remove the worktree REMOVAL describes; see `agent-hq--removal'.
Closes its shells and buffers, runs `agent-hq-remove-worktree-functions',
deletes the worktree, then its branch when that loses no commit."
  (let* ((ctx (plist-get removal :ctx))
         (root (plist-get removal :root))
         (main (plist-get removal :main))
         (branch (plist-get removal :branch))
         (dropped (and branch (eql (plist-get removal :unsaved) 0))))
    (mapc #'agent-hq--kill-shell-buffer (plist-get removal :shells))
    (run-hook-with-args 'agent-hq-remove-worktree-functions root)
    (let ((kill-buffer-query-functions nil))
      (mapc #'kill-buffer (agent-hq--worktree-file-buffers root)))
    ;; Buffers left working there, such as a dashboard, move to the main
    ;; worktree: commands fail in a directory that's gone.
    (let ((inside (file-name-as-directory (expand-file-name root))))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p inside (expand-file-name
                                       (or (buffer-local-value 'default-directory buffer) "/")))
          (with-current-buffer buffer
            (setq default-directory (file-name-as-directory main))))))
    ;; git refuses untracked files without --force, even disposable ones.
    (apply #'agent-hq--git-lines main "worktree" "remove"
           (append (when (or (plist-get removal :changes) (plist-get removal :junk))
                     '("--force"))
                   (list (directory-file-name root))))
    (when dropped
      (agent-hq--git-lines main "branch" "-D" branch))
    (message "Removed %s%s" (agent-hq-context-label ctx)
             (cond (dropped (format " and branch %s" branch))
                   (branch (format "; kept branch %s" branch))
                   (t "")))
    t))

(defun agent-hq--check-removable (removal force)
  "Signal a `user-error' if REMOVAL shouldn't go ahead.
Uncommitted changes stop it unless FORCE; modified buffers always do."
  (let ((label (agent-hq-context-label (plist-get removal :ctx)))
        (changes (plist-get removal :changes)))
    (when (and changes (not force))
      (user-error "%s has %s (%s); commit or stash them, or use C-u to discard"
                  label (agent-hq--count (length changes) "uncommitted change")
                  (agent-hq--preview (mapcar (lambda (line) (substring line 3)) changes))))
    (when-let* ((modified (seq-filter #'buffer-modified-p
                                      (seq-filter #'buffer-file-name
                                                  (agent-hq--worktree-file-buffers
                                                   (plist-get removal :root))))))
      (user-error "Save or revert first: %s"
                  (agent-hq--preview (mapcar #'buffer-name modified))))))

(defun agent-hq--worktree-line (ctx)
  "Return a picker line for CTX: repo:worktree, branch, shells and path."
  (let ((root (agent-hq-context-root ctx)))
    (concat
     (agent-hq--col (propertize (agent-hq-context-label ctx) 'face 'font-lock-keyword-face) 30)
     (agent-hq--col (propertize (or (agent-hq-context-branch ctx) "")
                                 'face 'font-lock-constant-face)
                     48)
     (agent-hq--col (agent-hq--shells-summary (agent-hq--shells-in root)) 22)
     (propertize (abbreviate-file-name root) 'face 'shadow))))

(defun agent-hq--linked-worktrees ()
  "Return the linked worktrees of every known repo, this worktree first."
  (let* ((here (agent-hq--truename (agent-hq--current-root)))
         (linked (seq-filter #'agent-hq-context-worktree (agent-hq--all-worktrees)))
         (current (seq-find (lambda (ctx)
                              (string-prefix-p (agent-hq--truename (agent-hq-context-root ctx))
                                               here))
                            linked)))
    (if current (cons current (delq current linked)) linked)))

;;;###autoload
(defun agent-hq-remove-worktree (&optional force)
  "Remove a linked worktree of any known repo, this one offered first.
Closes its shells and buffers, then deletes its directory, transcripts
included; sessions stay with Claude and Codex.  Refuses while it has
uncommitted changes or untracked files, unless FORCE (prefix argument).
The question lists the shells it closes and the ignored files it deletes
beyond `agent-hq-disposable-ignored'.  The branch goes too when no
commit would be lost: everything on it is in the repo's base branch (see
`agent-hq-base-branch-alist') or on its upstream."
  (interactive "P")
  (agent-hq-remove-worktree-at
   (agent-hq--read
    "Remove worktree: "
    (mapcar (lambda (ctx)
              (cons (agent-hq--worktree-line ctx)
                    (cons (agent-hq-context-repo ctx) ctx)))
            (or (agent-hq--linked-worktrees)
                (user-error "No linked worktrees"))))
   force))

(defun agent-hq-remove-worktree-at (ctx &optional force)
  "Remove CTX's linked worktree after asking, as `agent-hq-remove-worktree' does.
FORCE discards uncommitted changes."
  (let ((removal (agent-hq--removal ctx)))
    (agent-hq--check-removable removal force)
    (unless (yes-or-no-p (agent-hq--describe-removal removal))
      (user-error "Kept %s" (agent-hq-context-label ctx)))
    (agent-hq--remove removal)))

(defun agent-hq--finished (ctx)
  "Return why CTX's worktree looks finished, or nil.
Finished means nothing beyond the repo's base branch, or a branch whose
upstream is gone, as when a merged PR's branch was deleted."
  (let* ((root (agent-hq-context-root ctx))
         (base (agent-hq--base-ref root))
         (branch (agent-hq--worktree-branch root)))
    (cond ((and base (eql 0 (agent-hq--commits-only-on root "HEAD" base)))
           (format "nothing beyond %s" base))
          ((and branch (agent-hq--upstream-gone-p root branch))
           "upstream gone"))))

;;;###autoload
(defun agent-hq-clean-worktrees ()
  "Offer to remove each linked worktree whose work looks finished.
Finished means no shells, no uncommitted changes, and either nothing
beyond the repo's base branch or a branch whose upstream is gone.  Asks
about each, saying what removing it involves (y, n, ! for all, q to
stop); see `agent-hq-remove-worktree'."
  (interactive)
  (let ((open 0) (removed 0) removals)
    (dolist (ctx (agent-hq--linked-worktrees))
      (when-let* ((why (agent-hq--finished ctx)))
        (if (agent-hq--shells-in (agent-hq-context-root ctx))
            (cl-incf open)
          (let ((removal (agent-hq--removal ctx)))
            (unless (plist-get removal :changes)
              (push (plist-put removal :why why) removals))))))
    (if (null removals)
        (message "No finished worktrees%s"
                 (if (> open 0) (format " (%d with shells open)" open) ""))
      (map-y-or-n-p
       (lambda (removal)
         (agent-hq--describe-removal removal (plist-get removal :why)))
       (lambda (removal)
         (condition-case err
             (progn (agent-hq--check-removable removal nil)
                    (agent-hq--remove removal)
                    (cl-incf removed))
           (user-error (message "%s" (error-message-string err)))))
       (nreverse removals)
       '("worktree" "worktrees" "remove"))
      (message "Removed %s%s" (agent-hq--count removed "worktree")
               (if (> open 0) (format "; left %d with shells open" open) "")))))

;;;; Transcripts and notifications

;;;###autoload
(defun agent-hq-search-transcripts ()
  "Search the agent-shell transcripts of every known worktree with ripgrep."
  (interactive)
  (require 'consult)
  (let ((dirs (seq-filter #'file-directory-p
                          (mapcar (lambda (ctx)
                                    (expand-file-name ".agent-shell/transcripts"
                                                      (agent-hq-context-root ctx)))
                                  (agent-hq--all-worktrees))))
        ;; Transcripts sit in an ignored, hidden directory.
        (consult-ripgrep-args (if (listp consult-ripgrep-args)
                                  (append consult-ripgrep-args '("--hidden" "--no-ignore"))
                                (concat consult-ripgrep-args " --hidden --no-ignore"))))
    (unless dirs
      (user-error "No transcripts yet"))
    (consult-ripgrep dirs)))

(defun agent-hq--emacsclient ()
  "Return the emacsclient that belongs to this Emacs, or nil."
  (seq-find #'file-executable-p
            (list (expand-file-name "bin/emacsclient" invocation-directory)
                  (expand-file-name "emacsclient" invocation-directory)
                  (executable-find "emacsclient"))))

(defcustom agent-hq-notifier "terminal-notifier"
  "The program posting agent-hq's macOS notifications, or nil for none.
terminal-notifier 3 or later (brew install terminal-notifier).  When
it is missing or fails, a warning says so, once."
  :type '(choice (const :tag "None" nil) string))

(defvar agent-hq--notifier-warned nil
  "Non-nil once a notifier problem was reported: it is reported only once.")

(defun agent-hq--notifier-warn (format-string &rest args)
  "Warn, once, that notifications don't work: FORMAT-STRING and ARGS say why."
  (unless agent-hq--notifier-warned
    (setq agent-hq--notifier-warned t)
    (display-warning 'agent-hq (apply #'format-message format-string args))))

(defun agent-hq--run-notifier (args)
  "Run `agent-hq-notifier' with ARGS in the background.
Return its process, or nil when it can't run.  A failure, or a notifier
missing, is warned about once."
  (when agent-hq-notifier
    (if-let* ((program (executable-find agent-hq-notifier)))
        (condition-case err
            (let ((output (generate-new-buffer " *agent-hq-notifier*" t)))
              (make-process
               :name "agent-hq-notifier" :buffer output :command (cons program args)
               :connection-type 'pipe :noquery t
               :sentinel
               (lambda (process _event)
                 (unless (process-live-p process)
                   (unwind-protect
                       (unless (eql (process-exit-status process) 0)
                         (agent-hq--notifier-warn
                          "%s failed (exit %s), so agent notifications don't show: %s"
                          program (process-exit-status process)
                          (with-current-buffer output
                            (or (car (split-string (buffer-string) "\n" t "[ \t]+"))
                                "it said nothing"))))
                     (kill-buffer output))))))
          (error (agent-hq--notifier-warn "Can't run %s: %s" program (error-message-string err))
                 nil))
      (agent-hq--notifier-warn
       "No %s on `exec-path', so agent notifications are off (brew install terminal-notifier)"
       agent-hq-notifier)
      nil)))

(defun agent-hq--notifier-text (text)
  "Return TEXT so the notifier shows it as is.
A leading - would read as an option, a leading [ as markup."
  (if (string-match-p "\\`[-[]" text) (concat "​" text) text))

(defun agent-hq-notification-group (buffer)
  "Return the group of notifications about shell BUFFER.
A notification replaces the one of its group before it."
  (format "agent-hq-%s" (or (and (buffer-live-p buffer) (agent-hq--shell-id buffer))
                            (sxhash-equal (and (buffer-live-p buffer) (buffer-name buffer))))))

(cl-defun agent-hq-post-notification (buffer title body &key subtitle sound click group)
  "Post a macOS notification about agent shell BUFFER with TITLE and BODY.
SUBTITLE goes under the title; SOUND names a sound to play, from
/System/Library/Sounds or \"default\".  Clicking the notification
brings Emacs forward and, through emacsclient, evaluates the form
CLICK.  It replaces the notification of GROUP before it (default
`agent-hq-notification-group'), and `agent-hq-remove-notification'
takes it away.  Return GROUP, or nil when nothing could be posted."
  (let* ((group (or group (agent-hq-notification-group buffer)))
         (client (agent-hq--emacsclient))
         (args (append
                (list "-title" (agent-hq--notifier-text title)
                      "-message" (agent-hq--notifier-text
                                  (if (string-empty-p (or body "")) "Needs you" body))
                      "-group" group
                      "-activate" "org.gnu.Emacs")
                (and (stringp subtitle) (not (string-empty-p subtitle))
                     (list "-subtitle" (agent-hq--notifier-text subtitle)))
                (and sound (list "-sound" sound))
                (and click client (bound-and-true-p server-name)
                     (list "-execute"
                           (mapconcat #'shell-quote-argument
                                      (list client "-s" server-name "-n" "-e"
                                            (prin1-to-string click))
                                      " "))))))
    (and (agent-hq--run-notifier args) group)))

(defun agent-hq-remove-notification (group)
  "Take away the notifications of GROUP, from `agent-hq-post-notification'."
  (when (and agent-hq-notifier (executable-find agent-hq-notifier))
    (agent-hq--run-notifier (list "-remove" group))))

;;;###autoload
(defun agent-hq-notify (buffer title body)
  "Post a macOS notification about agent shell BUFFER with TITLE and BODY.
Clicking it visits the shell.  Its group is of its own, so it neither
replaces nor goes with the shell's notifications from agent-hq-attention."
  (let ((name (if (buffer-live-p buffer) (buffer-name buffer) title)))
    (agent-hq-post-notification
     buffer title body
     :group (concat (agent-hq-notification-group buffer) "-note")
     :click `(agent-hq-visit-shell-named
              ,name ,(and (buffer-live-p buffer) (agent-hq--shell-id buffer))))))

(provide 'agent-hq)

;;; agent-hq.el ends here
