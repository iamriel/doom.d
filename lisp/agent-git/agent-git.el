;;; agent-git.el --- Repo, worktree and branch context for agent-shell -*- lexical-binding: t; -*-

;;; Commentary:

;; Names every agent-shell buffer after the repo, worktree and branch it works
;; in, for example "Claude @ webapp:worktree1 ⎇ bug/KZN-19576-max-length",
;; and renames it when the agent switches branches.  The name is what the mode
;; line shows and what every buffer switcher matches, so the branch is both
;; visible and searchable.
;;
;; Commands:
;;   `agent-git-switch-shell'        every live shell, all projects
;;   `agent-git-switch-worktree'     every worktree of every known repo
;;   `agent-git-new-worktree'        new branch + sibling worktree + shell
;;   `agent-git-toggle'              this worktree's shell, never another's
;;   `agent-git-resume-session'      pick a past session for this worktree
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
(declare-function shell-maker-set-buffer-name "shell-maker")
(declare-function consult-ripgrep "consult")
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

;;;; Shell names

(defun agent-git--format-name (agent-name ctx fallback)
  "Return a shell name for AGENT-NAME in CTX, or in FALLBACK outside git."
  (concat agent-name " @ "
          (if ctx
              (concat (agent-git-context-label ctx)
                      (when-let* ((branch (agent-git-context-branch ctx)))
                        (concat agent-git-branch-separator branch)))
            fallback)))

;;;###autoload
(defun agent-git-buffer-name (agent-name project-name)
  "Name a new shell after AGENT-NAME and its repo, worktree and branch.
For `agent-shell-buffer-name-format'.  Uses PROJECT-NAME outside git."
  (agent-git--format-name agent-name (agent-git-context) project-name))

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
                    (new (agent-git--format-name (agent-git--agent-name) ctx nil))
                    ((not (string= (agent-git--strip-uniquifier old) new))))
          (let ((viewport (get-buffer (concat old " [viewport]"))))
            (shell-maker-set-buffer-name buffer new)
            (when (buffer-live-p viewport)
              (with-current-buffer viewport
                (rename-buffer (concat (buffer-name buffer) " [viewport]") t)))))))))

;;;###autoload
(defun agent-git-setup-shell-h ()
  "Rename this shell whenever a turn ends on a different branch.
For `agent-shell-mode-hook'."
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
                  "  " (propertize (format "[%s]" (agent-git--agent-name buffer)) 'face 'shadow))
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
      (mapcar
       (lambda (ctx)
         (let ((root (agent-git-context-root ctx)))
           (cons (concat
                  (agent-git--col (propertize (agent-git-context-label ctx)
                                              'face 'font-lock-keyword-face) 30)
                  (agent-git--col (propertize (or (agent-git-context-branch ctx) "")
                                              'face 'font-lock-constant-face) 48)
                  (agent-git--col (agent-git--shells-summary (agent-git--shells-in root)) 22)
                  (propertize (abbreviate-file-name root) 'face 'shadow))
                 (cons (agent-git-context-repo ctx) root))))
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

(defun agent-git--branch-prefix (main-root)
  "Return the branch name prefix configured for the repo at MAIN-ROOT."
  (or (cdr (seq-find (lambda (entry) (string-match-p (car entry) main-root))
                     agent-git-branch-prefix-alist))
      ""))

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
    (run-hook-with-args 'agent-git-worktree-created-hook path)
    (message "Created worktree %s on %s" (abbreviate-file-name path) branch)
    (agent-git-open-worktree path t)))

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
