;;; agent-git-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-git -l tests/agent-git-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Uses real git in a temporary directory; agent-shell is not needed.

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../lisp/agent-git/agent-git.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun agent-git-test--git (dir &rest args)
  "Run git ARGS in DIR, failing the test on a non-zero exit."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (let ((status (apply #'call-process "git" nil t nil
                           "-c" "user.name=Test" "-c" "user.email=t@example.com"
                           "-c" "init.defaultBranch=develop" args)))
        (unless (zerop status)
          (error "git %S failed: %s" args (buffer-string)))
        (string-trim (buffer-string))))))

(defmacro agent-git-test--with-repos (&rest body)
  "Run BODY with `base' bound to a temp dir holding a repo and its worktrees.
Layout:
  base/webapp                      main worktree, branch develop
  base/webapp_worktree1            branch bug/KZN-1-max-length
  base/webapp/.claude/worktrees/webapp-pr-review-abc   branch claude/pr-review
  base/plain                       not a git repo"
  (declare (indent 0))
  `(let* ((base (file-name-as-directory
                 (file-truename (make-temp-file "agent-git-test" t))))
          (main (expand-file-name "webapp/" base)))
     (unwind-protect
         (progn
           (make-directory main)
           (make-directory (expand-file-name "plain" base))
           (agent-git-test--git main "init" "-q")
           (agent-git-test--git main "commit" "-q" "--allow-empty" "-m" "init")
           (agent-git-test--git main "worktree" "add" "-q" "-b" "bug/KZN-1-max-length"
                                (expand-file-name "webapp_worktree1" base))
           (agent-git-test--git main "worktree" "add" "-q" "-b" "claude/pr-review"
                                (expand-file-name ".claude/worktrees/webapp-pr-review-abc" main))
           ,@body)
       (delete-directory base t))))

(ert-deftest agent-git-test-main-worktree ()
  (agent-git-test--with-repos
    (let ((ctx (agent-git-context (expand-file-name "webapp/" base))))
      (should (equal (agent-git-context-repo ctx) "webapp"))
      (should-not (agent-git-context-worktree ctx))
      (should (equal (agent-git-context-branch ctx) "develop"))
      (should (equal (agent-git-context-label ctx) "webapp")))))

(ert-deftest agent-git-test-sibling-worktree ()
  (agent-git-test--with-repos
    (let ((ctx (agent-git-context (expand-file-name "webapp_worktree1/" base))))
      (should (equal (agent-git-context-repo ctx) "webapp"))
      (should (equal (agent-git-context-worktree ctx) "worktree1"))
      (should (equal (agent-git-context-branch ctx) "bug/KZN-1-max-length"))
      (should (equal (agent-git-context-label ctx) "webapp:worktree1")))))

(ert-deftest agent-git-test-nested-claude-worktree ()
  (agent-git-test--with-repos
    (let ((ctx (agent-git-context
                (expand-file-name ".claude/worktrees/webapp-pr-review-abc/" main))))
      (should (equal (agent-git-context-label ctx) "webapp:pr-review-abc"))
      (should (equal (agent-git-context-branch ctx) "claude/pr-review")))))

(ert-deftest agent-git-test-subdirectory-uses-worktree-root ()
  (agent-git-test--with-repos
    (let ((sub (expand-file-name "webapp_worktree1/src/" base)))
      (make-directory sub t)
      (should (equal (agent-git-context-root (agent-git-context sub))
                     (expand-file-name "webapp_worktree1/" base))))))

(ert-deftest agent-git-test-not-git ()
  (agent-git-test--with-repos
    (should-not (agent-git-context (expand-file-name "plain/" base)))))

(ert-deftest agent-git-test-detached-and-rebasing ()
  (agent-git-test--with-repos
    (let ((wt (expand-file-name "webapp_worktree1/" base)))
      (agent-git-test--git wt "checkout" "-q" "--detach")
      (should (string-prefix-p "detached@" (agent-git-context-branch (agent-git-context wt))))
      ;; What git leaves behind mid-rebase: detached HEAD plus head-name.
      (let ((git-dir (agent-git--git-dir wt)))
        (make-directory (expand-file-name "rebase-merge" git-dir))
        (with-temp-file (expand-file-name "rebase-merge/head-name" git-dir)
          (insert "refs/heads/bug/KZN-1-max-length\n")))
      (should (equal (agent-git-context-branch (agent-git-context wt))
                     "bug/KZN-1-max-length (rebasing)")))))

(ert-deftest agent-git-test-branch-switch-is-seen ()
  (agent-git-test--with-repos
    (let ((wt (expand-file-name "webapp_worktree1/" base)))
      (agent-git-test--git wt "checkout" "-q" "-b" "feature/KZN-2-other")
      (should (equal (agent-git-context-branch (agent-git-context wt))
                     "feature/KZN-2-other")))))

(ert-deftest agent-git-test-labels ()
  (should (equal (agent-git--worktree-label "webapp" "/x/webapp_worktree1/" "/x/webapp/")
                 "worktree1"))
  (should (equal (agent-git--worktree-label "gymmers-app" "/x/gymmers-app_w1/" "/x/gymmers-app/")
                 "w1"))
  (should (equal (agent-git--worktree-label "webapp" "/x/hotfix/" "/x/webapp/") "hotfix"))
  ;; Codex app worktrees: ~/.codex/worktrees/<id>/<repo>.
  (should (equal (agent-git--worktree-label "triply" "/h/.codex/worktrees/08f5/triply/"
                                            "/p/triply/")
                 "08f5")))

(ert-deftest agent-git-test-buffer-name ()
  (agent-git-test--with-repos
    (let ((default-directory (expand-file-name "webapp_worktree1/" base)))
      (should (equal (agent-git-buffer-name "Claude" "ignored")
                     "Claude @ webapp:worktree1 ⎇ bug/KZN-1-max-length")))
    (let ((default-directory (expand-file-name "plain/" base)))
      (should (equal (agent-git-buffer-name "Codex" "plain") "Codex @ plain")))))

(ert-deftest agent-git-test-strip-uniquifier ()
  (should (equal (agent-git--strip-uniquifier "Claude @ webapp ⎇ develop<2>")
                 "Claude @ webapp ⎇ develop"))
  (should (equal (agent-git--strip-uniquifier "Claude @ webapp") "Claude @ webapp")))

(ert-deftest agent-git-test-repo-worktrees ()
  (agent-git-test--with-repos
    (let* ((common (agent-git-context-common-dir (agent-git-context main)))
           (labels (mapcar #'agent-git-context-label (agent-git--repo-worktrees common))))
      (should (equal labels '("webapp" "webapp:pr-review-abc" "webapp:worktree1"))))
    ;; A worktree deleted without `git worktree remove' is left out.
    (delete-directory (expand-file-name "webapp_worktree1" base) t)
    (let ((common (agent-git-context-common-dir (agent-git-context main))))
      (should (equal (mapcar #'agent-git-context-label (agent-git--repo-worktrees common))
                     '("webapp" "webapp:pr-review-abc"))))))

(defvar projectile-known-projects nil)

(ert-deftest agent-git-test-known-repos-dedupe ()
  (agent-git-test--with-repos
    (let* ((other (expand-file-name "other/" base))
           (projectile-known-projects
            (list (expand-file-name "webapp_worktree1/" base)
                  other
                  main
                  (expand-file-name "plain/" base)
                  "/no/such/dir/")))
      (make-directory other)
      (agent-git-test--git other "init" "-q")
      (cl-letf (((symbol-function 'agent-git--shells) #'ignore)
                ((symbol-function 'agent-git--projectile-projects)
                 (lambda () projectile-known-projects)))
        ;; Starting from the other repo puts it first; webapp's three
        ;; entries collapse into one; non-git and missing dirs drop out.
        (should (equal (mapcar #'agent-git--repo-name (agent-git--known-common-dirs other))
                       '("other" "webapp")))))))

(ert-deftest agent-git-test-worktree-dir ()
  (should (equal (agent-git--worktree-dir "/p/kizen/webapp/" "webapp" "feature/KZN-15113-start x")
                 "/p/kizen/webapp_feature-KZN-15113-start-x")))

(ert-deftest agent-git-test-new-branch-name ()
  (let ((agent-git-branch-prefix-alist '(("/kizen/" . "KZN-"))))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "bug"))
              ((symbol-function 'read-string) (lambda (&rest _) " 19576 text field ")))
      (should (equal (agent-git--read-new-branch "/p/kizen/webapp/")
                     "bug/KZN-19576-text-field"))
      (should (equal (agent-git--read-new-branch "/p/gymmers/app/")
                     "bug/19576-text-field")))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) ""))
              ((symbol-function 'read-string) (lambda (&rest _) "obs")))
      (should (equal (agent-git--read-new-branch "/p/gymmers/app/") "obs")))))

(ert-deftest agent-git-test-picker-groups-and-duplicates ()
  (let (seen-groups)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (let* ((metadata (funcall table "" nil 'metadata))
                        (group (alist-get 'group-function (cdr metadata)))
                        (strings (all-completions "" table)))
                   (setq seen-groups (mapcar (lambda (s) (funcall group s nil)) strings))
                   (nth 1 strings)))))
      (should (eq (agent-git--read "x: " '(("same" "webapp" . first)
                                           ("same" "gymmers" . second)))
                  'second))
      (should (equal seen-groups '("webapp" "gymmers"))))))

(defmacro agent-git-test--with-work-account (&rest body)
  "Run BODY in `agent-git-test--with-repos' with webapp mapped to a work account."
  (declare (indent 0))
  `(agent-git-test--with-repos
     (let ((agent-git-claude-config-dirs
            (list (cons (concat (regexp-quote main) "\\'") "~/.claude-work"))))
       ,@body)))

(ert-deftest agent-git-test-claude-config-dir ()
  (agent-git-test--with-work-account
    (let ((work (file-name-as-directory (expand-file-name "~/.claude-work"))))
      ;; Every worktree of webapp, and directories inside them, use the account.
      (dolist (dir (list main
                         (expand-file-name "webapp_worktree1/" base)
                         (expand-file-name ".claude/worktrees/webapp-pr-review-abc/" main)))
        (should (equal (agent-git-claude-config-dir dir) work)))
      (let ((sub (expand-file-name "webapp_worktree1/src/" base)))
        (make-directory sub t)
        (should (equal (agent-git-claude-config-dir sub) work)))
      ;; Unmapped and non-git directories keep the default.
      (should-not (agent-git-claude-config-dir (expand-file-name "plain/" base)))
      (let ((agent-git-claude-config-dirs nil))
        (should-not (agent-git-claude-config-dir main))))))

(ert-deftest agent-git-test-claude-account-label ()
  (cl-letf (((symbol-function 'agent-git-claude-config-dir)
             (lambda (&optional _) "/h/.claude-work/")))
    (should (equal (agent-git-claude-account) "work")))
  (cl-letf (((symbol-function 'agent-git-claude-config-dir)
             (lambda (&optional _) "/h/claude_kizen/")))
    (should (equal (agent-git-claude-account) "kizen")))
  (cl-letf (((symbol-function 'agent-git-claude-config-dir)
             (lambda (&optional _) "/h/company/")))
    (should (equal (agent-git-claude-account) "company")))
  (cl-letf (((symbol-function 'agent-git-claude-config-dir) #'ignore))
    (should-not (agent-git-claude-account))))

(ert-deftest agent-git-test-buffer-name-shows-account ()
  (agent-git-test--with-work-account
    (let ((default-directory (expand-file-name "webapp_worktree1/" base)))
      (should (equal (agent-git-buffer-name "Claude" "ignored")
                     "Claude (work) @ webapp:worktree1 ⎇ bug/KZN-1-max-length"))
      ;; Only Claude reads CLAUDE_CONFIG_DIR.
      (should (equal (agent-git-buffer-name "Codex" "ignored")
                     "Codex @ webapp:worktree1 ⎇ bug/KZN-1-max-length")))
    (let ((default-directory (expand-file-name "plain/" base)))
      (should (equal (agent-git-buffer-name "Claude" "plain") "Claude @ plain")))))

(defvar agent-shell-anthropic-claude-environment nil)

(ert-deftest agent-git-test-claude-environment-advice ()
  (agent-git-test--with-work-account
    (let ((agent-shell-anthropic-claude-environment '("ENABLE_TOOL_SEARCH=true"))
          (maker (lambda (&rest _) agent-shell-anthropic-claude-environment)))
      (with-temp-buffer
        (setq default-directory (expand-file-name "webapp_worktree1/" base))
        (should (equal (agent-git-claude-environment-a maker :buffer (current-buffer))
                       (list (concat "CLAUDE_CONFIG_DIR="
                                     (expand-file-name "~/.claude-work"))
                             "ENABLE_TOOL_SEARCH=true"))))
      ;; Elsewhere the environment is passed through untouched.
      (with-temp-buffer
        (setq default-directory (expand-file-name "plain/" base))
        (should (equal (agent-git-claude-environment-a maker :buffer (current-buffer))
                       '("ENABLE_TOOL_SEARCH=true"))))
      ;; The binding doesn't leak.
      (should (equal agent-shell-anthropic-claude-environment '("ENABLE_TOOL_SEARCH=true"))))))

(ert-deftest agent-git-test-label-follows-started-account ()
  (agent-git-test--with-work-account
    (let ((maker (lambda (&rest _) nil))
          (wt (expand-file-name "webapp_worktree1/" base)))
      ;; Started on the work account: keeps the label after the mapping goes.
      (with-temp-buffer
        (setq default-directory wt)
        (agent-git-claude-environment-a maker :buffer (current-buffer))
        (let ((agent-git-claude-config-dirs nil))
          (should (equal (agent-git--agent-display-name "Claude" (agent-git--claude-account-here))
                         "Claude (work)"))))
      ;; Started on the default account: gains no label once a mapping appears.
      (with-temp-buffer
        (setq default-directory wt)
        (let ((agent-git-claude-config-dirs nil))
          (agent-git-claude-environment-a maker :buffer (current-buffer)))
        (should (equal (agent-git--agent-display-name "Claude" (agent-git--claude-account-here))
                       "Claude"))))))

(ert-deftest agent-git-test-new-shell-name-ignores-current-shell ()
  ;; Naming a new shell while another shell is current: the new one's
  ;; directory decides, not the account of the shell the command ran in.
  (agent-git-test--with-work-account
    (let ((maker (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq default-directory (expand-file-name "webapp_worktree1/" base))
        (agent-git-claude-environment-a maker :buffer (current-buffer))
        (let ((default-directory (expand-file-name "plain/" base)))
          (should (equal (agent-git-buffer-name "Claude" "plain") "Claude @ plain"))))
      (with-temp-buffer
        (setq default-directory (expand-file-name "plain/" base))
        (agent-git-claude-environment-a maker :buffer (current-buffer))
        (let ((default-directory main))
          (should (equal (agent-git-buffer-name "Claude" "webapp")
                         "Claude (work) @ webapp ⎇ develop")))))))

;;;; Shared project memory

(defmacro agent-git-test--with-default-config (&rest body)
  "Run BODY with the default account's config dir at base/dot-claude/."
  (declare (indent 0))
  `(let ((process-environment
          (cons (concat "CLAUDE_CONFIG_DIR=" (expand-file-name "dot-claude" base))
                process-environment)))
     ,@body))

(defun agent-git-test--slurp (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest agent-git-test-claude-project-key ()
  (should (equal (agent-git--claude-project-key "/Users/me/projects/kizen/webapp/")
                 "-Users-me-projects-kizen-webapp"))
  (should (equal (agent-git--claude-project-key "/tmp-x/a_b.c d/")
                 "-tmp-x-a-b-c-d"))
  ;; Claude Code hashes very long names; don't guess those.
  (should-not (agent-git--claude-project-key (concat "/" (make-string 250 ?a)))))

(ert-deftest agent-git-test-shared-memory-dir-is-per-repo ()
  (agent-git-test--with-repos
    (agent-git-test--with-default-config
      (let ((expected (expand-file-name
                       (concat "dot-claude/projects/"
                               (agent-git--claude-project-key main) "/memory")
                       base)))
        ;; One memory for the whole repo, as Claude Code keeps it.
        (dolist (dir (list main
                           (expand-file-name "webapp_worktree1/" base)
                           (expand-file-name ".claude/worktrees/webapp-pr-review-abc/" main)))
          (should (equal (agent-git-claude-shared-memory-dir dir) expected)))))))

(ert-deftest agent-git-test-share-memory-edits-main-settings-only ()
  (agent-git-test--with-repos
    (agent-git-test--with-default-config
      (let* ((settings (expand-file-name ".claude/settings.local.json" main))
             (wt (expand-file-name "webapp_worktree1/" base))
             (wt-settings (expand-file-name ".claude/settings.local.json" wt))
             (memory (agent-git-claude-shared-memory-dir main)))
        (make-directory (file-name-directory settings) t)
        (make-directory (file-name-directory wt-settings) t)
        (write-region "{\"permissions\": {\"allow\": [\"Bash(ls)\"], \"deny\": []},
 \"env\": {}, \"flag\": false, \"gone\": null}\n" nil settings)
        (write-region "{\"permissions\": {}}\n" nil wt-settings)
        ;; Called from a linked worktree, it edits the main worktree's file.
        (should (equal (agent-git-claude-share-memory wt) memory))
        (let ((data (agent-git--read-json settings)))
          (should (equal (mapcar #'car data)
                         '(permissions env flag gone autoMemoryDirectory)))
          (should (equal (alist-get 'autoMemoryDirectory data) memory))
          ;; Everything else survives, including false, null and {}.
          (should (equal (alist-get 'permissions data) '((allow . ["Bash(ls)"]) (deny . []))))
          (should (equal (alist-get 'flag data) :false))
          (should (equal (alist-get 'gone data) :null))
          (should (string-match-p "\"env\": {}" (agent-git-test--slurp settings))))
        (should (equal (agent-git-test--slurp wt-settings) "{\"permissions\": {}}\n"))
        ;; Already in place: the file is not rewritten.
        (let ((before (agent-git-test--slurp settings)))
          (should (equal (agent-git-claude-share-memory main) memory))
          (should (equal (agent-git-test--slurp settings) before)))
        ;; A stale value is replaced, not duplicated.
        (write-region "{\"autoMemoryDirectory\": \"/old\", \"x\": 1}" nil settings)
        (agent-git-claude-share-memory main)
        (should (equal (agent-git--read-json settings)
                       `((autoMemoryDirectory . ,memory) (x . 1))))))))

(ert-deftest agent-git-test-share-memory-leaves-files-alone ()
  (agent-git-test--with-repos
    (agent-git-test--with-default-config
      (let ((settings (expand-file-name ".claude/settings.local.json" main))
            ;; Keep the user's global excludes (~/.config/git/ignore) out.
            (process-environment (append (list "GIT_CONFIG_GLOBAL=/dev/null"
                                               (concat "XDG_CONFIG_HOME=" base))
                                         process-environment)))
        ;; No file, and git would track one: nothing is created.
        (should-not (agent-git-claude-share-memory main))
        (should-not (file-exists-p settings))
        ;; Where git ignores it, the file is created.
        (write-region ".claude/settings.local.json\n" nil (expand-file-name ".gitignore" main))
        (should (agent-git-claude-share-memory main))
        (should (equal (mapcar #'car (agent-git--read-json settings)) '(autoMemoryDirectory)))
        ;; Unreadable JSON is left as it is.
        (write-region "{ not json" nil settings)
        (let ((inhibit-message t))
          (should-not (agent-git-claude-share-memory main)))
        (should (equal (agent-git-test--slurp settings) "{ not json"))
        ;; Outside git there is no repo memory to point at.
        (should-not (agent-git-claude-share-memory (expand-file-name "plain/" base)))))))

(ert-deftest agent-git-test-advice-shares-memory-for-mapped-repos ()
  (agent-git-test--with-work-account
    (agent-git-test--with-default-config
      (let ((settings (expand-file-name ".claude/settings.local.json" main))
            (maker (lambda (&rest _) nil)))
        (make-directory (file-name-directory settings) t)
        (write-region "{}" nil settings)
        (let ((agent-git-claude-shared-memory nil))
          (with-temp-buffer
            (setq default-directory (expand-file-name "webapp_worktree1/" base))
            (agent-git-claude-environment-a maker :buffer (current-buffer)))
          (should (equal (agent-git-test--slurp settings) "{}")))
        (with-temp-buffer
          (setq default-directory (expand-file-name "webapp_worktree1/" base))
          (agent-git-claude-environment-a maker :buffer (current-buffer)))
        (should (equal (alist-get 'autoMemoryDirectory (agent-git--read-json settings))
                       (agent-git-claude-shared-memory-dir main)))
        ;; Repos on the default account are never touched.
        (let ((agent-git-claude-config-dirs nil))
          (write-region "{}" nil settings)
          (with-temp-buffer
            (setq default-directory main)
            (agent-git-claude-environment-a maker :buffer (current-buffer)))
          (should (equal (agent-git-test--slurp settings) "{}")))))))

;;;; Shells in any project

(ert-deftest agent-git-test-project-candidates ()
  (agent-git-test--with-repos
    (let ((wt (expand-file-name "webapp_worktree1/" base))
          (plain (expand-file-name "plain/" base)))
      (cl-letf (((symbol-function 'agent-git--projectile-projects)
                 (lambda ()
                   (list (abbreviate-file-name wt)
                         main
                         (directory-file-name main) ; same project, no slash
                         plain
                         (expand-file-name "gone/" base)
                         "/ssh:host:/srv/app/"
                         nil))))
        (let ((candidates (agent-git--project-candidates)))
          ;; Projectile's order, one line per existing local project.
          (should (equal (mapcar #'cddr candidates) (list wt main plain)))
          (should (equal (mapcar #'cadr candidates) '("webapp" "webapp" "other")))
          (should (string-match-p "\\`webapp:worktree1 +bug/KZN-1-max-length "
                                  (substring-no-properties (car (nth 0 candidates)))))
          (should (string-match-p "\\`plain " (substring-no-properties (car (nth 2 candidates))))))))))

(ert-deftest agent-git-test-start-in-project ()
  (agent-git-test--with-repos
    (let* ((wt (expand-file-name "webapp_worktree1/" base))
           switched started
           (agent-git-switch-project-function (lambda (root) (setq switched root))))
      (cl-letf (((symbol-function 'agent-git--projectile-projects) (lambda () (list main wt)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (seq-find (lambda (s) (string-prefix-p "webapp:worktree1" s))
                             (all-completions "" table))))
                ((symbol-function 'agent-shell-anthropic-start-claude-code)
                 (lambda () (setq started (list 'claude default-directory))))
                ((symbol-function 'agent-shell-openai-start-codex)
                 (lambda () (setq started (list 'codex default-directory)))))
        ;; The workspace is switched first, then the shell starts in the project.
        (agent-git-new-claude-in-project)
        (should (equal switched wt))
        (should (equal started (list 'claude wt)))
        (agent-git-new-codex-in-project)
        (should (equal started (list 'codex wt))))
      (cl-letf (((symbol-function 'agent-git--projectile-projects) #'ignore))
        (should-error (agent-git-new-claude-in-project) :type 'user-error)))))

;;;; /resume

(ert-deftest agent-git-test-resume-query ()
  (should (equal (agent-git--resume-query "/resume") ""))
  (should (equal (agent-git--resume-query "  /resume  \n") ""))
  (should (equal (agent-git--resume-query "/resume  KZN-1 fix ") "KZN-1 fix"))
  (should-not (agent-git--resume-query "/resumed"))
  (should-not (agent-git--resume-query "please /resume"))
  (should-not (agent-git--resume-query "/resume\nand more"))
  (should-not (agent-git--resume-query nil)))

(defvar shell-maker--config nil)

(ert-deftest agent-git-test-resume-command-advice ()
  (let (passed finished scheduled)
    (cl-letf (((symbol-function 'agent-shell--finish-output)
               (lambda (&rest args) (setq finished args)))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat fn &rest args) (setq scheduled (cons fn args))))
              ((symbol-function 'agent-git-resume-in-shell)
               (lambda (buffer query) (list 'resumed buffer query))))
      (let ((fn (lambda (&rest args) (setq passed args) 'sent)))
        (with-temp-buffer
          (setq-local agent-shell--state '((:session . ((:id . "s1")))))
          ;; /resume with a live session: handled here, never sent.
          (agent-git-resume-command-a fn :command "/resume bug" :shell-buffer (current-buffer))
          (should-not passed)
          (should (equal finished (list :config nil :success t)))
          (should (equal (funcall (car scheduled)) (list 'resumed (current-buffer) "bug")))
          ;; Anything else goes to the agent.
          (should (eq (agent-git-resume-command-a fn :command "/review" :shell-buffer (current-buffer))
                      'sent))
          (should (equal passed (list :command "/review" :shell-buffer (current-buffer))))
          ;; Before the session is up, /resume goes through untouched too.
          (setq passed nil finished nil)
          (setq-local agent-shell--state '((:session . ((:id . nil)))))
          (agent-git-resume-command-a fn :command "/resume" :shell-buffer (current-buffer))
          (should passed)
          (should-not finished))))))

(ert-deftest agent-git-test-resume-pick ()
  (let ((sessions '(((sessionId . "old") (title . "Old"))
                    ((sessionId . "now") (title . "Current"))
                    ((sessionId . "mid") (title . "Middle"))))
        restarted offered initial)
    (cl-letf (((symbol-function 'agent-shell--sort-sessions-by-recency) #'identity)
              ((symbol-function 'agent-shell--session-selection-columns) (lambda () '(title)))
              ((symbol-function 'agent-shell--session-column-value)
               (lambda (_column session) (map-elt session 'title)))
              ((symbol-function 'agent-shell--session-choice-label)
               (cl-function
                (lambda (&key acp-session max-widths)
                  (format "%s|%d" (map-elt acp-session 'title) (map-elt max-widths 'title)))))
              ((symbol-function 'completing-read)
               (lambda (_prompt table _pred _require init)
                 (setq offered (all-completions "" table) initial init)
                 (seq-find (lambda (s) (string-prefix-p "Middle|6" s)) offered)))
              ((symbol-function 'agent-shell-restart)
               (cl-function
                (lambda (&key session-id) (setq restarted (list session-id (current-buffer)))))))
      (with-temp-buffer
        (agent-git--resume-pick (current-buffer) "now" sessions "mid")
        ;; The shell's own session isn't offered; the choice resumes in it.
        (should (equal offered '("Old|6" "Middle|6")))
        (should (equal initial "mid"))
        (should (equal restarted (list "mid" (current-buffer))))
        ;; An empty query starts the picker empty.
        (agent-git--resume-pick (current-buffer) "now" sessions "")
        (should-not initial)
        ;; A session another shell runs is marked, and picking it shows
        ;; that shell instead of opening the session twice.
        (setq restarted nil)
        (let ((other (generate-new-buffer " other-shell"))
              visited)
          (unwind-protect
              (cl-letf (((symbol-function 'agent-git--shells)
                         (lambda () (list (current-buffer) other)))
                        ((symbol-function 'agent-git-visit-shell)
                         (lambda (shell) (setq visited shell))))
                (with-current-buffer other
                  (setq-local agent-shell--state '((:session . ((:id . "mid"))))))
                (let ((inhibit-message t))
                  (agent-git--resume-pick (current-buffer) "now" sessions ""))
                (should (equal (mapcar #'substring-no-properties offered)
                               '("Old|6" "Middle|6  [open in a shell]")))
                (should (eq visited other))
                (should-not restarted))
            (kill-buffer other)))
        ;; Nothing else to resume: no picker, no restart.
        (setq restarted nil offered nil)
        (let ((inhibit-message t))
          (agent-git--resume-pick (current-buffer) "now" (list (nth 1 sessions)) ""))
        (should-not offered)
        (should-not restarted)))))

(ert-deftest agent-git-test-new-worktree-creates-branch ()
  (agent-git-test--with-repos
    (let ((default-directory main)
          opened)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _)
                   (if (string-prefix-p "Branch type" prompt) "feature" "develop")))
                ((symbol-function 'read-string) (lambda (&rest _) "new-thing"))
                ((symbol-function 'read-directory-name)
                 (lambda (_prompt dir _default _mustmatch initial)
                   (expand-file-name initial dir)))
                ((symbol-function 'agent-git-open-worktree)
                 (lambda (root &optional new-shell) (setq opened (list root new-shell))))
                ((symbol-function 'agent-git--current-root) (lambda () main)))
        (agent-git-new-worktree))
      (let ((path (expand-file-name "webapp_feature-new-thing" base)))
        (should (equal opened (list path t)))
        (should (equal (agent-git-context-branch (agent-git-context path))
                       "feature/new-thing"))))))

;;;; Killing a shell

(define-derived-mode agent-git-test-shell-mode fundamental-mode "Test shell"
  "Stands in for `agent-shell-mode'.")
(put 'agent-git-test-shell-mode 'derived-mode-parent 'agent-shell-mode)

(ert-deftest agent-git-test-kill-shell ()
  ;; No `let' of `agent-shell--state' here: it isn't special without
  ;; agent-shell loaded, so the binding would be lexical, and since Emacs 31
  ;; `setq-local' (a `setq') would set that instead of the shell's own value.
  (let ((status 'ready)
        asked)
    (cl-letf (((symbol-function 'agent-git--status) (lambda (_) status))
              ((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) nil)))
      (let ((shell (generate-new-buffer "Claude @ proj")))
        (with-current-buffer shell
          (agent-git-test-shell-mode)
          ;; agent-shell's own question, which must not run as well.
          (add-hook 'kill-buffer-query-functions (lambda () (error "Asked again")) nil t)
          (setq-local agent-shell--state (list (cons :pending-prompts '("a" "b")))))
        ;; Working with prompts queued: one question, declined.
        (setq status 'busy)
        (should-error (agent-git-kill-shell shell) :type 'user-error)
        (should (equal asked '("Claude @ proj is working and has 2 prompts queued; kill it? ")))
        (should (buffer-live-p shell))
        ;; Idle and nothing queued: killed without a question.
        (setq status 'ready asked nil)
        (with-current-buffer shell (setq-local agent-shell--state nil))
        (agent-git-kill-shell shell)
        (should-not asked)
        (should-not (buffer-live-p shell))))
    ;; Anything else isn't a shell.
    (with-temp-buffer
      (should-error (agent-git-kill-shell) :type 'user-error))))

(ert-deftest agent-git-test-kill-shell-restores-window ()
  (cl-letf (((symbol-function 'agent-git--status) (lambda (_) 'ready)))
    (let ((before (get-buffer-create " *agent-git-test-before*"))
          (shell (generate-new-buffer "Codex @ proj")))
      (with-current-buffer shell (agent-git-test-shell-mode))
      (switch-to-buffer before)
      ;; Shown in a window opened for it: the window closes.
      (let ((window (display-buffer shell '(display-buffer-pop-up-window))))
        (should (window-live-p window))
        (with-selected-window window (agent-git-kill-shell))
        (should-not (window-live-p window))
        (should (eq (window-buffer) before)))
      (kill-buffer before))))

;;;; Shared files and removing worktrees

(defun agent-git-test--write (file &optional text)
  "Write TEXT (default empty) to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert (or text ""))))

(defun agent-git-test--add-worktree (main base name branch)
  "Add worktree BASE/NAME of MAIN on new BRANCH from develop; return its root."
  (let ((root (expand-file-name (concat name "/") base)))
    (agent-git-test--git main "worktree" "add" "-q" "-b" branch
                         (directory-file-name root) "develop")
    root))

(defmacro agent-git-test--picking (root &rest body)
  "Run BODY with pickers taking their first choice and ROOT as this worktree.
Questions are answered yes; the last one asked is in `asked'."
  (declare (indent 1))
  `(let (asked)
     (cl-letf (((symbol-function 'agent-git--current-root) (lambda () ,root))
               ((symbol-function 'completing-read)
                (lambda (_prompt table &rest _) (car (all-completions "" table))))
               ((symbol-function 'yes-or-no-p) (lambda (prompt) (setq asked prompt) t)))
       ,@body)))

(ert-deftest agent-git-test-link-shared-files ()
  (agent-git-test--with-repos
    (agent-git-test--write (expand-file-name ".gitignore" main) ".claude/\nAGENTS.md\n")
    (agent-git-test--git main "add" ".gitignore")
    (agent-git-test--git main "commit" "-q" "-m" "ignore")
    (dolist (file '(".claude/CLAUDE.md" "AGENTS.md" "NOTES.md"))
      (agent-git-test--write (expand-file-name file main) file))
    (let ((agent-git-worktree-shared-files
           '(("/elsewhere/" "NOTES.md")
             ("/webapp/\\'" ".claude/CLAUDE.md" "AGENTS.md" "NOTES.md" "missing.md")))
          (wt (agent-git-test--add-worktree main base "webapp_x" "feature/x")))
      ;; Ignored files the main worktree has; NOTES.md isn't ignored, missing.md is missing.
      (should (equal (agent-git-link-shared-files wt) '(".claude/CLAUDE.md" "AGENTS.md")))
      (let ((link (expand-file-name ".claude/CLAUDE.md" wt)))
        (should (file-symlink-p link))
        (should (file-equal-p (file-truename link) (expand-file-name ".claude/CLAUDE.md" main))))
      (should (equal (agent-git-test--slurp (expand-file-name "AGENTS.md" wt)) "AGENTS.md"))
      (should (equal (agent-git-test--git wt "status" "--porcelain") ""))
      ;; Linked already: nothing to do.
      (should-not (agent-git-link-shared-files wt))
      ;; A worktree's own copy stays.
      (let ((own (agent-git-test--add-worktree main base "webapp_y" "feature/y")))
        (agent-git-test--write (expand-file-name "AGENTS.md" own) "mine")
        (should (equal (agent-git-link-shared-files own) '(".claude/CLAUDE.md")))
        (should-not (file-symlink-p (expand-file-name "AGENTS.md" own))))
      ;; Not the main worktree, nor a branch where git doesn't ignore them.
      (should-not (agent-git-link-shared-files main))
      (should-not (agent-git-link-shared-files (expand-file-name "webapp_worktree1/" base))))))

(ert-deftest agent-git-test-removal-state ()
  (agent-git-test--with-repos
    (let* ((agent-git-base-branch-alist '(("." . "develop")))
           (wt (expand-file-name "webapp_worktree1/" base))
           (removal (lambda () (agent-git--removal (agent-git-context wt)))))
      (agent-git-test--write (expand-file-name ".git/info/exclude" main)
                             "plans/\n__pycache__/\n*.pyc\nlink.md\n")
      (agent-git-test--write (expand-file-name "plans/a.md" wt))
      (agent-git-test--write (expand-file-name "src/__pycache__/m.pyc" wt))
      (make-symbolic-link (expand-file-name "README" main) (expand-file-name "link.md" wt))
      ;; Only the ignored files worth a mention; no commits beyond develop.
      (let ((r (funcall removal)))
        (should (equal (plist-get r :kept) '("plans/")))
        (should-not (plist-get r :changes))
        (should (equal (plist-get r :branch) "bug/KZN-1-max-length"))
        (should (eql (plist-get r :unsaved) 0))
        (should (string-match-p "deleting ignored plans/, deleting branch bug/KZN-1-max-length\\? \\'"
                                (agent-git--describe-removal r))))
      ;; A commit only here keeps the branch...
      (agent-git-test--git wt "commit" "-q" "--allow-empty" "-m" "work")
      (let ((r (funcall removal)))
        (should (eql (plist-get r :unsaved) 1))
        (should (string-match-p "keeping branch bug/KZN-1-max-length (1 commit only there)"
                                (agent-git--describe-removal r))))
      ;; ...until it's pushed.
      (agent-git-test--git main "remote" "add" "origin" "/nonexistent")
      (agent-git-test--git main "config" "branch.bug/KZN-1-max-length.remote" "origin")
      (agent-git-test--git main "config" "branch.bug/KZN-1-max-length.merge"
                           "refs/heads/bug/KZN-1-max-length")
      (agent-git-test--git main "update-ref" "refs/remotes/origin/bug/KZN-1-max-length"
                           "bug/KZN-1-max-length")
      (should (eql (plist-get (funcall removal) :unsaved) 0))
      ;; Untracked files count as changes, unless they're tool leftovers.
      (agent-git-test--write (expand-file-name "new.txt" wt))
      (agent-git-test--write (expand-file-name ".serena/project.yml" wt))
      (let ((r (funcall removal)))
        (should (equal (plist-get r :changes) '("?? new.txt")))
        (should (equal (plist-get r :junk) '(".serena/project.yml"))))
      ;; Detached: no branch to delete.
      (agent-git-test--git wt "checkout" "-q" "--detach")
      (should-not (plist-get (funcall removal) :branch)))))

(ert-deftest agent-git-test-remove-worktree ()
  (agent-git-test--with-repos
    (let* ((agent-git-base-branch-alist '(("." . "develop")))
           (wt (expand-file-name "webapp_worktree1/" base))
           (file (expand-file-name "notes.txt" wt))
           (hooked nil)
           (agent-git-remove-worktree-functions
            (list (lambda (root) (setq hooked (list root (file-directory-p root)))))))
      (agent-git-test--write (expand-file-name ".git/info/exclude" main) "notes.txt\n")
      (agent-git-test--write file "x")
      ;; What Serena's MCP server leaves, untracked: no reason to stop.
      (agent-git-test--write (expand-file-name ".serena/project.yml" wt) "x")
      (let ((buffer (find-file-noselect file))
            (special (get-buffer-create " *agent-git-test-dashboard*"))
            (inhibit-message t))
        (with-current-buffer special (setq default-directory wt))
        (agent-git-test--picking wt
          ;; A modified buffer there stops it.
          (with-current-buffer buffer (insert "y"))
          (should-error (agent-git-remove-worktree) :type 'user-error)
          (should (file-exists-p wt))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (agent-git-remove-worktree)
          (should (string-match-p "\\`Remove webapp:worktree1 " asked))
          (should (string-match-p "deleting ignored notes.txt" asked)))
        (should (equal hooked (list wt t)))
        (should-not (buffer-live-p buffer))
        ;; Special buffers stay, moved to the main worktree.
        (should (equal (buffer-local-value 'default-directory special) main))
        (kill-buffer special)
        (should-not (file-exists-p wt))
        (should-not (member "bug/KZN-1-max-length" (car (agent-git--refs main))))))))

(ert-deftest agent-git-test-remove-keeps-work ()
  (agent-git-test--with-repos
    (let ((agent-git-base-branch-alist '(("." . "develop")))
          (wt (expand-file-name "webapp_worktree1/" base))
          (other (expand-file-name ".claude/worktrees/webapp-pr-review-abc/" main))
          (inhibit-message t))
      (agent-git-test--write (expand-file-name "new.txt" wt) "x")
      (agent-git-test--picking wt
        ;; Untracked files stop it without C-u.
        (should-error (agent-git-remove-worktree) :type 'user-error)
        (should (file-exists-p wt))
        ;; Committed work develop lacks: the worktree goes, the branch stays.
        (agent-git-test--git wt "add" "new.txt")
        (agent-git-test--git wt "commit" "-q" "-m" "work")
        (agent-git-remove-worktree)
        (should-not (file-exists-p wt))
        (should (member "bug/KZN-1-max-length" (car (agent-git--refs main)))))
      ;; C-u discards uncommitted changes.
      (agent-git-test--write (expand-file-name "scratch.txt" other) "x")
      (agent-git-test--picking other
        (agent-git-remove-worktree t)
        (should (string-match-p "discarding 1 uncommitted change" asked)))
      (should-not (file-exists-p other)))))

(ert-deftest agent-git-test-clean-worktrees ()
  (agent-git-test--with-repos
    (let* ((agent-git-base-branch-alist '(("." . "develop")))
           (wip (agent-git-test--add-worktree main base "webapp_wip" "feature/wip"))
           (dirty (agent-git-test--add-worktree main base "webapp_dirty" "feature/dirty"))
           (merged (agent-git-test--add-worktree main base "webapp_merged" "feature/merged"))
           (inhibit-message t)
           offered)
      ;; wip: a commit develop lacks.  dirty: nothing beyond develop, but changes.
      (agent-git-test--git wip "commit" "-q" "--allow-empty" "-m" "wip")
      (agent-git-test--write (expand-file-name "x.txt" dirty) "x")
      ;; merged: a commit develop lacks, its upstream gone (a squash-merged PR).
      (agent-git-test--git merged "commit" "-q" "--allow-empty" "-m" "done")
      (agent-git-test--git main "remote" "add" "origin" "/nonexistent")
      (agent-git-test--git main "config" "branch.feature/merged.remote" "origin")
      (agent-git-test--git main "config" "branch.feature/merged.merge" "refs/heads/feature/merged")
      (cl-letf (((symbol-function 'agent-git--current-root) (lambda () main))
                ((symbol-function 'map-y-or-n-p)
                 (lambda (prompter actor items &rest _)
                   (dolist (item items)
                     (push (funcall prompter item) offered)
                     (funcall actor item)))))
        (agent-git-clean-worktrees))
      (should (= (length offered) 3))
      (should (seq-find (lambda (q) (string-match-p "\\`Remove webapp:worktree1 \\[nothing beyond develop\\].*deleting branch" q))
                        offered))
      (should (seq-find (lambda (q) (string-match-p "\\`Remove webapp:pr-review-abc " q)) offered))
      (should (seq-find (lambda (q) (string-match-p "\\`Remove webapp:merged \\[upstream gone\\].*keeping branch feature/merged (1 commit only there)" q))
                        offered))
      (should-not (file-exists-p merged))
      (should (member "feature/merged" (car (agent-git--refs main))))
      (should (file-exists-p wip))
      (should (file-exists-p dirty)))))

;;;; Sessions of every project

(defun agent-git-test--message (cwd branch text)
  "Return a Claude message record saying TEXT in CWD on BRANCH."
  `((parentUuid . :null) (type . "user")
    (message . ((role . "user") (content . ,text)))
    (cwd . ,(directory-file-name cwd)) (gitBranch . ,branch)))

(defun agent-git-test--record (type key value)
  "Return a Claude metadata record of TYPE holding KEY: VALUE."
  (list (cons 'type type) (cons key value) (cons 'sessionId "x")))

(defun agent-git-test--session-file (config key id records mtime)
  "Write session ID of account CONFIG in projects/KEY as RECORDS, at MTIME."
  (let ((file (expand-file-name (format "projects/%s/%s.jsonl" key id) config)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (dolist (record records)
        (insert (json-serialize record) "\n")))
    (set-file-times file (seconds-to-time mtime))
    file))

(defmacro agent-git-test--with-sessions (&rest body)
  "Run BODY with Claude session files under temp accounts `home' and `work'.
Sessions, newest first:
  w1  work account, in `dir-a', titled \"Work thing\"
  s2  in `dir-b', untitled, last prompt \"second\", says \"alpha only\"
  s3  names a directory but has no prompt: never listed
  s1  in `dir-a' on main, renamed \"Renamed\", says \"alpha beta\"
  s4  in a directory that is gone
A subagent's file under s1 is never listed either.  `s2-file' is s2's."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (file-truename (make-temp-file "agent-git-sessions" t))))
          (home (expand-file-name "home/" root))
          (work (expand-file-name "work/" root))
          (dir-a (expand-file-name "a/" root))
          (dir-b (expand-file-name "b/" root))
          (process-environment (cons (concat "CLAUDE_CONFIG_DIR=" home) process-environment))
          (agent-git-claude-config-dirs (list (cons "/matches-nothing/" work)))
          (agent-git-session-ignored-dirs nil)
          (agent-git--session-cache (make-hash-table :test #'equal))
          (s2-file nil))
     (ignore s2-file)
     (unwind-protect
         (progn
           (make-directory dir-a t)
           (make-directory dir-b t)
           (agent-git-test--session-file
            home "-a" "s1"
            (list (agent-git-test--message dir-a "main" "alpha beta")
                  (agent-git-test--record "ai-title" 'aiTitle "First title")
                  (agent-git-test--record "last-prompt" 'lastPrompt "do it")
                  (agent-git-test--record "custom-title" 'customTitle "Renamed")
                  (agent-git-test--message dir-a "other-branch" "more")
                  (agent-git-test--record "last-prompt" 'lastPrompt "do it again"))
            1000)
           (agent-git-test--session-file
            home "-a/s1/subagents" "agent-x"
            (list (agent-git-test--message dir-a "main" "alpha beta")
                  (agent-git-test--record "last-prompt" 'lastPrompt "sub"))
            1100)
           (setq s2-file (agent-git-test--session-file
                          home "-b" "s2"
                          (list (agent-git-test--message dir-b "dev" "alpha only")
                                (agent-git-test--record "last-prompt" 'lastPrompt "second"))
                          2000))
           (agent-git-test--session-file
            home "-b" "s3" (list (agent-git-test--message dir-b "dev" "no prompt")) 1500)
           (agent-git-test--session-file
            home "-gone" "s4"
            (list (agent-git-test--message (expand-file-name "gone/" root) "x" "old")
                  (agent-git-test--record "last-prompt" 'lastPrompt "old"))
            500)
           (agent-git-test--session-file
            work "-a" "w1"
            (list (agent-git-test--message dir-a "main" "for the company")
                  (agent-git-test--record "ai-title" 'aiTitle "Work thing")
                  (agent-git-test--record "last-prompt" 'lastPrompt "ship it"))
            3000)
           ,@body)
       (delete-directory root t))))

(defun agent-git-test--ids (sessions)
  "Return the ids of SESSIONS."
  (mapcar (lambda (session) (plist-get session :id)) sessions))

(ert-deftest agent-git-test-claude-sessions ()
  (agent-git-test--with-sessions
    (let* ((sessions (agent-git--claude-sessions))
           (s1 (nth 2 sessions)))
      (should (equal (agent-git-test--ids sessions) '("w1" "s2" "s1" "s4")))
      (should (equal (plist-get s1 :cwd) dir-a))
      ;; The branch it started on; a rename beats the generated title; the
      ;; latest prompt.
      (should (equal (plist-get s1 :branch) "main"))
      (should (equal (plist-get s1 :title) "Renamed"))
      (should (equal (plist-get s1 :prompt) "do it again"))
      (should-not (plist-get (nth 1 sessions) :title))
      ;; Each session knows its account.
      (should (eq (agent-git--session-config s1) 'default))
      (should (equal (agent-git--session-config (car sessions)) work))
      ;; Sessions whose directory is gone, or is ignored, aren't offered.
      (should (equal (agent-git-test--ids (seq-filter #'agent-git--session-listed-p sessions))
                     '("w1" "s2" "s1")))
      (let ((agent-git-session-ignored-dirs (regexp-quote dir-b)))
        (should (equal (agent-git-test--ids (seq-filter #'agent-git--session-listed-p sessions))
                       '("w1" "s1")))))))

(ert-deftest agent-git-test-claude-sessions-cache ()
  (agent-git-test--with-sessions
    (let ((scan (symbol-function 'agent-git--scan-session-files))
          (inhibit-message t)
          scanned)
      (cl-letf (((symbol-function 'agent-git--scan-session-files)
                 (lambda (files)
                   (push (sort (mapcar #'file-name-base files) #'string<) scanned)
                   (funcall scan files))))
        (agent-git--claude-sessions)
        (should (equal scanned '(("s1" "s2" "s3" "s4" "w1"))))
        ;; Nothing changed: nothing is read again.
        (setq scanned nil)
        (agent-git--claude-sessions)
        (should-not scanned)
        ;; A session that grew is read again, alone.
        (append-to-file (concat (json-serialize (agent-git-test--record
                                                 "last-prompt" 'lastPrompt "third"))
                                "\n")
                        nil s2-file)
        (set-file-times s2-file (seconds-to-time 5000))
        (let ((sessions (agent-git--claude-sessions)))
          (should (equal scanned '(("s2"))))
          (should (equal (agent-git-test--ids sessions) '("s2" "w1" "s1" "s4")))
          (should (equal (plist-get (car sessions) :prompt) "third")))
        ;; A deleted one is forgotten.
        (delete-file s2-file)
        (should (equal (agent-git-test--ids (agent-git--claude-sessions)) '("w1" "s1" "s4")))
        (should-not (gethash s2-file agent-git--session-cache))))))

(ert-deftest agent-git-test-sessions-containing ()
  (agent-git-test--with-sessions
    (let ((sessions (agent-git--claude-sessions)))
      (should (equal (agent-git-test--ids (agent-git--sessions-containing sessions '("alpha")))
                     '("s2" "s1")))
      ;; Every word, anywhere in the session, in any order.
      (should (equal (agent-git-test--ids (agent-git--sessions-containing sessions '("beta" "alpha")))
                     '("s1")))
      ;; A lower-case word matches any case; one with upper case, only itself.
      (should (equal (agent-git-test--ids (agent-git--sessions-containing sessions '("work")))
                     '("w1")))
      (should-not (agent-git--sessions-containing sessions '("ALPHA")))
      (should-not (agent-git--sessions-containing sessions '("nowhere"))))))

(ert-deftest agent-git-test-find-session ()
  (agent-git-test--with-sessions
    (let (offered prompt resumed)
      (cl-letf (((symbol-function 'agent-git--read)
                 (lambda (text candidates)
                   (setq prompt text
                         offered (mapcar (lambda (c) (substring-no-properties (car c))) candidates))
                   (cddr (car candidates))))
                ((symbol-function 'agent-git--resume-found-session)
                 (lambda (session) (setq resumed (plist-get session :id))))
                ((symbol-function 'agent-git--shells) #'ignore))
        ;; No words: every session, newest first.
        (agent-git-find-session "")
        (should (equal resumed "w1"))
        (should (equal prompt "Resume session: "))
        (should (= (length offered) 3))
        ;; Another account is named; an untitled session shows its prompt once.
        (should (string-match-p "Work thing +ship it +\\[work\\]\\'" (nth 0 offered)))
        (should (string-match-p " dev +second +\\'" (nth 1 offered)))
        (should (string-match-p " main +Renamed +do it again\\'" (nth 2 offered)))
        ;; Words narrow to the sessions that have them all.
        (agent-git-find-session " alpha  beta ")
        (should (equal resumed "s1"))
        (should (equal prompt "Resume session (1 with  alpha  beta ): "))
        (should-error (agent-git-find-session "nowhere") :type 'user-error)))))

(ert-deftest agent-git-test-resume-found-session ()
  (agent-git-test--with-sessions
    (let* ((sessions (agent-git--claude-sessions))
           (find (lambda (id) (seq-find (lambda (s) (equal (plist-get s :id) id)) sessions)))
           (shells nil)
           switched started config)
      (cl-letf (((symbol-function 'agent-shell-anthropic-make-claude-code-config)
                 (lambda ()
                   (list (cons :buffer-name "Claude")
                         (cons :client-maker (lambda (_buffer) agent-git--claude-config-override)))))
                ((symbol-function 'agent-shell--start)
                 (cl-function
                  (lambda (&key ((:config agent-config)) session-id new-session cwd
                                &allow-other-keys)
                    (setq config agent-config
                          started (list :id session-id :cwd cwd :dir default-directory
                                        :new new-session
                                        :naming agent-git--claude-config-override)))))
                ((symbol-function 'agent-git--shells) (lambda () shells))
                ((symbol-function 'agent-git-visit-shell) (lambda (shell) (setq switched shell))))
        (let ((agent-git-switch-project-function (lambda (root) (setq switched root))))
          ;; A session of ~/.claude: its directory's workspace, a shell there.
          (agent-git--resume-found-session (funcall find "s1"))
          (should (equal switched dir-a))
          (should (equal started (list :id "s1" :cwd dir-a :dir dir-a :new t :naming 'default)))
          (should (eq (funcall (map-elt config :client-maker) nil) 'default))
          ;; One of the work account starts on it, and every later client
          ;; (restart, reload) is made on it too.
          (agent-git--resume-found-session (funcall find "w1"))
          (should (equal (plist-get started :naming) work))
          (should (equal (funcall (map-elt config :client-maker) nil) work))
          (should (equal (map-elt config :buffer-name) "Claude"))
          ;; A session a shell runs is shown there, not opened again.
          (setq started nil)
          (let ((shell (generate-new-buffer " shell")))
            (unwind-protect
                (let ((inhibit-message t))
                  (with-current-buffer shell
                    (setq-local agent-shell--state '((:session . ((:id . "s2"))))))
                  (setq shells (list shell))
                  (agent-git--resume-found-session (funcall find "s2"))
                  (should (eq switched shell))
                  (should-not started))
              (kill-buffer shell))))))))

(ert-deftest agent-git-test-claude-config-override ()
  (agent-git-test--with-work-account
    (let ((agent-shell-anthropic-claude-environment '("ENABLE_TOOL_SEARCH=true"))
          (maker (lambda (&rest _) agent-shell-anthropic-claude-environment))
          (agent-git--claude-config-override 'default))
      (with-temp-buffer
        ;; A ~/.claude session resumed in webapp stays off the work account.
        (setq default-directory (expand-file-name "webapp_worktree1/" base))
        (should (equal (agent-git-buffer-name "Claude" "ignored")
                       "Claude @ webapp:worktree1 ⎇ bug/KZN-1-max-length"))
        (should (equal (agent-git-claude-environment-a maker :buffer (current-buffer))
                       '("ENABLE_TOOL_SEARCH=true")))
        (should-not (agent-git--claude-account-here))))))

;;; agent-git-test.el ends here
