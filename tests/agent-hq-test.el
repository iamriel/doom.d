;;; agent-hq-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Uses real git in a temporary directory; agent-shell is not needed.

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../lisp/agent-hq/agent-hq.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun agent-hq-test--git (dir &rest args)
  "Run git ARGS in DIR, failing the test on a non-zero exit."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (let ((status (apply #'call-process "git" nil t nil
                           "-c" "user.name=Test" "-c" "user.email=t@example.com"
                           "-c" "init.defaultBranch=develop" args)))
        (unless (zerop status)
          (error "git %S failed: %s" args (buffer-string)))
        (string-trim (buffer-string))))))

(defmacro agent-hq-test--with-repos (&rest body)
  "Run BODY with `base' bound to a temp dir holding a repo and its worktrees.
Layout:
  base/webapp                      main worktree, branch develop
  base/webapp_worktree1            branch bug/KZN-1-max-length
  base/webapp/.claude/worktrees/webapp-pr-review-abc   branch claude/pr-review
  base/plain                       not a git repo"
  (declare (indent 0))
  `(let* ((base (file-name-as-directory
                 (file-truename (make-temp-file "agent-hq-test" t))))
          (main (expand-file-name "webapp/" base)))
     (unwind-protect
         (progn
           (make-directory main)
           (make-directory (expand-file-name "plain" base))
           (agent-hq-test--git main "init" "-q")
           (agent-hq-test--git main "commit" "-q" "--allow-empty" "-m" "init")
           (agent-hq-test--git main "worktree" "add" "-q" "-b" "bug/KZN-1-max-length"
                                (expand-file-name "webapp_worktree1" base))
           (agent-hq-test--git main "worktree" "add" "-q" "-b" "claude/pr-review"
                                (expand-file-name ".claude/worktrees/webapp-pr-review-abc" main))
           ,@body)
       (delete-directory base t))))

(ert-deftest agent-hq-test-main-worktree ()
  (agent-hq-test--with-repos
    (let ((ctx (agent-hq-context (expand-file-name "webapp/" base))))
      (should (equal (agent-hq-context-repo ctx) "webapp"))
      (should-not (agent-hq-context-worktree ctx))
      (should (equal (agent-hq-context-branch ctx) "develop"))
      (should (equal (agent-hq-context-label ctx) "webapp")))))

(ert-deftest agent-hq-test-sibling-worktree ()
  (agent-hq-test--with-repos
    (let ((ctx (agent-hq-context (expand-file-name "webapp_worktree1/" base))))
      (should (equal (agent-hq-context-repo ctx) "webapp"))
      (should (equal (agent-hq-context-worktree ctx) "worktree1"))
      (should (equal (agent-hq-context-branch ctx) "bug/KZN-1-max-length"))
      (should (equal (agent-hq-context-label ctx) "webapp:worktree1")))))

(ert-deftest agent-hq-test-nested-claude-worktree ()
  (agent-hq-test--with-repos
    (let ((ctx (agent-hq-context
                (expand-file-name ".claude/worktrees/webapp-pr-review-abc/" main))))
      (should (equal (agent-hq-context-label ctx) "webapp:pr-review-abc"))
      (should (equal (agent-hq-context-branch ctx) "claude/pr-review")))))

(ert-deftest agent-hq-test-subdirectory-uses-worktree-root ()
  (agent-hq-test--with-repos
    (let ((sub (expand-file-name "webapp_worktree1/src/" base)))
      (make-directory sub t)
      (should (equal (agent-hq-context-root (agent-hq-context sub))
                     (expand-file-name "webapp_worktree1/" base))))))

(ert-deftest agent-hq-test-not-git ()
  (agent-hq-test--with-repos
    (should-not (agent-hq-context (expand-file-name "plain/" base)))))

(ert-deftest agent-hq-test-detached-and-rebasing ()
  (agent-hq-test--with-repos
    (let ((wt (expand-file-name "webapp_worktree1/" base)))
      (agent-hq-test--git wt "checkout" "-q" "--detach")
      (should (string-prefix-p "detached@" (agent-hq-context-branch (agent-hq-context wt))))
      ;; What git leaves behind mid-rebase: detached HEAD plus head-name.
      (let ((git-dir (agent-hq--git-dir wt)))
        (make-directory (expand-file-name "rebase-merge" git-dir))
        (with-temp-file (expand-file-name "rebase-merge/head-name" git-dir)
          (insert "refs/heads/bug/KZN-1-max-length\n")))
      (should (equal (agent-hq-context-branch (agent-hq-context wt))
                     "bug/KZN-1-max-length (rebasing)")))))

(ert-deftest agent-hq-test-branch-switch-is-seen ()
  (agent-hq-test--with-repos
    (let ((wt (expand-file-name "webapp_worktree1/" base)))
      (agent-hq-test--git wt "checkout" "-q" "-b" "feature/KZN-2-other")
      (should (equal (agent-hq-context-branch (agent-hq-context wt))
                     "feature/KZN-2-other")))))

(ert-deftest agent-hq-test-labels ()
  (should (equal (agent-hq--worktree-label "webapp" "/x/webapp_worktree1/" "/x/webapp/")
                 "worktree1"))
  (should (equal (agent-hq--worktree-label "gymmers-app" "/x/gymmers-app_w1/" "/x/gymmers-app/")
                 "w1"))
  (should (equal (agent-hq--worktree-label "webapp" "/x/hotfix/" "/x/webapp/") "hotfix"))
  ;; Codex app worktrees: ~/.codex/worktrees/<id>/<repo>.
  (should (equal (agent-hq--worktree-label "triply" "/h/.codex/worktrees/08f5/triply/"
                                            "/p/triply/")
                 "08f5")))

(ert-deftest agent-hq-test-buffer-name ()
  (agent-hq-test--with-repos
    (let ((default-directory (expand-file-name "webapp_worktree1/" base)))
      (should (equal (agent-hq-buffer-name "Claude" "ignored")
                     "Claude @ webapp:worktree1 ⎇ bug/KZN-1-max-length")))
    (let ((default-directory (expand-file-name "plain/" base)))
      (should (equal (agent-hq-buffer-name "Codex" "plain") "Codex @ plain")))))

(ert-deftest agent-hq-test-strip-uniquifier ()
  (should (equal (agent-hq--strip-uniquifier "Claude @ webapp ⎇ develop<2>")
                 "Claude @ webapp ⎇ develop"))
  (should (equal (agent-hq--strip-uniquifier "Claude @ webapp") "Claude @ webapp")))

(ert-deftest agent-hq-test-repo-worktrees ()
  (agent-hq-test--with-repos
    (let* ((common (agent-hq-context-common-dir (agent-hq-context main)))
           (labels (mapcar #'agent-hq-context-label (agent-hq--repo-worktrees common))))
      (should (equal labels '("webapp" "webapp:pr-review-abc" "webapp:worktree1"))))
    ;; A worktree deleted without `git worktree remove' is left out.
    (delete-directory (expand-file-name "webapp_worktree1" base) t)
    (let ((common (agent-hq-context-common-dir (agent-hq-context main))))
      (should (equal (mapcar #'agent-hq-context-label (agent-hq--repo-worktrees common))
                     '("webapp" "webapp:pr-review-abc"))))))

(defvar projectile-known-projects nil)

(ert-deftest agent-hq-test-known-repos-dedupe ()
  (agent-hq-test--with-repos
    (let* ((other (expand-file-name "other/" base))
           (projectile-known-projects
            (list (expand-file-name "webapp_worktree1/" base)
                  other
                  main
                  (expand-file-name "plain/" base)
                  "/no/such/dir/")))
      (make-directory other)
      (agent-hq-test--git other "init" "-q")
      (cl-letf (((symbol-function 'agent-hq--shells) #'ignore)
                ((symbol-function 'agent-hq--projectile-projects)
                 (lambda () projectile-known-projects)))
        ;; Starting from the other repo puts it first; webapp's three
        ;; entries collapse into one; non-git and missing dirs drop out.
        (should (equal (mapcar #'agent-hq--repo-name (agent-hq--known-common-dirs other))
                       '("other" "webapp")))))))

(ert-deftest agent-hq-test-worktree-dir ()
  (should (equal (agent-hq--worktree-dir "/p/kizen/webapp/" "webapp" "feature/KZN-15113-start x")
                 "/p/kizen/webapp_feature-KZN-15113-start-x")))

(ert-deftest agent-hq-test-new-branch-name ()
  (let ((agent-hq-branch-prefix-alist '(("/kizen/" . "KZN-"))))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "bug"))
              ((symbol-function 'read-string) (lambda (&rest _) " 19576 text field ")))
      (should (equal (agent-hq--read-new-branch "/p/kizen/webapp/")
                     "bug/KZN-19576-text-field"))
      (should (equal (agent-hq--read-new-branch "/p/gymmers/app/")
                     "bug/19576-text-field")))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) ""))
              ((symbol-function 'read-string) (lambda (&rest _) "obs")))
      (should (equal (agent-hq--read-new-branch "/p/gymmers/app/") "obs")))))

(ert-deftest agent-hq-test-picker-groups-and-duplicates ()
  (let (seen-groups)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (let* ((metadata (funcall table "" nil 'metadata))
                        (group (alist-get 'group-function (cdr metadata)))
                        (strings (all-completions "" table)))
                   (setq seen-groups (mapcar (lambda (s) (funcall group s nil)) strings))
                   (nth 1 strings)))))
      (should (eq (agent-hq--read "x: " '(("same" "webapp" . first)
                                           ("same" "gymmers" . second)))
                  'second))
      (should (equal seen-groups '("webapp" "gymmers"))))))

(defmacro agent-hq-test--with-work-account (&rest body)
  "Run BODY in `agent-hq-test--with-repos' with webapp mapped to a work account."
  (declare (indent 0))
  `(agent-hq-test--with-repos
     (let ((agent-hq-claude-config-dirs
            (list (cons (concat (regexp-quote main) "\\'") "~/.claude-work"))))
       ,@body)))

(ert-deftest agent-hq-test-claude-config-dir ()
  (agent-hq-test--with-work-account
    (let ((work (file-name-as-directory (expand-file-name "~/.claude-work"))))
      ;; Every worktree of webapp, and directories inside them, use the account.
      (dolist (dir (list main
                         (expand-file-name "webapp_worktree1/" base)
                         (expand-file-name ".claude/worktrees/webapp-pr-review-abc/" main)))
        (should (equal (agent-hq-claude-config-dir dir) work)))
      (let ((sub (expand-file-name "webapp_worktree1/src/" base)))
        (make-directory sub t)
        (should (equal (agent-hq-claude-config-dir sub) work)))
      ;; Unmapped and non-git directories keep the default.
      (should-not (agent-hq-claude-config-dir (expand-file-name "plain/" base)))
      (let ((agent-hq-claude-config-dirs nil))
        (should-not (agent-hq-claude-config-dir main))))))

(ert-deftest agent-hq-test-claude-account-label ()
  (cl-letf (((symbol-function 'agent-hq-claude-config-dir)
             (lambda (&optional _) "/h/.claude-work/")))
    (should (equal (agent-hq-claude-account) "work")))
  (cl-letf (((symbol-function 'agent-hq-claude-config-dir)
             (lambda (&optional _) "/h/claude_kizen/")))
    (should (equal (agent-hq-claude-account) "kizen")))
  (cl-letf (((symbol-function 'agent-hq-claude-config-dir)
             (lambda (&optional _) "/h/company/")))
    (should (equal (agent-hq-claude-account) "company")))
  (cl-letf (((symbol-function 'agent-hq-claude-config-dir) #'ignore))
    (should-not (agent-hq-claude-account))))

(ert-deftest agent-hq-test-buffer-name-shows-account ()
  (agent-hq-test--with-work-account
    (let ((default-directory (expand-file-name "webapp_worktree1/" base)))
      (should (equal (agent-hq-buffer-name "Claude" "ignored")
                     "Claude (work) @ webapp:worktree1 ⎇ bug/KZN-1-max-length"))
      ;; Only Claude reads CLAUDE_CONFIG_DIR.
      (should (equal (agent-hq-buffer-name "Codex" "ignored")
                     "Codex @ webapp:worktree1 ⎇ bug/KZN-1-max-length")))
    (let ((default-directory (expand-file-name "plain/" base)))
      (should (equal (agent-hq-buffer-name "Claude" "plain") "Claude @ plain")))))

(defvar agent-shell-anthropic-claude-environment nil)

(ert-deftest agent-hq-test-claude-environment-advice ()
  (agent-hq-test--with-work-account
    (let ((agent-shell-anthropic-claude-environment '("ENABLE_TOOL_SEARCH=true"))
          (maker (lambda (&rest _) agent-shell-anthropic-claude-environment)))
      (with-temp-buffer
        (setq default-directory (expand-file-name "webapp_worktree1/" base))
        (should (equal (agent-hq-claude-environment-a maker :buffer (current-buffer))
                       (list (concat "CLAUDE_CONFIG_DIR="
                                     (expand-file-name "~/.claude-work"))
                             "ENABLE_TOOL_SEARCH=true"))))
      ;; Elsewhere the environment is passed through untouched.
      (with-temp-buffer
        (setq default-directory (expand-file-name "plain/" base))
        (should (equal (agent-hq-claude-environment-a maker :buffer (current-buffer))
                       '("ENABLE_TOOL_SEARCH=true"))))
      ;; The binding doesn't leak.
      (should (equal agent-shell-anthropic-claude-environment '("ENABLE_TOOL_SEARCH=true"))))))

(ert-deftest agent-hq-test-label-follows-started-account ()
  (agent-hq-test--with-work-account
    (let ((maker (lambda (&rest _) nil))
          (wt (expand-file-name "webapp_worktree1/" base)))
      ;; Started on the work account: keeps the label after the mapping goes.
      (with-temp-buffer
        (setq default-directory wt)
        (agent-hq-claude-environment-a maker :buffer (current-buffer))
        (let ((agent-hq-claude-config-dirs nil))
          (should (equal (agent-hq--agent-display-name "Claude" (agent-hq--claude-account-here))
                         "Claude (work)"))))
      ;; Started on the default account: gains no label once a mapping appears.
      (with-temp-buffer
        (setq default-directory wt)
        (let ((agent-hq-claude-config-dirs nil))
          (agent-hq-claude-environment-a maker :buffer (current-buffer)))
        (should (equal (agent-hq--agent-display-name "Claude" (agent-hq--claude-account-here))
                       "Claude"))))))

(ert-deftest agent-hq-test-new-shell-name-ignores-current-shell ()
  ;; Naming a new shell while another shell is current: the new one's
  ;; directory decides, not the account of the shell the command ran in.
  (agent-hq-test--with-work-account
    (let ((maker (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq default-directory (expand-file-name "webapp_worktree1/" base))
        (agent-hq-claude-environment-a maker :buffer (current-buffer))
        (let ((default-directory (expand-file-name "plain/" base)))
          (should (equal (agent-hq-buffer-name "Claude" "plain") "Claude @ plain"))))
      (with-temp-buffer
        (setq default-directory (expand-file-name "plain/" base))
        (agent-hq-claude-environment-a maker :buffer (current-buffer))
        (let ((default-directory main))
          (should (equal (agent-hq-buffer-name "Claude" "webapp")
                         "Claude (work) @ webapp ⎇ develop")))))))

;;;; Shared project memory

(defmacro agent-hq-test--with-default-config (&rest body)
  "Run BODY with the default account's config dir at base/dot-claude/."
  (declare (indent 0))
  `(let ((process-environment
          (cons (concat "CLAUDE_CONFIG_DIR=" (expand-file-name "dot-claude" base))
                process-environment)))
     ,@body))

(defun agent-hq-test--slurp (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest agent-hq-test-claude-project-key ()
  (should (equal (agent-hq--claude-project-key "/Users/me/projects/kizen/webapp/")
                 "-Users-me-projects-kizen-webapp"))
  (should (equal (agent-hq--claude-project-key "/tmp-x/a_b.c d/")
                 "-tmp-x-a-b-c-d"))
  ;; Claude Code hashes very long names; don't guess those.
  (should-not (agent-hq--claude-project-key (concat "/" (make-string 250 ?a)))))

(ert-deftest agent-hq-test-shared-memory-dir-is-per-repo ()
  (agent-hq-test--with-repos
    (agent-hq-test--with-default-config
      (let ((expected (expand-file-name
                       (concat "dot-claude/projects/"
                               (agent-hq--claude-project-key main) "/memory")
                       base)))
        ;; One memory for the whole repo, as Claude Code keeps it.
        (dolist (dir (list main
                           (expand-file-name "webapp_worktree1/" base)
                           (expand-file-name ".claude/worktrees/webapp-pr-review-abc/" main)))
          (should (equal (agent-hq-claude-shared-memory-dir dir) expected)))))))

(ert-deftest agent-hq-test-share-memory-edits-main-settings-only ()
  (agent-hq-test--with-repos
    (agent-hq-test--with-default-config
      (let* ((settings (expand-file-name ".claude/settings.local.json" main))
             (wt (expand-file-name "webapp_worktree1/" base))
             (wt-settings (expand-file-name ".claude/settings.local.json" wt))
             (memory (agent-hq-claude-shared-memory-dir main)))
        (make-directory (file-name-directory settings) t)
        (make-directory (file-name-directory wt-settings) t)
        (write-region "{\"permissions\": {\"allow\": [\"Bash(ls)\"], \"deny\": []},
 \"env\": {}, \"flag\": false, \"gone\": null}\n" nil settings)
        (write-region "{\"permissions\": {}}\n" nil wt-settings)
        ;; Called from a linked worktree, it edits the main worktree's file.
        (should (equal (agent-hq-claude-share-memory wt) memory))
        (let ((data (agent-hq--read-json settings)))
          (should (equal (mapcar #'car data)
                         '(permissions env flag gone autoMemoryDirectory)))
          (should (equal (alist-get 'autoMemoryDirectory data) memory))
          ;; Everything else survives, including false, null and {}.
          (should (equal (alist-get 'permissions data) '((allow . ["Bash(ls)"]) (deny . []))))
          (should (equal (alist-get 'flag data) :false))
          (should (equal (alist-get 'gone data) :null))
          (should (string-match-p "\"env\": {}" (agent-hq-test--slurp settings))))
        (should (equal (agent-hq-test--slurp wt-settings) "{\"permissions\": {}}\n"))
        ;; Already in place: the file is not rewritten.
        (let ((before (agent-hq-test--slurp settings)))
          (should (equal (agent-hq-claude-share-memory main) memory))
          (should (equal (agent-hq-test--slurp settings) before)))
        ;; A stale value is replaced, not duplicated.
        (write-region "{\"autoMemoryDirectory\": \"/old\", \"x\": 1}" nil settings)
        (agent-hq-claude-share-memory main)
        (should (equal (agent-hq--read-json settings)
                       `((autoMemoryDirectory . ,memory) (x . 1))))))))

(ert-deftest agent-hq-test-share-memory-leaves-files-alone ()
  (agent-hq-test--with-repos
    (agent-hq-test--with-default-config
      (let ((settings (expand-file-name ".claude/settings.local.json" main))
            ;; Keep the user's global excludes (~/.config/git/ignore) out.
            (process-environment (append (list "GIT_CONFIG_GLOBAL=/dev/null"
                                               (concat "XDG_CONFIG_HOME=" base))
                                         process-environment)))
        ;; No file, and git would track one: nothing is created.
        (should-not (agent-hq-claude-share-memory main))
        (should-not (file-exists-p settings))
        ;; Where git ignores it, the file is created.
        (write-region ".claude/settings.local.json\n" nil (expand-file-name ".gitignore" main))
        (should (agent-hq-claude-share-memory main))
        (should (equal (mapcar #'car (agent-hq--read-json settings)) '(autoMemoryDirectory)))
        ;; Unreadable JSON is left as it is.
        (write-region "{ not json" nil settings)
        (let ((inhibit-message t))
          (should-not (agent-hq-claude-share-memory main)))
        (should (equal (agent-hq-test--slurp settings) "{ not json"))
        ;; Outside git there is no repo memory to point at.
        (should-not (agent-hq-claude-share-memory (expand-file-name "plain/" base)))))))

(ert-deftest agent-hq-test-advice-shares-memory-for-mapped-repos ()
  (agent-hq-test--with-work-account
    (agent-hq-test--with-default-config
      (let ((settings (expand-file-name ".claude/settings.local.json" main))
            (maker (lambda (&rest _) nil)))
        (make-directory (file-name-directory settings) t)
        (write-region "{}" nil settings)
        (let ((agent-hq-claude-shared-memory nil))
          (with-temp-buffer
            (setq default-directory (expand-file-name "webapp_worktree1/" base))
            (agent-hq-claude-environment-a maker :buffer (current-buffer)))
          (should (equal (agent-hq-test--slurp settings) "{}")))
        (with-temp-buffer
          (setq default-directory (expand-file-name "webapp_worktree1/" base))
          (agent-hq-claude-environment-a maker :buffer (current-buffer)))
        (should (equal (alist-get 'autoMemoryDirectory (agent-hq--read-json settings))
                       (agent-hq-claude-shared-memory-dir main)))
        ;; Repos on the default account are never touched.
        (let ((agent-hq-claude-config-dirs nil))
          (write-region "{}" nil settings)
          (with-temp-buffer
            (setq default-directory main)
            (agent-hq-claude-environment-a maker :buffer (current-buffer)))
          (should (equal (agent-hq-test--slurp settings) "{}")))))))

;;;; Shells in any project

(ert-deftest agent-hq-test-project-candidates ()
  (agent-hq-test--with-repos
    (let ((wt (expand-file-name "webapp_worktree1/" base))
          (plain (expand-file-name "plain/" base)))
      (cl-letf (((symbol-function 'agent-hq--projectile-projects)
                 (lambda ()
                   (list (abbreviate-file-name wt)
                         main
                         (directory-file-name main) ; same project, no slash
                         plain
                         (expand-file-name "gone/" base)
                         "/ssh:host:/srv/app/"
                         nil))))
        (let ((candidates (agent-hq--project-candidates)))
          ;; Projectile's order, one line per existing local project.
          (should (equal (mapcar #'cddr candidates) (list wt main plain)))
          (should (equal (mapcar #'cadr candidates) '("webapp" "webapp" "other")))
          (should (string-match-p "\\`webapp:worktree1 +bug/KZN-1-max-length "
                                  (substring-no-properties (car (nth 0 candidates)))))
          (should (string-match-p "\\`plain " (substring-no-properties (car (nth 2 candidates))))))))))

(ert-deftest agent-hq-test-start-in-project ()
  (agent-hq-test--with-repos
    (let* ((wt (expand-file-name "webapp_worktree1/" base))
           switched started
           (agent-hq-switch-project-function (lambda (root) (setq switched root))))
      (cl-letf (((symbol-function 'agent-hq--projectile-projects) (lambda () (list main wt)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (seq-find (lambda (s) (string-prefix-p "webapp:worktree1" s))
                             (all-completions "" table))))
                ((symbol-function 'agent-shell-anthropic-start-claude-code)
                 (lambda () (setq started (list 'claude default-directory))))
                ((symbol-function 'agent-shell-openai-start-codex)
                 (lambda () (setq started (list 'codex default-directory)))))
        ;; The workspace is switched first, then the shell starts in the project.
        (agent-hq-new-claude-in-project)
        (should (equal switched wt))
        (should (equal started (list 'claude wt)))
        (agent-hq-new-codex-in-project)
        (should (equal started (list 'codex wt))))
      (cl-letf (((symbol-function 'agent-hq--projectile-projects) #'ignore))
        (should-error (agent-hq-new-claude-in-project) :type 'user-error)))))

;;;; /resume

(ert-deftest agent-hq-test-resume-query ()
  (should (equal (agent-hq--resume-query "/resume") ""))
  (should (equal (agent-hq--resume-query "  /resume  \n") ""))
  (should (equal (agent-hq--resume-query "/resume  KZN-1 fix ") "KZN-1 fix"))
  (should-not (agent-hq--resume-query "/resumed"))
  (should-not (agent-hq--resume-query "please /resume"))
  (should-not (agent-hq--resume-query "/resume\nand more"))
  (should-not (agent-hq--resume-query nil)))

(defvar shell-maker--config nil)

(ert-deftest agent-hq-test-resume-command-advice ()
  (let (passed finished scheduled)
    (cl-letf (((symbol-function 'agent-shell--finish-output)
               (lambda (&rest args) (setq finished args)))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat fn &rest args) (setq scheduled (cons fn args))))
              ((symbol-function 'agent-hq-resume-in-shell)
               (lambda (buffer query) (list 'resumed buffer query))))
      (let ((fn (lambda (&rest args) (setq passed args) 'sent)))
        (with-temp-buffer
          (setq-local agent-shell--state '((:session . ((:id . "s1")))))
          ;; /resume with a live session: handled here, never sent.
          (agent-hq-resume-command-a fn :command "/resume bug" :shell-buffer (current-buffer))
          (should-not passed)
          (should (equal finished (list :config nil :success t)))
          (should (equal (funcall (car scheduled)) (list 'resumed (current-buffer) "bug")))
          ;; Anything else goes to the agent.
          (should (eq (agent-hq-resume-command-a fn :command "/review" :shell-buffer (current-buffer))
                      'sent))
          (should (equal passed (list :command "/review" :shell-buffer (current-buffer))))
          ;; Before the session is up, /resume goes through untouched too.
          (setq passed nil finished nil)
          (setq-local agent-shell--state '((:session . ((:id . nil)))))
          (agent-hq-resume-command-a fn :command "/resume" :shell-buffer (current-buffer))
          (should passed)
          (should-not finished))))))

(ert-deftest agent-hq-test-resume-pick ()
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
        (agent-hq--resume-pick (current-buffer) "now" sessions "mid")
        ;; The shell's own session isn't offered; the choice resumes in it.
        (should (equal offered '("Old|6" "Middle|6")))
        (should (equal initial "mid"))
        (should (equal restarted (list "mid" (current-buffer))))
        ;; An empty query starts the picker empty.
        (agent-hq--resume-pick (current-buffer) "now" sessions "")
        (should-not initial)
        ;; A session another shell runs is marked, and picking it shows
        ;; that shell instead of opening the session twice.
        (setq restarted nil)
        (let ((other (generate-new-buffer " other-shell"))
              visited)
          (unwind-protect
              (cl-letf (((symbol-function 'agent-hq--shells)
                         (lambda () (list (current-buffer) other)))
                        ((symbol-function 'agent-hq-visit-shell)
                         (lambda (shell) (setq visited shell))))
                (with-current-buffer other
                  (setq-local agent-shell--state '((:session . ((:id . "mid"))))))
                (let ((inhibit-message t))
                  (agent-hq--resume-pick (current-buffer) "now" sessions ""))
                (should (equal (mapcar #'substring-no-properties offered)
                               '("Old|6" "Middle|6  [open in a shell]")))
                (should (eq visited other))
                (should-not restarted))
            (kill-buffer other)))
        ;; Nothing else to resume: no picker, no restart.
        (setq restarted nil offered nil)
        (let ((inhibit-message t))
          (agent-hq--resume-pick (current-buffer) "now" (list (nth 1 sessions)) ""))
        (should-not offered)
        (should-not restarted)))))

(ert-deftest agent-hq-test-new-worktree-creates-branch ()
  (agent-hq-test--with-repos
    (let ((default-directory main)
          opened)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _)
                   (if (string-prefix-p "Branch type" prompt) "feature" "develop")))
                ((symbol-function 'read-string) (lambda (&rest _) "new-thing"))
                ((symbol-function 'read-directory-name)
                 (lambda (_prompt dir _default _mustmatch initial)
                   (expand-file-name initial dir)))
                ((symbol-function 'agent-hq-open-worktree)
                 (lambda (root &optional new-shell) (setq opened (list root new-shell))))
                ((symbol-function 'agent-hq--current-root) (lambda () main)))
        (agent-hq-new-worktree))
      (let ((path (expand-file-name "webapp_feature-new-thing" base)))
        (should (equal opened (list path t)))
        (should (equal (agent-hq-context-branch (agent-hq-context path))
                       "feature/new-thing"))))))

;;;; Killing a shell

(define-derived-mode agent-hq-test-shell-mode fundamental-mode "Test shell"
  "Stands in for `agent-shell-mode'.")
(put 'agent-hq-test-shell-mode 'derived-mode-parent 'agent-shell-mode)

;;;; Point when a shell is shown

(defmacro agent-hq-test--with-shown-shell (&rest body)
  "Run BODY with `shell', a stand-in shell, in `window' with point mid-way.
`other' is a buffer that isn't a shell."
  (declare (indent 0))
  `(let ((shell (generate-new-buffer " shell"))
         (other (generate-new-buffer " other"))
         (window (selected-window)))
     (unwind-protect
         (progn
           (with-current-buffer shell
             (agent-hq-test-shell-mode)
             (insert "output\n> "))
           (set-window-buffer window shell)
           (set-window-point window 3)
           ,@body)
       (mapc #'kill-buffer (list shell other)))))

(ert-deftest agent-hq-test-shown-goes-to-prompt ()
  "A shell coming on screen has point at its end, not where it last was."
  (agent-hq-test--with-shown-shell
    (with-current-buffer shell (agent-hq--shown-h window))
    (should (= (window-point window) (with-current-buffer shell (point-max))))
    ;; Since Emacs 31 the hook also runs once the shell has left the window:
    ;; the buffer shown now keeps its point.
    (set-window-buffer window other)
    (with-current-buffer other (insert "text"))
    (set-window-point window 2)
    (with-current-buffer shell (agent-hq--shown-h window))
    (should (= (window-point window) 2))))

(ert-deftest agent-hq-test-shown-keeps-point-once ()
  "`agent-hq-keep-point' leaves point alone the one time, in its window only."
  (agent-hq-test--with-shown-shell
    (with-current-buffer shell
      (setq-local agent-hq-keep-point window)
      (agent-hq--shown-h window))
    (should (= (window-point window) 3))
    (should-not (buffer-local-value 'agent-hq-keep-point shell))
    (with-current-buffer shell (agent-hq--shown-h window))
    (should (= (window-point window) (with-current-buffer shell (point-max))))))

(ert-deftest agent-hq-test-shown-goes-to-permission ()
  "A permission request waiting on you is where point goes instead."
  (agent-hq-test--with-shown-shell
    (let (jumped)
      (cl-letf (((symbol-function 'agent-shell-jump-to-latest-permission-button-row)
                 (lambda () (setq jumped (current-buffer)) (goto-char 2) t)))
        (with-current-buffer shell (agent-hq--shown-h window)))
      (should (eq jumped shell))
      (should (= (window-point window) 2)))))

(ert-deftest agent-hq-test-visit-shown-shell-goes-to-prompt ()
  "Visiting a shell already on screen moves point to its prompt too."
  (agent-hq-test--with-shown-shell
    (set-window-buffer window other)
    (let ((side (split-window window nil 'right)))
      (unwind-protect
          (progn
            (set-window-buffer side shell)
            (set-window-point side 3)
            (agent-hq-visit-shell shell)
            (should (eq (selected-window) side))
            (should (= (window-point side) (with-current-buffer shell (point-max)))))
        (select-window window)
        (delete-window side)))))

(ert-deftest agent-hq-test-kill-shell ()
  ;; No `let' of `agent-shell--state' here: it isn't special without
  ;; agent-shell loaded, so the binding would be lexical, and since Emacs 31
  ;; `setq-local' (a `setq') would set that instead of the shell's own value.
  (let ((status 'ready)
        asked)
    (cl-letf (((symbol-function 'agent-hq--status) (lambda (_) status))
              ((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) nil)))
      (let ((shell (generate-new-buffer "Claude @ proj")))
        (with-current-buffer shell
          (agent-hq-test-shell-mode)
          ;; agent-shell's own question, which must not run as well.
          (add-hook 'kill-buffer-query-functions (lambda () (error "Asked again")) nil t)
          (setq-local agent-shell--state (list (cons :pending-prompts '("a" "b")))))
        ;; Working with prompts queued: one question, declined.
        (setq status 'busy)
        (should-error (agent-hq-kill-shell shell) :type 'user-error)
        (should (equal asked '("Claude @ proj is working and has 2 prompts queued; kill it? ")))
        (should (buffer-live-p shell))
        ;; Idle and nothing queued: killed without a question.
        (setq status 'ready asked nil)
        (with-current-buffer shell (setq-local agent-shell--state nil))
        (agent-hq-kill-shell shell)
        (should-not asked)
        (should-not (buffer-live-p shell))))
    ;; Anything else isn't a shell.
    (with-temp-buffer
      (should-error (agent-hq-kill-shell) :type 'user-error))))

(ert-deftest agent-hq-test-kill-shell-restores-window ()
  (cl-letf (((symbol-function 'agent-hq--status) (lambda (_) 'ready)))
    (let ((before (get-buffer-create " *agent-hq-test-before*"))
          (shell (generate-new-buffer "Codex @ proj")))
      (with-current-buffer shell (agent-hq-test-shell-mode))
      (switch-to-buffer before)
      ;; Shown in a window opened for it: the window closes.
      (let ((window (display-buffer shell '(display-buffer-pop-up-window))))
        (should (window-live-p window))
        (with-selected-window window (agent-hq-kill-shell))
        (should-not (window-live-p window))
        (should (eq (window-buffer) before)))
      (kill-buffer before))))

;;;; Shared files and removing worktrees

(defun agent-hq-test--write (file &optional text)
  "Write TEXT (default empty) to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert (or text ""))))

(defun agent-hq-test--add-worktree (main base name branch)
  "Add worktree BASE/NAME of MAIN on new BRANCH from develop; return its root."
  (let ((root (expand-file-name (concat name "/") base)))
    (agent-hq-test--git main "worktree" "add" "-q" "-b" branch
                         (directory-file-name root) "develop")
    root))

(defmacro agent-hq-test--picking (root &rest body)
  "Run BODY with pickers taking their first choice and ROOT as this worktree.
Questions are answered yes; the last one asked is in `asked'."
  (declare (indent 1))
  `(let (asked)
     (cl-letf (((symbol-function 'agent-hq--current-root) (lambda () ,root))
               ((symbol-function 'completing-read)
                (lambda (_prompt table &rest _) (car (all-completions "" table))))
               ((symbol-function 'yes-or-no-p) (lambda (prompt) (setq asked prompt) t)))
       ,@body)))

(ert-deftest agent-hq-test-link-shared-files ()
  (agent-hq-test--with-repos
    (agent-hq-test--write (expand-file-name ".gitignore" main) ".claude/\nAGENTS.md\n")
    (agent-hq-test--git main "add" ".gitignore")
    (agent-hq-test--git main "commit" "-q" "-m" "ignore")
    (dolist (file '(".claude/CLAUDE.md" "AGENTS.md" "NOTES.md"))
      (agent-hq-test--write (expand-file-name file main) file))
    (let ((agent-hq-worktree-shared-files
           '(("/elsewhere/" "NOTES.md")
             ("/webapp/\\'" ".claude/CLAUDE.md" "AGENTS.md" "NOTES.md" "missing.md")))
          (wt (agent-hq-test--add-worktree main base "webapp_x" "feature/x")))
      ;; Ignored files the main worktree has; NOTES.md isn't ignored, missing.md is missing.
      (should (equal (agent-hq-link-shared-files wt) '(".claude/CLAUDE.md" "AGENTS.md")))
      (let ((link (expand-file-name ".claude/CLAUDE.md" wt)))
        (should (file-symlink-p link))
        (should (file-equal-p (file-truename link) (expand-file-name ".claude/CLAUDE.md" main))))
      (should (equal (agent-hq-test--slurp (expand-file-name "AGENTS.md" wt)) "AGENTS.md"))
      (should (equal (agent-hq-test--git wt "status" "--porcelain") ""))
      ;; Linked already: nothing to do.
      (should-not (agent-hq-link-shared-files wt))
      ;; A worktree's own copy stays.
      (let ((own (agent-hq-test--add-worktree main base "webapp_y" "feature/y")))
        (agent-hq-test--write (expand-file-name "AGENTS.md" own) "mine")
        (should (equal (agent-hq-link-shared-files own) '(".claude/CLAUDE.md")))
        (should-not (file-symlink-p (expand-file-name "AGENTS.md" own))))
      ;; Not the main worktree, nor a branch where git doesn't ignore them.
      (should-not (agent-hq-link-shared-files main))
      (should-not (agent-hq-link-shared-files (expand-file-name "webapp_worktree1/" base))))))

(ert-deftest agent-hq-test-removal-state ()
  (agent-hq-test--with-repos
    (let* ((agent-hq-base-branch-alist '(("." . "develop")))
           (wt (expand-file-name "webapp_worktree1/" base))
           (removal (lambda () (agent-hq--removal (agent-hq-context wt)))))
      (agent-hq-test--write (expand-file-name ".git/info/exclude" main)
                             "plans/\n__pycache__/\n*.pyc\nlink.md\n")
      (agent-hq-test--write (expand-file-name "plans/a.md" wt))
      (agent-hq-test--write (expand-file-name "src/__pycache__/m.pyc" wt))
      (make-symbolic-link (expand-file-name "README" main) (expand-file-name "link.md" wt))
      ;; Only the ignored files worth a mention; no commits beyond develop.
      (let ((r (funcall removal)))
        (should (equal (plist-get r :kept) '("plans/")))
        (should-not (plist-get r :changes))
        (should (equal (plist-get r :branch) "bug/KZN-1-max-length"))
        (should (eql (plist-get r :unsaved) 0))
        (should (string-match-p "deleting ignored plans/, deleting branch bug/KZN-1-max-length\\? \\'"
                                (agent-hq--describe-removal r))))
      ;; A commit only here keeps the branch...
      (agent-hq-test--git wt "commit" "-q" "--allow-empty" "-m" "work")
      (let ((r (funcall removal)))
        (should (eql (plist-get r :unsaved) 1))
        (should (string-match-p "keeping branch bug/KZN-1-max-length (1 commit only there)"
                                (agent-hq--describe-removal r))))
      ;; ...until it's pushed.
      (agent-hq-test--git main "remote" "add" "origin" "/nonexistent")
      (agent-hq-test--git main "config" "branch.bug/KZN-1-max-length.remote" "origin")
      (agent-hq-test--git main "config" "branch.bug/KZN-1-max-length.merge"
                           "refs/heads/bug/KZN-1-max-length")
      (agent-hq-test--git main "update-ref" "refs/remotes/origin/bug/KZN-1-max-length"
                           "bug/KZN-1-max-length")
      (should (eql (plist-get (funcall removal) :unsaved) 0))
      ;; Untracked files count as changes, unless they're tool leftovers.
      (agent-hq-test--write (expand-file-name "new.txt" wt))
      (agent-hq-test--write (expand-file-name ".pytest_cache/README.md" wt))
      (let ((r (funcall removal)))
        (should (equal (plist-get r :changes) '("?? new.txt")))
        (should (equal (plist-get r :junk) '(".pytest_cache/README.md"))))
      ;; Detached: no branch to delete.
      (agent-hq-test--git wt "checkout" "-q" "--detach")
      (should-not (plist-get (funcall removal) :branch)))))

(ert-deftest agent-hq-test-remove-worktree ()
  (agent-hq-test--with-repos
    (let* ((agent-hq-base-branch-alist '(("." . "develop")))
           (wt (expand-file-name "webapp_worktree1/" base))
           (file (expand-file-name "notes.txt" wt))
           (hooked nil)
           (agent-hq-remove-worktree-functions
            (list (lambda (root) (setq hooked (list root (file-directory-p root)))))))
      (agent-hq-test--write (expand-file-name ".git/info/exclude" main) "notes.txt\n")
      (agent-hq-test--write file "x")
      ;; An untracked test cache can go with the worktree.
      (agent-hq-test--write (expand-file-name ".pytest_cache/README.md" wt) "x")
      (let ((buffer (find-file-noselect file))
            (special (get-buffer-create " *agent-hq-test-dashboard*"))
            (inhibit-message t))
        (with-current-buffer special (setq default-directory wt))
        (agent-hq-test--picking wt
          ;; A modified buffer there stops it.
          (with-current-buffer buffer (insert "y"))
          (should-error (agent-hq-remove-worktree) :type 'user-error)
          (should (file-exists-p wt))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (agent-hq-remove-worktree)
          (should (string-match-p "\\`Remove webapp:worktree1 " asked))
          (should (string-match-p "deleting ignored notes.txt" asked)))
        (should (equal hooked (list wt t)))
        (should-not (buffer-live-p buffer))
        ;; Special buffers stay, moved to the main worktree.
        (should (equal (buffer-local-value 'default-directory special) main))
        (kill-buffer special)
        (should-not (file-exists-p wt))
        (should-not (member "bug/KZN-1-max-length" (car (agent-hq--refs main))))))))

(ert-deftest agent-hq-test-remove-keeps-work ()
  (agent-hq-test--with-repos
    (let ((agent-hq-base-branch-alist '(("." . "develop")))
          (wt (expand-file-name "webapp_worktree1/" base))
          (other (expand-file-name ".claude/worktrees/webapp-pr-review-abc/" main))
          (inhibit-message t))
      (agent-hq-test--write (expand-file-name "new.txt" wt) "x")
      (agent-hq-test--picking wt
        ;; Untracked files stop it without C-u.
        (should-error (agent-hq-remove-worktree) :type 'user-error)
        (should (file-exists-p wt))
        ;; Committed work develop lacks: the worktree goes, the branch stays.
        (agent-hq-test--git wt "add" "new.txt")
        (agent-hq-test--git wt "commit" "-q" "-m" "work")
        (agent-hq-remove-worktree)
        (should-not (file-exists-p wt))
        (should (member "bug/KZN-1-max-length" (car (agent-hq--refs main)))))
      ;; C-u discards uncommitted changes.
      (agent-hq-test--write (expand-file-name "scratch.txt" other) "x")
      (agent-hq-test--picking other
        (agent-hq-remove-worktree t)
        (should (string-match-p "discarding 1 uncommitted change" asked)))
      (should-not (file-exists-p other)))))

(ert-deftest agent-hq-test-clean-worktrees ()
  (agent-hq-test--with-repos
    (let* ((agent-hq-base-branch-alist '(("." . "develop")))
           (wip (agent-hq-test--add-worktree main base "webapp_wip" "feature/wip"))
           (dirty (agent-hq-test--add-worktree main base "webapp_dirty" "feature/dirty"))
           (merged (agent-hq-test--add-worktree main base "webapp_merged" "feature/merged"))
           (inhibit-message t)
           offered)
      ;; wip: a commit develop lacks.  dirty: nothing beyond develop, but changes.
      (agent-hq-test--git wip "commit" "-q" "--allow-empty" "-m" "wip")
      (agent-hq-test--write (expand-file-name "x.txt" dirty) "x")
      ;; merged: a commit develop lacks, its upstream gone (a squash-merged PR).
      (agent-hq-test--git merged "commit" "-q" "--allow-empty" "-m" "done")
      (agent-hq-test--git main "remote" "add" "origin" "/nonexistent")
      (agent-hq-test--git main "config" "branch.feature/merged.remote" "origin")
      (agent-hq-test--git main "config" "branch.feature/merged.merge" "refs/heads/feature/merged")
      (cl-letf (((symbol-function 'agent-hq--current-root) (lambda () main))
                ((symbol-function 'map-y-or-n-p)
                 (lambda (prompter actor items &rest _)
                   (dolist (item items)
                     (push (funcall prompter item) offered)
                     (funcall actor item)))))
        (agent-hq-clean-worktrees))
      (should (= (length offered) 3))
      (should (seq-find (lambda (q) (string-match-p "\\`Remove webapp:worktree1 \\[nothing beyond develop\\].*deleting branch" q))
                        offered))
      (should (seq-find (lambda (q) (string-match-p "\\`Remove webapp:pr-review-abc " q)) offered))
      (should (seq-find (lambda (q) (string-match-p "\\`Remove webapp:merged \\[upstream gone\\].*keeping branch feature/merged (1 commit only there)" q))
                        offered))
      (should-not (file-exists-p merged))
      (should (member "feature/merged" (car (agent-hq--refs main))))
      (should (file-exists-p wip))
      (should (file-exists-p dirty)))))

;;;; Sessions of every project

(defun agent-hq-test--message (cwd branch text)
  "Return a Claude message record saying TEXT in CWD on BRANCH."
  `((parentUuid . :null) (type . "user")
    (message . ((role . "user") (content . ,text)))
    (cwd . ,(directory-file-name cwd)) (gitBranch . ,branch)))

(defun agent-hq-test--record (type key value)
  "Return a Claude metadata record of TYPE holding KEY: VALUE."
  (list (cons 'type type) (cons key value) (cons 'sessionId "x")))

(defun agent-hq-test--session-file (config key id records mtime)
  "Write session ID of account CONFIG in projects/KEY as RECORDS, at MTIME."
  (let ((file (expand-file-name (format "projects/%s/%s.jsonl" key id) config)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (dolist (record records)
        (insert (json-serialize record) "\n")))
    (set-file-times file (seconds-to-time mtime))
    file))

(defmacro agent-hq-test--with-sessions (&rest body)
  "Run BODY with Claude session files under temp accounts `home' and `work'.
Sessions, newest first:
  w1  work account, in `dir-a', titled \"Work thing\"
  s2  in `dir-b', untitled, last prompt \"second\", says \"alpha only\"
  s3  names a directory but has no prompt: never listed
  s1  in `dir-a' on main, renamed \"Renamed\", says \"alpha beta\"
  s4  in a directory that is gone
A subagent's file under s1 is never listed either.  `s2-file' is s2's."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (file-truename (make-temp-file "agent-hq-sessions" t))))
          (home (expand-file-name "home/" root))
          (work (expand-file-name "work/" root))
          (dir-a (expand-file-name "a/" root))
          (dir-b (expand-file-name "b/" root))
          (process-environment (cons (concat "CLAUDE_CONFIG_DIR=" home) process-environment))
          (agent-hq-claude-config-dirs (list (cons "/matches-nothing/" work)))
          (agent-hq-session-ignored-dirs nil)
          (agent-hq--session-cache (make-hash-table :test #'equal))
          (s2-file nil))
     (ignore s2-file)
     (unwind-protect
         (progn
           (make-directory dir-a t)
           (make-directory dir-b t)
           (agent-hq-test--session-file
            home "-a" "s1"
            (list (agent-hq-test--message dir-a "main" "alpha beta")
                  (agent-hq-test--record "ai-title" 'aiTitle "First title")
                  (agent-hq-test--record "last-prompt" 'lastPrompt "do it")
                  (agent-hq-test--record "custom-title" 'customTitle "Renamed")
                  (agent-hq-test--message dir-a "other-branch" "more")
                  (agent-hq-test--record "last-prompt" 'lastPrompt "do it again"))
            1000)
           (agent-hq-test--session-file
            home "-a/s1/subagents" "agent-x"
            (list (agent-hq-test--message dir-a "main" "alpha beta")
                  (agent-hq-test--record "last-prompt" 'lastPrompt "sub"))
            1100)
           (setq s2-file (agent-hq-test--session-file
                          home "-b" "s2"
                          (list (agent-hq-test--message dir-b "dev" "alpha only")
                                (agent-hq-test--record "last-prompt" 'lastPrompt "second"))
                          2000))
           (agent-hq-test--session-file
            home "-b" "s3" (list (agent-hq-test--message dir-b "dev" "no prompt")) 1500)
           (agent-hq-test--session-file
            home "-gone" "s4"
            (list (agent-hq-test--message (expand-file-name "gone/" root) "x" "old")
                  (agent-hq-test--record "last-prompt" 'lastPrompt "old"))
            500)
           (agent-hq-test--session-file
            work "-a" "w1"
            (list (agent-hq-test--message dir-a "main" "for the company")
                  (agent-hq-test--record "ai-title" 'aiTitle "Work thing")
                  (agent-hq-test--record "last-prompt" 'lastPrompt "ship it"))
            3000)
           ,@body)
       (delete-directory root t))))

(defun agent-hq-test--ids (sessions)
  "Return the ids of SESSIONS."
  (mapcar (lambda (session) (plist-get session :id)) sessions))

(ert-deftest agent-hq-test-claude-sessions ()
  (agent-hq-test--with-sessions
    (let* ((sessions (agent-hq--claude-sessions))
           (s1 (nth 2 sessions)))
      (should (equal (agent-hq-test--ids sessions) '("w1" "s2" "s1" "s4")))
      (should (equal (plist-get s1 :cwd) dir-a))
      ;; The branch it started on; a rename beats the generated title; the
      ;; latest prompt.
      (should (equal (plist-get s1 :branch) "main"))
      (should (equal (plist-get s1 :title) "Renamed"))
      (should (equal (plist-get s1 :prompt) "do it again"))
      (should-not (plist-get (nth 1 sessions) :title))
      ;; Each session knows its account.
      (should (eq (agent-hq--session-config s1) 'default))
      (should (equal (agent-hq--session-config (car sessions)) work))
      ;; Sessions whose directory is gone, or is ignored, aren't offered.
      (should (equal (agent-hq-test--ids (seq-filter #'agent-hq--session-listed-p sessions))
                     '("w1" "s2" "s1")))
      (let ((agent-hq-session-ignored-dirs (regexp-quote dir-b)))
        (should (equal (agent-hq-test--ids (seq-filter #'agent-hq--session-listed-p sessions))
                       '("w1" "s1")))))))

(ert-deftest agent-hq-test-claude-sessions-cache ()
  (agent-hq-test--with-sessions
    (let ((scan (symbol-function 'agent-hq--scan-session-files))
          (inhibit-message t)
          scanned)
      (cl-letf (((symbol-function 'agent-hq--scan-session-files)
                 (lambda (files)
                   (push (sort (mapcar #'file-name-base files) #'string<) scanned)
                   (funcall scan files))))
        (agent-hq--claude-sessions)
        (should (equal scanned '(("s1" "s2" "s3" "s4" "w1"))))
        ;; Nothing changed: nothing is read again.
        (setq scanned nil)
        (agent-hq--claude-sessions)
        (should-not scanned)
        ;; A session that grew is read again, alone.
        (append-to-file (concat (json-serialize (agent-hq-test--record
                                                 "last-prompt" 'lastPrompt "third"))
                                "\n")
                        nil s2-file)
        (set-file-times s2-file (seconds-to-time 5000))
        (let ((sessions (agent-hq--claude-sessions)))
          (should (equal scanned '(("s2"))))
          (should (equal (agent-hq-test--ids sessions) '("s2" "w1" "s1" "s4")))
          (should (equal (plist-get (car sessions) :prompt) "third")))
        ;; A deleted one is forgotten.
        (delete-file s2-file)
        (should (equal (agent-hq-test--ids (agent-hq--claude-sessions)) '("w1" "s1" "s4")))
        (should-not (gethash s2-file agent-hq--session-cache))))))

(ert-deftest agent-hq-test-sessions-containing ()
  (agent-hq-test--with-sessions
    (let ((sessions (agent-hq--claude-sessions)))
      (should (equal (agent-hq-test--ids (agent-hq--sessions-containing sessions '("alpha")))
                     '("s2" "s1")))
      ;; Every word, anywhere in the session, in any order.
      (should (equal (agent-hq-test--ids (agent-hq--sessions-containing sessions '("beta" "alpha")))
                     '("s1")))
      ;; A lower-case word matches any case; one with upper case, only itself.
      (should (equal (agent-hq-test--ids (agent-hq--sessions-containing sessions '("work")))
                     '("w1")))
      (should-not (agent-hq--sessions-containing sessions '("ALPHA")))
      (should-not (agent-hq--sessions-containing sessions '("nowhere"))))))

(ert-deftest agent-hq-test-find-session ()
  (agent-hq-test--with-sessions
    (let (offered prompt resumed)
      (cl-letf (((symbol-function 'agent-hq--read)
                 (lambda (text candidates)
                   (setq prompt text
                         offered (mapcar (lambda (c) (substring-no-properties (car c))) candidates))
                   (cddr (car candidates))))
                ((symbol-function 'agent-hq--resume-found-session)
                 (lambda (session) (setq resumed (plist-get session :id))))
                ((symbol-function 'agent-hq--shells) #'ignore))
        ;; No words: every session, newest first.
        (agent-hq-find-session "")
        (should (equal resumed "w1"))
        (should (equal prompt "Resume session: "))
        (should (= (length offered) 3))
        ;; Another account is named; an untitled session shows its prompt once.
        (should (string-match-p "Work thing +ship it +\\[work\\]\\'" (nth 0 offered)))
        (should (string-match-p " dev +second +\\'" (nth 1 offered)))
        (should (string-match-p " main +Renamed +do it again\\'" (nth 2 offered)))
        ;; Words narrow to the sessions that have them all.
        (agent-hq-find-session " alpha  beta ")
        (should (equal resumed "s1"))
        (should (equal prompt "Resume session (1 with  alpha  beta ): "))
        (should-error (agent-hq-find-session "nowhere") :type 'user-error)))))

(ert-deftest agent-hq-test-resume-found-session ()
  (agent-hq-test--with-sessions
    (let* ((sessions (agent-hq--claude-sessions))
           (find (lambda (id) (seq-find (lambda (s) (equal (plist-get s :id) id)) sessions)))
           (shells nil)
           switched started config)
      (cl-letf (((symbol-function 'agent-shell-anthropic-make-claude-code-config)
                 (lambda ()
                   (list (cons :buffer-name "Claude")
                         (cons :client-maker (lambda (_buffer) agent-hq--claude-config-override)))))
                ((symbol-function 'agent-shell--start)
                 (cl-function
                  (lambda (&key ((:config agent-config)) session-id new-session cwd
                                &allow-other-keys)
                    (setq config agent-config
                          started (list :id session-id :cwd cwd :dir default-directory
                                        :new new-session
                                        :naming agent-hq--claude-config-override)))))
                ((symbol-function 'agent-hq--shells) (lambda () shells))
                ((symbol-function 'agent-hq-visit-shell) (lambda (shell) (setq switched shell))))
        (let ((agent-hq-switch-project-function (lambda (root) (setq switched root))))
          ;; A session of ~/.claude: its directory's workspace, a shell there.
          (agent-hq--resume-found-session (funcall find "s1"))
          (should (equal switched dir-a))
          (should (equal started (list :id "s1" :cwd dir-a :dir dir-a :new t :naming 'default)))
          (should (eq (funcall (map-elt config :client-maker) nil) 'default))
          ;; One of the work account starts on it, and every later client
          ;; (restart, reload) is made on it too.
          (agent-hq--resume-found-session (funcall find "w1"))
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
                  (agent-hq--resume-found-session (funcall find "s2"))
                  (should (eq switched shell))
                  (should-not started))
              (kill-buffer shell))))))))

(ert-deftest agent-hq-test-claude-config-override ()
  (agent-hq-test--with-work-account
    (let ((agent-shell-anthropic-claude-environment '("ENABLE_TOOL_SEARCH=true"))
          (maker (lambda (&rest _) agent-shell-anthropic-claude-environment))
          (agent-hq--claude-config-override 'default))
      (with-temp-buffer
        ;; A ~/.claude session resumed in webapp stays off the work account.
        (setq default-directory (expand-file-name "webapp_worktree1/" base))
        (should (equal (agent-hq-buffer-name "Claude" "ignored")
                       "Claude @ webapp:worktree1 ⎇ bug/KZN-1-max-length"))
        (should (equal (agent-hq-claude-environment-a maker :buffer (current-buffer))
                       '("ENABLE_TOOL_SEARCH=true")))
        (should-not (agent-hq--claude-account-here))))))

(defmacro agent-hq-test--with-shells (specs &rest body)
  "Run BODY with stub shells; SPECS is a list of (VAR DIR NAME).
Each VAR is bound to a buffer named NAME working in DIR.
`agent-hq--shells' returns them in reverse, so order has to be made."
  (declare (indent 1))
  `(let* (,@(mapcar (lambda (spec)
                      `(,(car spec) (with-current-buffer (generate-new-buffer ,(nth 2 spec))
                                      (setq default-directory
                                            (file-name-as-directory ,(nth 1 spec)))
                                      (current-buffer))))
                    specs)
          (all (list ,@(mapcar #'car specs))))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-hq--shells) (lambda () (reverse all)))
                   ((symbol-function 'agent-hq-refresh-buffer-name) #'ignore))
           ,@body)
       (mapc #'kill-buffer all))))

(ert-deftest agent-hq-test-repo-shells ()
  (agent-hq-test--with-repos
    (let ((worktree1 (expand-file-name "webapp_worktree1/" base))
          (review (expand-file-name ".claude/worktrees/webapp-pr-review-abc/" main))
          (plain (expand-file-name "plain/" base)))
      (agent-hq-test--with-shells ((main-b main " b main")
                                    (main-a main " a main")
                                    (wt worktree1 " wt1")
                                    (rev review " review")
                                    (other plain " plain"))
        ;; From any worktree: every worktree's shells, main first, then
        ;; worktrees and shells by name.  The non-git shell isn't one.
        (dolist (dir (list main worktree1 (expand-file-name "src/" review)))
          (should (equal (agent-hq--repo-shells dir) (list main-a main-b rev wt))))
        ;; Outside git, the shells of that directory.
        (should (equal (agent-hq--repo-shells plain) (list other)))))))

(ert-deftest agent-hq-test-cycle-shell ()
  (agent-hq-test--with-repos
    (let ((worktree1 (expand-file-name "webapp_worktree1/" base))
          (plain (expand-file-name "plain/" base))
          visited)
      (agent-hq-test--with-shells ((a main " a main")
                                    (b main " b main")
                                    (wt worktree1 " wt1")
                                    (other plain " plain"))
        (cl-letf (((symbol-function 'agent-hq-visit-shell)
                   (lambda (shell) (setq visited shell))))
          (let ((inhibit-message t))
            (cl-flet ((from (buffer fn &optional n)
                        (setq visited nil)
                        (with-current-buffer buffer (funcall fn n))
                        visited))
              ;; Forward and back, wrapping, across worktrees.
              (should (eq (from a #'agent-hq-next-shell) b))
              (should (eq (from b #'agent-hq-next-shell) wt))
              (should (eq (from wt #'agent-hq-next-shell) a))
              (should (eq (from a #'agent-hq-previous-shell) wt))
              (should (eq (from a #'agent-hq-next-shell 2) wt))
              ;; From a file in the repo: the first, or the last.
              (with-temp-buffer
                (setq default-directory worktree1)
                (should (eq (from (current-buffer) #'agent-hq-next-shell) a))
                (should (eq (from (current-buffer) #'agent-hq-previous-shell) wt)))
              ;; The only shell of its place: nowhere to go.
              (should-error (from other #'agent-hq-next-shell) :type 'user-error)
              (should-not visited)
              ;; From normal state the shell shown goes to normal state too,
              ;; so gt again doesn't type into its prompt.
              (let (normal-in)
                (cl-letf (((symbol-function 'evil-normal-state)
                           (lambda () (setq normal-in (current-buffer)))))
                  (with-current-buffer a (setq-local evil-state 'normal))
                  (should (eq (from a #'agent-hq-next-shell) b))
                  (should (eq normal-in b))
                  (setq normal-in nil)
                  (with-current-buffer b (setq-local evil-state 'insert))
                  (from b #'agent-hq-next-shell)
                  (should-not normal-in))))))))))

(ert-deftest agent-hq-test-switch-repo-shell ()
  (agent-hq-test--with-repos
    (let ((worktree1 (expand-file-name "webapp_worktree1/" base))
          (plain (expand-file-name "plain/" base))
          offered visited)
      (agent-hq-test--with-shells ((m main " main")
                                    (wt worktree1 " wt1")
                                    (other plain " plain"))
        (cl-letf (((symbol-function 'agent-hq-visit-shell)
                   (lambda (shell) (setq visited shell)))
                  ((symbol-function 'agent-hq--read)
                   (lambda (prompt candidates)
                     (setq offered (cons prompt
                                         (mapcar (lambda (c) (cons (cadr c) (cddr c)))
                                                 candidates)))
                     (cddr (car candidates)))))
          ;; From worktree1: its shell first, grouped by worktree, and no
          ;; other repo's shell.
          (with-current-buffer wt
            (agent-hq-switch-repo-shell))
          (should (equal offered `("Agent shell (webapp): "
                                   ("webapp:worktree1" . ,wt) ("webapp" . ,m))))
          (should (eq visited wt))
          (with-temp-buffer
            (setq default-directory main)
            (agent-hq-switch-repo-shell))
          (should (equal (cdr offered) `(("webapp" . ,m) ("webapp:worktree1" . ,wt))))
          ;; The all-projects picker still groups by repo.
          (with-temp-buffer
            (setq default-directory main)
            (agent-hq-switch-shell))
          (should (equal (sort (mapcar #'car (cdr offered)) #'string<)
                         '("other" "webapp" "webapp")))
          ;; Nothing in this repo.
          (kill-buffer m)
          (kill-buffer wt)
          (setq all (list other))
          (with-temp-buffer
            (setq default-directory main)
            (should-error (agent-hq-switch-repo-shell) :type 'user-error)))))))

(ert-deftest agent-hq-test-repo-shells-keep-start-order ()
  (agent-hq-test--with-repos
    (agent-hq-test--with-shells ((late main " a late")
                                  (early main " z early")
                                  (unnumbered main " m"))
      (with-current-buffer early (setq agent-hq--shell-number 1))
      (with-current-buffer late (setq agent-hq--shell-number 2))
      ;; Names change with titles; the order they started in doesn't.
      (should (equal (agent-hq--repo-shells main) (list early late unnumbered))))))

(ert-deftest agent-hq-test-cycle-skips-shells-on-screen ()
  (agent-hq-test--with-repos
    (let (visited)
      (agent-hq-test--with-shells ((a main " a") (b main " b") (c main " c"))
        (dolist (shell all)
          (with-current-buffer shell (agent-hq-test-shell-mode)))
        (cl-letf (((symbol-function 'agent-hq-visit-shell)
                   (lambda (shell) (setq visited shell))))
          (let ((inhibit-message t))
            ;; b is in another window: a's window steps over it.
            (set-window-buffer (selected-window) b)
            (with-current-buffer a (agent-hq-next-shell))
            (should (eq visited c))
            (with-current-buffer c (agent-hq-next-shell))
            (should (eq visited a))
            (with-current-buffer a (agent-hq-previous-shell))
            (should (eq visited c))
            ;; The other shell is on screen: nowhere to go.
            (kill-buffer c)
            (setq all (list a b) visited nil)
            (should-error (with-current-buffer a (agent-hq-next-shell)) :type 'user-error)
            (should-not visited)))))))

;;;; Titles in shell names

(ert-deftest agent-hq-test-name-carries-title ()
  (agent-hq-test--with-repos
    (let ((ctx (agent-hq-context (expand-file-name "webapp_worktree1/" base))))
      (should (equal (agent-hq--format-name "Claude" nil ctx nil "Fix the name field")
                     "Claude @ webapp:worktree1 ⎇ bug/KZN-1-max-length · Fix the name field"))
      (should (equal (agent-hq--format-name "Codex" nil nil "plain" "") "Codex @ plain"))
      (let ((agent-hq-title-width 10))
        (should (equal (agent-hq--format-name "Codex" nil nil "plain" "Fix the name field")
                       "Codex @ plain · Fix the n…")))
      (let ((agent-hq-title-width nil))
        (should (equal (agent-hq--format-name "Codex" nil nil "plain" "Fix") "Codex @ plain"))))))

(ert-deftest agent-hq-test-refresh-name-follows-title ()
  (agent-hq-test--with-repos
    (cl-letf (((symbol-function 'shell-maker-set-buffer-name)
               (lambda (buffer name) (with-current-buffer buffer (rename-buffer name t)))))
      (let ((shell (generate-new-buffer "Claude @ webapp ⎇ develop"))
            (viewport (generate-new-buffer "Claude @ webapp ⎇ develop [viewport]")))
        (unwind-protect
            (with-current-buffer shell
              (agent-hq-test-shell-mode)
              (setq default-directory main)
              (setq-local agent-shell--state
                          (list (cons :agent-config (list (cons :buffer-name "Claude")))
                                (cons :session (list (cons :title "Fix login\nand more")))))
              ;; The title's first line goes last, the viewport follows.
              (agent-hq-refresh-buffer-name shell)
              (should (equal (buffer-name shell) "Claude @ webapp ⎇ develop · Fix login"))
              (should (equal (buffer-name viewport)
                             "Claude @ webapp ⎇ develop · Fix login [viewport]"))
              ;; Outside git too, after the directory.
              (setq default-directory (expand-file-name "plain/" base))
              (agent-hq-refresh-buffer-name shell)
              (should (equal (buffer-name shell) "Claude @ plain · Fix login")))
          (kill-buffer shell)
          (kill-buffer viewport))))))

(ert-deftest agent-hq-test-visit-shell-by-id ()
  (let ((shell (generate-new-buffer "Claude @ proj · New name"))
        visited)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-hq--shells) (lambda () (list shell)))
                  ((symbol-function 'agent-hq-visit-shell) (lambda (b) (setq visited b)))
                  ((symbol-function 'select-frame-set-input-focus) #'ignore))
          (with-current-buffer shell (setq agent-hq--shell-number 7))
          ;; Renamed since the notification: found by id.
          (agent-hq-visit-shell-named "Claude @ proj" (agent-hq--shell-id shell))
          (should (eq visited shell))
          (setq visited nil)
          (let ((inhibit-message t))
            (agent-hq-visit-shell-named "Claude @ proj" "1.1"))
          (should-not visited))
      (kill-buffer shell))))

;;;; Shells side by side

(defmacro agent-hq-test--with-agent-windows (&rest body)
  "Run BODY with `code-window' showing a file and `wa' shell `a' to its right.
Shells `b' and `c' exist but aren't shown."
  (declare (indent 0))
  `(let ((code (get-buffer-create " *agent-hq-test-code*"))
         (a (generate-new-buffer " shell a"))
         (b (generate-new-buffer " shell b"))
         (c (generate-new-buffer " shell c")))
     (dolist (shell (list a b c))
       (with-current-buffer shell (agent-hq-test-shell-mode)))
     (unwind-protect
         (progn
           (delete-other-windows)
           (switch-to-buffer code)
           (let* ((code-window (selected-window))
                  (wa (split-window code-window nil 'right)))
             (set-window-buffer wa a)
             ,@body))
       (delete-other-windows)
       (mapc #'kill-buffer (list code a b c)))))

(ert-deftest agent-hq-test-agent-window-is-the-one-in-use ()
  (agent-hq-test--with-agent-windows
    (let ((wb (split-window wa nil 'below)))
      (set-window-buffer wb b)
      ;; From a shell's window: that one, not the first.
      (select-window wb)
      (should (eq (agent-hq--agent-window) wb))
      (select-window wa)
      (should (eq (agent-hq--agent-window) wa))
      ;; From another window: the shell window used last.
      (select-window wb)
      (select-window code-window)
      (should (eq (agent-hq--agent-window) wb))
      (should (eq (agent-hq-display-in-agent-window c nil) wb))
      (should (eq (window-buffer wb) c))
      (should (eq (window-buffer wa) a))
      ;; No shell on screen: nothing to reuse.
      (set-window-buffer wa code)
      (set-window-buffer wb code)
      (should-not (agent-hq-display-in-agent-window c nil)))))

(ert-deftest agent-hq-test-display-beside ()
  (agent-hq-test--with-agent-windows
    (select-window code-window)
    ;; Wide enough: sideways, the shell shown stays.
    (let* ((agent-hq-beside-min-width 0)
           (new (agent-hq--display-beside b nil)))
      (should (eq (window-buffer new) b))
      (should (eq (window-buffer wa) a))
      (should (eq (window-in-direction 'right wa) new))
      ;; It closes again when the shell goes.
      (quit-window nil new)
      (should-not (window-live-p new)))
    ;; Too narrow for two: below.
    (let* ((agent-hq-beside-min-width 1000)
           (new (agent-hq--display-beside b nil)))
      (should (eq (window-in-direction 'below wa) new)))))

(ert-deftest agent-hq-test-beside-prefix ()
  (agent-hq-test--with-agent-windows
    (select-window code-window)
    (let ((agent-hq-beside-min-width 0)
          (display-buffer-overriding-action (list nil))
          (post-command-hook nil)
          (inhibit-message t))
      (let ((this-command 'agent-hq-beside))
        (agent-hq-beside)
        ;; Its own post-command doesn't end it.
        (run-hooks 'post-command-hook))
      (let ((this-command 'some-shell-command))
        ;; Other buffers, which-key's popup say, don't use it up.
        (display-buffer (get-buffer-create " *agent-hq-test-other*")
                        '(display-buffer-no-window (allow-no-window . t)))
        ;; The first shell shown goes beside, the next replaces as usual.
        (let ((window (display-buffer b '(agent-hq-display-in-agent-window))))
          (should-not (memq window (list wa code-window)))
          (should (eq (window-buffer wa) a)))
        (select-window wa)
        (should (eq (display-buffer c '(agent-hq-display-in-agent-window)) wa))
        (run-hooks 'post-command-hook))
      ;; Gone once that command is done.
      (should-not (car display-buffer-overriding-action))
      (should-not post-command-hook))
    (kill-buffer " *agent-hq-test-other*")))

;;; agent-hq-test.el ends here
