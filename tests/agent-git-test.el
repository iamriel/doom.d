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

;;; agent-git-test.el ends here
