;;; agent-hq-statusline-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-statusline-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: its few functions used here are stubbed.

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-statusline.el" dir) nil t))

(defvar agent-shell--state nil)
(defvar agent-shell-header-style 'graphical)

(defconst agent-hq-statusline-test--models
  '(((:model-id . "default") (:name . "Default (recommended)")
     (:description . "Opus 5.5 · Best for everyday, complex tasks"))
    ((:model-id . "sonnet") (:name . "Sonnet")
     (:description . "Sonnet 5.5 · Efficient for routine tasks"))
    ((:model-id . "custom") (:name . "Custom") (:description . nil))))

(defmacro agent-hq-statusline-test--with-models (choice &rest body)
  "Run BODY with the picker on CHOICE among `agent-hq-statusline-test--models'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-shell--current-model-id) (lambda (_) ,choice))
             ((symbol-function 'agent-shell--get-available-models)
              (lambda (_) agent-hq-statusline-test--models)))
     ,@body))

(ert-deftest agent-hq-statusline-test-model-name ()
  (should (equal (agent-hq-statusline-model-name "claude-opus-5-5") "Opus 5.5"))
  (should (equal (agent-hq-statusline-model-name "claude-fable-5-2[1m]") "Fable 5.2"))
  (should (equal (agent-hq-statusline-model-name "claude-haiku-4-5-20251001") "Haiku 4.5"))
  (should (equal (agent-hq-statusline-model-name "claude-sonnet-4-20250514") "Sonnet 4"))
  (should (equal (agent-hq-statusline-model-name "gpt-5.5") "gpt-5.5")))

(ert-deftest agent-hq-statusline-test-model-shown ()
  (with-temp-buffer
    ;; Before any answer: the model the picker's description names.
    (agent-hq-statusline-test--with-models "default"
      (should (equal (agent-hq-statusline--model nil "Default (recommended)") "Opus 5.5"))
      ;; After one: the model Claude reported.
      (setq agent-hq-statusline--answered-with '("default" . "claude-fable-5-2[1m]"))
      (should (equal (agent-hq-statusline--model nil "Default (recommended)") "Fable 5.2")))
    ;; A different pick ignores the answer made under the old one.
    (agent-hq-statusline-test--with-models "sonnet"
      (should (equal (agent-hq-statusline--model nil "Sonnet") "Sonnet 5.5")))
    ;; Nothing to go on: agent-shell's label.
    (agent-hq-statusline-test--with-models "custom"
      (should (equal (agent-hq-statusline--model nil "Custom") "Custom")))))

(ert-deftest agent-hq-statusline-test-default-names-an-alias ()
  ;; Claude's ACP config options describe "default" by alias only.
  (let ((models '(((:model-id . "default") (:name . "Default (recommended)")
                   (:description . "Opus"))
                  ((:model-id . "opus") (:name . "Opus 5.5")
                   (:description . "Opus 5.5 · Best for everyday, complex tasks"))
                  ((:model-id . "haiku") (:name . "Haiku 5.5") (:description . nil))
                  ((:model-id . "fast") (:name . "Fast") (:description . "Haiku"))
                  ((:model-id . "loop") (:name . "Loop") (:description . "Loop")))))
    (cl-letf (((symbol-function 'agent-shell--get-available-models) (lambda (_) models)))
      (should (equal (agent-hq-statusline--described-model nil "default") "Opus 5.5"))
      ;; The alias's own name when it has no description.
      (should (equal (agent-hq-statusline--described-model nil "fast") "Haiku 5.5"))
      ;; An entry naming itself, or nothing, gives nothing.
      (should-not (agent-hq-statusline--described-model nil "loop"))
      (should-not (agent-hq-statusline--described-model nil "haiku")))))

(ert-deftest agent-hq-statusline-test-note-model ()
  (with-temp-buffer
    (let ((redraws 0)
          (event '((:event . usage-update)
                   (:data . ((:usage . nil)
                             (:meta . ((_claude/model . "claude-opus-5-5"))))))))
      (cl-letf (((symbol-function 'agent-shell--current-model-id) (lambda (_) "default"))
                ((symbol-function 'agent-shell--update-header-and-mode-line)
                 (lambda (&rest _) (cl-incf redraws))))
        (agent-hq-statusline--note-model (current-buffer) event)
        (should (equal agent-hq-statusline--answered-with '("default" . "claude-opus-5-5")))
        (should (= redraws 1))
        ;; The same model again doesn't redraw.
        (agent-hq-statusline--note-model (current-buffer) event)
        (should (= redraws 1))
        ;; Updates without a model change nothing.
        (agent-hq-statusline--note-model (current-buffer) '((:data . ((:meta . nil)))))
        (should (equal agent-hq-statusline--answered-with '("default" . "claude-opus-5-5")))))))

(ert-deftest agent-hq-statusline-test-format-usage ()
  (let ((team '((five_hour . ((utilization . 2) (resets_at . "2026-11-24T04:10:00.517004+00:00")))
                (seven_day . ((utilization . 3) (resets_at . "2026-11-30T11:00:00.517021+00:00")))))
        (max '((five_hour . nil)
               (seven_day . ((utilization . 14.4) (resets_at . "2026-11-29T21:00:00+00:00"))))))
    (should (equal (agent-hq-statusline-format-usage team "Asia/Manila")
                   "5h 2% (12:10pm) · week 3% (Mon 7pm)"))
    ;; No 5-hour window: only the week.
    (should (equal (agent-hq-statusline-format-usage max "Asia/Manila") "week 14% (Mon 5am)"))
    ;; Resets just before the minute round to it; other minutes show.
    (should (equal (agent-hq-statusline-format-usage
                    '((five_hour . ((utilization . 1) (resets_at . "2026-11-24T04:09:59.8+00:00")))
                      (seven_day . ((utilization . 1) (resets_at . "2026-11-30T10:59:59.826+00:00"))))
                    "Asia/Manila")
                   "5h 1% (12:10pm) · week 1% (Mon 7pm)"))
    (should (equal (agent-hq-statusline-format-usage
                    '((five_hour . ((utilization . 1) (resets_at . "2026-11-24T05:00:00+00:00")))
                      (seven_day . ((utilization . 1) (resets_at . "2026-11-30T11:30:00+00:00"))))
                    "Asia/Manila")
                   "5h 1% (1pm) · week 1% (Mon 7:30pm)"))
    ;; Missing numbers or reset times.
    (should (equal (agent-hq-statusline-format-usage '((seven_day . ((utilization . 50)))))
                   "week 50%"))
    (should-not (agent-hq-statusline-format-usage '((five_hour . ((utilization . nil))))))
    (should-not (agent-hq-statusline-format-usage nil))))

(ert-deftest agent-hq-statusline-test-duration ()
  (should (equal (agent-hq-statusline-duration 0) "0m"))
  (should (equal (agent-hq-statusline-duration 3599) "59m"))
  (should (equal (agent-hq-statusline-duration 3900) "1h 05m"))
  (should (equal (agent-hq-statusline-duration (* 26 3600)) "26h 00m")))

(defconst agent-hq-statusline-test--usage
  '((five_hour . nil)
    (seven_day . ((utilization . 14) (resets_at . "2026-11-29T21:00:00+00:00")))))

(defun agent-hq-statusline-test--header (fn-result started-with &optional show-git)
  "Run the header advice on FN-RESULT in a shell started with STARTED-WITH.
Git status is left out unless SHOW-GIT."
  (with-temp-buffer
    (setq agent-hq--claude-started-with started-with
          agent-hq-statusline--opened (- (float-time) 3900))
    (let ((agent-hq-statusline--usage (make-hash-table :test #'equal))
          (agent-hq-statusline-show-git show-git))
      (dolist (account '("" "/h/.claude-work/"))
        (puthash account (list :data agent-hq-statusline-test--usage :time (float-time))
                 agent-hq-statusline--usage))
      (cl-letf (((symbol-function 'agent-hq-statusline--fetch-usage)
                 (lambda (&rest _) (error "Fresh usage must not be fetched"))))
        (agent-hq-statusline-test--with-models "default"
          (agent-hq-statusline-model-a (lambda (&rest _) fn-result)
                                    `((:buffer . ,(current-buffer)))))))))

(ert-deftest agent-hq-statusline-test-advice ()
  (let ((week (agent-hq-statusline-format-usage agent-hq-statusline-test--usage))
        (base '((:model-name . "Default (recommended)") (:session-id . nil)
                (:project-name . "webapp"))))
    ;; Claude: real model, usage and session time after the project.
    (let ((model (agent-hq-statusline-test--header base 'default)))
      (should (equal (map-elt model :model-name) "Opus 5.5"))
      (should (equal (map-elt model :session-id) (concat week " ➤ 1h 05m")))
      (should (equal (map-elt model :project-name) "webapp")))
    ;; A session id agent-shell shows stays first.
    (let ((model (agent-hq-statusline-test--header
                  (cons '(:session-id . "abc") base) 'default)))
      (should (equal (map-elt model :session-id) (concat "abc ➤ " week " ➤ 1h 05m"))))
    ;; The text header is a mode-line format: % is doubled there.
    (let* ((agent-shell-header-style 'text)
           (model (agent-hq-statusline-test--header base 'default)))
      (should (equal (map-elt model :session-id)
                     (concat (string-replace "%" "%%" week) " ➤ 1h 05m"))))
    ;; Another account is named before its usage.
    (let ((model (agent-hq-statusline-test--header base "/h/.claude-work/")))
      (should (equal (map-elt model :session-id) (concat "work " week " ➤ 1h 05m")))
      (should (eq (get-text-property 0 'face (map-elt model :session-id))
                  'agent-hq-statusline-account)))
    ;; Codex (no Claude account recorded): model untouched, only the time.
    (let ((model (agent-hq-statusline-test--header base nil)))
      (should (equal (map-elt model :model-name) "Default (recommended)"))
      (should (equal (map-elt model :session-id) "1h 05m")))
    ;; Headers built outside a live shell are left alone.
    (should (equal (agent-hq-statusline-model-a (lambda (&rest _) base) '((:buffer . nil)))
                   base))))

(defun agent-hq-statusline-test--wait (account)
  "Wait for ACCOUNT's fetch to finish and its sentinel to run."
  (let ((process (plist-get (gethash account agent-hq-statusline--usage) :process)))
    (should (processp process))
    (while (process-live-p process)
      (accept-process-output process 0.05))
    (accept-process-output nil 0.05)))

(ert-deftest agent-hq-statusline-test-fetch ()
  (let ((agent-hq-statusline--usage (make-hash-table :test #'equal))
        (script "echo 'Warning: noise on stderr' >&2
echo '{\"dir\": \"'\"${CLAUDE_CONFIG_DIR-unset}\"'\", \"seven_day\": {\"utilization\": 9}}'")
        (redrawn nil))
    (cl-letf (((symbol-function 'agent-hq-statusline--usage-command)
               (lambda () (list "sh" "-c" script)))
              ((symbol-function 'agent-hq-statusline--redraw)
               (lambda (_) (setq redrawn t))))
      ;; First call starts a fetch and has nothing to show yet.
      (should-not (agent-hq-statusline--usage-text "/h/.claude-work/"))
      (agent-hq-statusline-test--wait "/h/.claude-work/")
      (should redrawn)
      ;; The account's dir reached the process; stderr noise was skipped.
      (should (equal (alist-get 'dir (plist-get (gethash "/h/.claude-work/" agent-hq-statusline--usage)
                                                :data))
                     "/h/.claude-work"))
      (should (equal (agent-hq-statusline--usage-text "/h/.claude-work/") "week 9%"))
      ;; Fresh: no second fetch.
      (should-not (plist-get (gethash "/h/.claude-work/" agent-hq-statusline--usage) :process))
      ;; The default account runs with CLAUDE_CONFIG_DIR unset.
      (let ((process-environment (cons "CLAUDE_CONFIG_DIR=/leaked" process-environment)))
        (agent-hq-statusline--usage-text ""))
      (agent-hq-statusline-test--wait "")
      (should (equal (alist-get 'dir (plist-get (gethash "" agent-hq-statusline--usage) :data))
                     "unset"))
      ;; A failed fetch keeps the last usage and waits for the TTL.
      (setq script "exit 1")
      (let ((agent-hq-statusline-usage-ttl -1))
        (agent-hq-statusline--usage-text "/h/.claude-work/")
        (agent-hq-statusline-test--wait "/h/.claude-work/"))
      (should (equal (agent-hq-statusline--usage-text "/h/.claude-work/") "week 9%")))))

(define-derived-mode agent-hq-statusline-test-shell-mode fundamental-mode "Shell"
  "Stands in for `agent-shell-mode'.")
(put 'agent-hq-statusline-test-shell-mode 'derived-mode-parent 'agent-shell-mode)

(defun agent-hq-statusline-test--show-usage (usage started-with entry)
  "Return what `agent-hq-show-usage' prints for a shell's USAGE.
The shell was started with STARTED-WITH; ENTRY is its account's cache."
  (with-temp-buffer
    (agent-hq-statusline-test-shell-mode)
    (setq agent-shell--state (list (cons :usage usage))
          agent-hq--claude-started-with started-with)
    (let ((agent-hq-statusline--usage (make-hash-table :test #'equal))
          shown)
      (when entry (puthash "" entry agent-hq-statusline--usage))
      (cl-letf (((symbol-function 'agent-shell--usage-has-data-p)
                 (lambda (usage) (map-elt usage :context-size)))
                ((symbol-function 'agent-shell--format-usage)
                 (lambda (&rest _) " Context: 58k/1m (5.8%)"))
                ((symbol-function 'agent-hq-statusline--fetch-usage) #'ignore)
                ((symbol-function 'message)
                 (lambda (format &rest args)
                   (setq shown (substring-no-properties (apply #'format format args))))))
        (agent-hq-show-usage))
      shown)))

(ert-deftest agent-hq-statusline-test-show-usage ()
  (let ((fresh (list :data (cons '(subscription_type . "max") agent-hq-statusline-test--usage)
                     :time (float-time)))
        (week (agent-hq-statusline-format-usage agent-hq-statusline-test--usage)))
    ;; Claude has answered: the session's numbers, then the plan.
    (should (equal (agent-hq-statusline-test--show-usage '((:context-size . 1000000)) 'default fresh)
                   (concat "\n Context: 58k/1m (5.8%)\n    Plan: max · " week "\n")))
    ;; New or resumed shell: no error, and the plan still shows.
    (should (equal (agent-hq-statusline-test--show-usage nil 'default fresh)
                   (concat "\n Session: nothing reported until the agent answers"
                           "\n    Plan: max · " week "\n")))
    ;; The first fetch hasn't come back yet.
    (should (string-suffix-p
             "Plan: fetching, try again in a moment\n"
             (agent-hq-statusline-test--show-usage
              nil 'default (list :time (float-time)
                                 :process (make-process :name "agent-hq-statusline-test"
                                                        :command '("sleep" "5")
                                                        :noquery t)))))
    ;; Codex: no plan row.
    (should (equal (agent-hq-statusline-test--show-usage nil nil fresh)
                   "\n Session: nothing reported until the agent answers\n"))
    ;; Anywhere else it's a user error.
    (with-temp-buffer
      (should-error (agent-hq-show-usage) :type 'user-error))))

;;;; Status-line parts: account, usage colors, git

(ert-deftest agent-hq-statusline-test-account-label ()
  (should-not (agent-hq-statusline-account-label ""))
  (should-not (agent-hq-statusline-account-label nil))
  (should (equal (agent-hq-statusline-account-label "/Users/me/.claude-work/") "work"))
  (should (equal (agent-hq-statusline-account-label "/Users/me/.claude-work") "work"))
  (should (equal (agent-hq-statusline-account-label "/opt/agents/acme") "acme")))

(ert-deftest agent-hq-statusline-test-usage-colors ()
  (should (eq (agent-hq-statusline-usage-face 59) 'agent-shell-success))
  (should (eq (agent-hq-statusline-usage-face 60) 'agent-shell-warning))
  (should (eq (agent-hq-statusline-usage-face 85) 'agent-shell-error))
  ;; The percentage carries the face, the label and reset time don't.
  (let ((text (agent-hq-statusline-format-usage '((seven_day . ((utilization . 72)))))))
    (should (equal text "week 72%"))
    (should-not (get-text-property 0 'face text))
    (should (eq (get-text-property 5 'face text) 'agent-shell-warning))))

(ert-deftest agent-hq-statusline-test-parse-status ()
  (let ((output "# branch.oid 1234567890abcdef
# branch.head feat/x
# branch.upstream origin/feat/x
# branch.ab +2 -1
1 M. N... 100644 100644 100644 aaa bbb staged.el
1 .M N... 100644 100644 100644 aaa bbb changed.el
1 MM N... 100644 100644 100644 aaa bbb both.el
2 R. N... 100644 100644 100644 aaa bbb R100 new.el\told.el
u UU N... 100644 100644 100644 100644 aaa bbb ccc conflict.el
? new-file
? other
"))
    (should (equal (agent-hq-statusline-parse-status output)
                   '(:ahead 2 :behind 1 :staged 3 :modified 2 :untracked 2 :conflicts 1))))
  ;; No upstream: no branch.ab line.
  (should (equal (agent-hq-statusline-parse-status "# branch.oid (initial)\n# branch.head main\n")
                 '(:ahead 0 :behind 0 :staged 0 :modified 0 :untracked 0 :conflicts 0))))

(ert-deftest agent-hq-statusline-test-format-git ()
  (let ((counts '(:ahead 1 :behind 0 :staged 2 :modified 1 :untracked 3 :conflicts 0)))
    (should (equal (agent-hq-statusline-format-git "feat/x" counts) "feat/x ↑1 +2 ~1 ?3"))
    (let ((text (agent-hq-statusline-format-git "feat/x" counts)))
      (should (eq (get-text-property 0 'face text) 'agent-hq-statusline-branch))
      (should (eq (get-text-property (string-search "+" text) 'face text) 'agent-shell-success))
      (should (eq (get-text-property (string-search "~" text) 'face text) 'agent-shell-warning))))
  ;; Clean, behind, in a merge, with a conflict.
  (should (equal (agent-hq-statusline-format-git
                  "main" '(:ahead 0 :behind 4 :staged 0 :modified 0 :untracked 0 :conflicts 0))
                 "main ↓4 ✓"))
  (should (equal (agent-hq-statusline-format-git
                  "main" '(:ahead 0 :behind 0 :staged 0 :modified 0 :untracked 0 :conflicts 1)
                  "merging")
                 "main merging !1"))
  ;; Before git answers, or when it failed: just the branch, no ✓.
  (should (equal (agent-hq-statusline-format-git "main" nil) "main")))

(ert-deftest agent-hq-statusline-test-escape-percent ()
  (let* ((text (concat "5h " (propertize "42%" 'face 'agent-shell-success) " (1pm)"))
         (escaped (agent-hq-statusline--escape-percent text)))
    (should (equal escaped "5h 42%% (1pm)"))
    ;; Both %s keep the face, so the mode line shows one colored "42%".
    (should (eq (get-text-property 3 'face escaped) 'agent-shell-success))
    (should (eq (get-text-property 6 'face escaped) 'agent-shell-success))
    (should-not (get-text-property 8 'face escaped))))

(ert-deftest agent-hq-statusline-test-color-runs ()
  (let* ((text (concat (propertize "a&b" 'face 'agent-hq-statusline-branch) " ➤ "
                       (propertize "9%" 'face 'agent-shell-error)))
         (tspan (dom-node 'tspan '((fill . "#777777")) (substring-no-properties text)))
         (svg (dom-node 'svg nil (dom-node 'text nil (dom-node 'tspan nil "webapp") tspan))))
    (cl-letf (((symbol-function 'agent-shell--svg-fill-color)
               (lambda (face) (format "#%s" face))))
      (should (agent-hq-statusline-color-runs svg text)))
    ;; Each face run gets its own fill and the separator the default one;
    ;; the spaces around it keep the tspan's.
    (should (equal (dom-attr tspan 'fill) "#777777"))
    (should (equal (dom-children tspan)
                   '((tspan ((fill . "#agent-hq-statusline-branch")) "a&amp;b")
                     " " (tspan ((fill . "#default")) "➤") " "
                     (tspan ((fill . "#agent-shell-error")) "9%"))))
    ;; Text that isn't drawn anywhere leaves the SVG alone.
    (should-not (agent-hq-statusline-color-runs svg (propertize "nope" 'face 'error)))))

(defun agent-hq-statusline-test--git (dir &rest args)
  "Run git ARGS in DIR, failing the test on a non-zero exit."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil
                            "-c" "user.name=Test" "-c" "user.email=t@example.com"
                            "-c" "init.defaultBranch=main" args))
        (error "git %S failed: %s" args (buffer-string))))))

(defun agent-hq-statusline-test--wait-git (root)
  "Wait for git status in ROOT to finish and its sentinel to run."
  (let ((process (plist-get (gethash root agent-hq-statusline--git) :process)))
    (should (processp process))
    (while (process-live-p process)
      (accept-process-output process 0.05))
    (accept-process-output nil 0.05)))

(ert-deftest agent-hq-statusline-test-git-in-header ()
  (let* ((base (file-name-as-directory (file-truename (make-temp-file "agent-hq-statusline" t))))
         (main (expand-file-name "app/" base))
         (linked (expand-file-name "app_w1/" base))
         ;; The user's global ignores must not hide the test's files.
         (process-environment (append (list "GIT_CONFIG_GLOBAL=/dev/null"
                                            (concat "XDG_CONFIG_HOME=" base))
                                      process-environment))
         (agent-hq-statusline--git (make-hash-table :test #'equal))
         (redrawn 0))
    (unwind-protect
        (progn
          (make-directory main)
          (agent-hq-statusline-test--git main "init" "-q")
          (agent-hq-statusline-test--git main "commit" "-q" "--allow-empty" "-m" "init")
          (agent-hq-statusline-test--git main "worktree" "add" "-q" "-b" "feat/x"
                                      (directory-file-name linked))
          (with-temp-file (expand-file-name "new.el" linked) (insert "x"))
          (cl-letf (((symbol-function 'agent-hq-statusline--redraw)
                     (lambda (_) (setq redrawn (1+ redrawn)))))
            (with-temp-buffer
              (setq default-directory linked
                    agent-hq--claude-started-with nil
                    agent-hq-statusline--opened nil)
              (let ((base-model '((:model-name . "gpt") (:session-id . nil)
                                  (:project-name . "app_w1"))))
                ;; First header: the branch at once, counts once git answers.
                (let ((model (agent-hq-statusline-model-a (lambda (&rest _) base-model)
                                                       `((:buffer . ,(current-buffer))))))
                  (should (equal (map-elt model :project-name) "app:w1"))
                  (should (equal (map-elt model :session-id) "feat/x")))
                (agent-hq-statusline-test--wait-git linked)
                (should (= redrawn 1))
                (let ((model (agent-hq-statusline-model-a (lambda (&rest _) base-model)
                                                       `((:buffer . ,(current-buffer))))))
                  (should (equal (map-elt model :session-id) "feat/x ?1")))
                ;; Fresh: no second git; the same answer again doesn't redraw.
                (should-not (plist-get (gethash linked agent-hq-statusline--git) :process))
                (agent-hq-statusline--git-fetch linked)
                (agent-hq-statusline-test--wait-git linked)
                (should (= redrawn 1))
                ;; Staged after magit refreshes the repo: asked again, redrawn.
                (agent-hq-statusline-test--git linked "add" "new.el")
                (agent-hq-statusline-magit-refresh-h)
                (agent-hq-statusline-test--wait-git linked)
                (should (= redrawn 2))
                (should (equal (agent-hq-statusline--git-text (agent-hq-context)) "feat/x +1"))
                ;; The main worktree keeps agent-shell's project name.
                (setq default-directory main)
                (let ((model (agent-hq-statusline-model-a (lambda (&rest _) '((:project-name . "app")))
                                                       `((:buffer . ,(current-buffer))))))
                  (should (equal (map-elt model :project-name) "app")))
                (agent-hq-statusline-test--wait-git main)))))
      (delete-directory base t))))

;;; agent-hq-statusline-test.el ends here
