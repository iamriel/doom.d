;;; agent-git-header-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-git -l tests/agent-git-header-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: its few functions used here are stubbed.

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-git/agent-git.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-git/agent-git-header.el" dir) nil t))

(defvar agent-shell--state nil)
(defvar agent-shell-header-style 'graphical)

(defconst agent-git-header-test--models
  '(((:model-id . "default") (:name . "Default (recommended)")
     (:description . "Opus 5.5 · Best for everyday, complex tasks"))
    ((:model-id . "sonnet") (:name . "Sonnet")
     (:description . "Sonnet 5.5 · Efficient for routine tasks"))
    ((:model-id . "custom") (:name . "Custom") (:description . nil))))

(defmacro agent-git-header-test--with-models (choice &rest body)
  "Run BODY with the picker on CHOICE among `agent-git-header-test--models'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-shell--current-model-id) (lambda (_) ,choice))
             ((symbol-function 'agent-shell--get-available-models)
              (lambda (_) agent-git-header-test--models)))
     ,@body))

(ert-deftest agent-git-header-test-model-name ()
  (should (equal (agent-git-header-model-name "claude-opus-5-5") "Opus 5.5"))
  (should (equal (agent-git-header-model-name "claude-fable-5-2[1m]") "Fable 5.2"))
  (should (equal (agent-git-header-model-name "claude-haiku-4-5-20251001") "Haiku 4.5"))
  (should (equal (agent-git-header-model-name "claude-sonnet-4-20250514") "Sonnet 4"))
  (should (equal (agent-git-header-model-name "gpt-5.5") "gpt-5.5")))

(ert-deftest agent-git-header-test-model-shown ()
  (with-temp-buffer
    ;; Before any answer: the model the picker's description names.
    (agent-git-header-test--with-models "default"
      (should (equal (agent-git-header--model nil "Default (recommended)") "Opus 5.5"))
      ;; After one: the model Claude reported.
      (setq agent-git-header--answered-with '("default" . "claude-fable-5-2[1m]"))
      (should (equal (agent-git-header--model nil "Default (recommended)") "Fable 5.2")))
    ;; A different pick ignores the answer made under the old one.
    (agent-git-header-test--with-models "sonnet"
      (should (equal (agent-git-header--model nil "Sonnet") "Sonnet 5.5")))
    ;; Nothing to go on: agent-shell's label.
    (agent-git-header-test--with-models "custom"
      (should (equal (agent-git-header--model nil "Custom") "Custom")))))

(ert-deftest agent-git-header-test-default-names-an-alias ()
  ;; Claude's ACP config options describe "default" by alias only.
  (let ((models '(((:model-id . "default") (:name . "Default (recommended)")
                   (:description . "Opus"))
                  ((:model-id . "opus") (:name . "Opus 5.5")
                   (:description . "Opus 5.5 · Best for everyday, complex tasks"))
                  ((:model-id . "haiku") (:name . "Haiku 5.5") (:description . nil))
                  ((:model-id . "fast") (:name . "Fast") (:description . "Haiku"))
                  ((:model-id . "loop") (:name . "Loop") (:description . "Loop")))))
    (cl-letf (((symbol-function 'agent-shell--get-available-models) (lambda (_) models)))
      (should (equal (agent-git-header--described-model nil "default") "Opus 5.5"))
      ;; The alias's own name when it has no description.
      (should (equal (agent-git-header--described-model nil "fast") "Haiku 5.5"))
      ;; An entry naming itself, or nothing, gives nothing.
      (should-not (agent-git-header--described-model nil "loop"))
      (should-not (agent-git-header--described-model nil "haiku")))))

(ert-deftest agent-git-header-test-note-model ()
  (with-temp-buffer
    (let ((redraws 0)
          (event '((:event . usage-update)
                   (:data . ((:usage . nil)
                             (:meta . ((_claude/model . "claude-opus-5-5"))))))))
      (cl-letf (((symbol-function 'agent-shell--current-model-id) (lambda (_) "default"))
                ((symbol-function 'agent-shell--update-header-and-mode-line)
                 (lambda (&rest _) (cl-incf redraws))))
        (agent-git-header--note-model (current-buffer) event)
        (should (equal agent-git-header--answered-with '("default" . "claude-opus-5-5")))
        (should (= redraws 1))
        ;; The same model again doesn't redraw.
        (agent-git-header--note-model (current-buffer) event)
        (should (= redraws 1))
        ;; Updates without a model change nothing.
        (agent-git-header--note-model (current-buffer) '((:data . ((:meta . nil)))))
        (should (equal agent-git-header--answered-with '("default" . "claude-opus-5-5")))))))

(ert-deftest agent-git-header-test-format-usage ()
  (let ((team '((five_hour . ((utilization . 2) (resets_at . "2026-11-24T04:10:00.517004+00:00")))
                (seven_day . ((utilization . 3) (resets_at . "2026-11-30T11:00:00.517021+00:00")))))
        (max '((five_hour . nil)
               (seven_day . ((utilization . 14.4) (resets_at . "2026-11-29T21:00:00+00:00"))))))
    (should (equal (agent-git-header-format-usage team "Asia/Manila")
                   "5h 2% (12:10pm) · week 3% (Mon 7pm)"))
    ;; No 5-hour window: only the week.
    (should (equal (agent-git-header-format-usage max "Asia/Manila") "week 14% (Mon 5am)"))
    ;; Resets just before the minute round to it; other minutes show.
    (should (equal (agent-git-header-format-usage
                    '((five_hour . ((utilization . 1) (resets_at . "2026-11-24T04:09:59.8+00:00")))
                      (seven_day . ((utilization . 1) (resets_at . "2026-11-30T10:59:59.826+00:00"))))
                    "Asia/Manila")
                   "5h 1% (12:10pm) · week 1% (Mon 7pm)"))
    (should (equal (agent-git-header-format-usage
                    '((five_hour . ((utilization . 1) (resets_at . "2026-11-24T05:00:00+00:00")))
                      (seven_day . ((utilization . 1) (resets_at . "2026-11-30T11:30:00+00:00"))))
                    "Asia/Manila")
                   "5h 1% (1pm) · week 1% (Mon 7:30pm)"))
    ;; Missing numbers or reset times.
    (should (equal (agent-git-header-format-usage '((seven_day . ((utilization . 50)))))
                   "week 50%"))
    (should-not (agent-git-header-format-usage '((five_hour . ((utilization . nil))))))
    (should-not (agent-git-header-format-usage nil))))

(ert-deftest agent-git-header-test-duration ()
  (should (equal (agent-git-header-duration 0) "0m"))
  (should (equal (agent-git-header-duration 3599) "59m"))
  (should (equal (agent-git-header-duration 3900) "1h 05m"))
  (should (equal (agent-git-header-duration (* 26 3600)) "26h 00m")))

(defconst agent-git-header-test--usage
  '((five_hour . nil)
    (seven_day . ((utilization . 14) (resets_at . "2026-11-29T21:00:00+00:00")))))

(defun agent-git-header-test--header (fn-result started-with)
  "Run the header advice on FN-RESULT in a shell started with STARTED-WITH."
  (with-temp-buffer
    (setq agent-git--claude-started-with started-with
          agent-git-header--opened (- (float-time) 3900))
    (let ((agent-git-header--usage (make-hash-table :test #'equal)))
      (puthash "" (list :data agent-git-header-test--usage :time (float-time))
               agent-git-header--usage)
      (cl-letf (((symbol-function 'agent-git-header--fetch-usage)
                 (lambda (&rest _) (error "Fresh usage must not be fetched"))))
        (agent-git-header-test--with-models "default"
          (agent-git-header-model-a (lambda (&rest _) fn-result)
                                    `((:buffer . ,(current-buffer)))))))))

(ert-deftest agent-git-header-test-advice ()
  (let ((week (agent-git-header-format-usage agent-git-header-test--usage))
        (base '((:model-name . "Default (recommended)") (:session-id . nil)
                (:project-name . "webapp"))))
    ;; Claude: real model, usage and session time after the project.
    (let ((model (agent-git-header-test--header base 'default)))
      (should (equal (map-elt model :model-name) "Opus 5.5"))
      (should (equal (map-elt model :session-id) (concat week " ➤ 1h 05m")))
      (should (equal (map-elt model :project-name) "webapp")))
    ;; A session id agent-shell shows stays first.
    (let ((model (agent-git-header-test--header
                  (cons '(:session-id . "abc") base) 'default)))
      (should (equal (map-elt model :session-id) (concat "abc ➤ " week " ➤ 1h 05m"))))
    ;; The text header is a mode-line format: % is doubled there.
    (let* ((agent-shell-header-style 'text)
           (model (agent-git-header-test--header base 'default)))
      (should (equal (map-elt model :session-id)
                     (concat (string-replace "%" "%%" week) " ➤ 1h 05m"))))
    ;; Codex (no Claude account recorded): model untouched, only the time.
    (let ((model (agent-git-header-test--header base nil)))
      (should (equal (map-elt model :model-name) "Default (recommended)"))
      (should (equal (map-elt model :session-id) "1h 05m")))
    ;; Headers built outside a live shell are left alone.
    (should (equal (agent-git-header-model-a (lambda (&rest _) base) '((:buffer . nil)))
                   base))))

(defun agent-git-header-test--wait (account)
  "Wait for ACCOUNT's fetch to finish and its sentinel to run."
  (let ((process (plist-get (gethash account agent-git-header--usage) :process)))
    (should (processp process))
    (while (process-live-p process)
      (accept-process-output process 0.05))
    (accept-process-output nil 0.05)))

(ert-deftest agent-git-header-test-fetch ()
  (let ((agent-git-header--usage (make-hash-table :test #'equal))
        (script "echo 'Warning: noise on stderr' >&2
echo '{\"dir\": \"'\"${CLAUDE_CONFIG_DIR-unset}\"'\", \"seven_day\": {\"utilization\": 9}}'")
        (redrawn nil))
    (cl-letf (((symbol-function 'agent-git-header--usage-command)
               (lambda () (list "sh" "-c" script)))
              ((symbol-function 'agent-git-header--redraw)
               (lambda (_) (setq redrawn t))))
      ;; First call starts a fetch and has nothing to show yet.
      (should-not (agent-git-header--usage-text "/h/.claude-work/"))
      (agent-git-header-test--wait "/h/.claude-work/")
      (should redrawn)
      ;; The account's dir reached the process; stderr noise was skipped.
      (should (equal (alist-get 'dir (plist-get (gethash "/h/.claude-work/" agent-git-header--usage)
                                                :data))
                     "/h/.claude-work"))
      (should (equal (agent-git-header--usage-text "/h/.claude-work/") "week 9%"))
      ;; Fresh: no second fetch.
      (should-not (plist-get (gethash "/h/.claude-work/" agent-git-header--usage) :process))
      ;; The default account runs with CLAUDE_CONFIG_DIR unset.
      (let ((process-environment (cons "CLAUDE_CONFIG_DIR=/leaked" process-environment)))
        (agent-git-header--usage-text ""))
      (agent-git-header-test--wait "")
      (should (equal (alist-get 'dir (plist-get (gethash "" agent-git-header--usage) :data))
                     "unset"))
      ;; A failed fetch keeps the last usage and waits for the TTL.
      (setq script "exit 1")
      (let ((agent-git-header-usage-ttl -1))
        (agent-git-header--usage-text "/h/.claude-work/")
        (agent-git-header-test--wait "/h/.claude-work/"))
      (should (equal (agent-git-header--usage-text "/h/.claude-work/") "week 9%")))))

;;; agent-git-header-test.el ends here
