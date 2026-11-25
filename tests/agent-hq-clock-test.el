;;; agent-hq-clock-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-clock-test.el \
;;         -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq-clock.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-spinner.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-logo.el" dir) nil t))

(defvar agent-shell--state nil)

(defmacro agent-hq-clock-test--fresh (&rest body)
  "Run BODY with a clock of its own, stopped afterwards."
  (declare (indent 0))
  `(let ((agent-hq-clock--animations nil)
         (agent-hq-clock--timer nil)
         (agent-hq-clock-beat 0))
     (unwind-protect
         (progn ,@body)
       (when (timerp agent-hq-clock--timer)
         (cancel-timer agent-hq-clock--timer)))))

(ert-deftest agent-hq-clock-test-one-beat-for-all ()
  (agent-hq-clock-test--fresh
    (let (drawn)
      (agent-hq-clock-start 'a (lambda (beat) (push (cons 'a beat) drawn)))
      (agent-hq-clock-start 'b (lambda (beat) (push (cons 'b beat) drawn)))
      (should (timerp agent-hq-clock--timer))
      ;; One timer, on wall-clock tenths of a second.
      (should (= (timer--repeat-delay agent-hq-clock--timer) 0.1))
      ;; Each beat, every animation draws, with the same beat.
      (agent-hq-clock--tick)
      (should (equal (sort drawn (lambda (x y) (string< (car x) (car y))))
                     '((a . 1) (b . 1))))
      ;; Starting a key again replaces it.
      (setq drawn nil)
      (agent-hq-clock-start 'a (lambda (beat) (push (cons 'a2 beat) drawn)))
      (agent-hq-clock--tick)
      (should (equal (length drawn) 2))
      (should (assq 'a2 drawn))
      ;; The timer goes with the last animation.
      (agent-hq-clock-stop 'a)
      (should (timerp agent-hq-clock--timer))
      (should (agent-hq-clock-running-p 'b))
      (agent-hq-clock-stop 'b)
      (should-not agent-hq-clock--timer)
      (should-not (agent-hq-clock-running-p 'b)))))

(ert-deftest agent-hq-clock-test-keys-come-first ()
  (agent-hq-clock-test--fresh
    (let (drawn (pending t))
      (agent-hq-clock-start 'a (lambda (beat) (push beat drawn)))
      (cl-letf (((symbol-function 'input-pending-p) (lambda (&rest _) pending)))
        ;; A key waiting: nothing draws, but the beat goes on.
        (agent-hq-clock--tick)
        (should-not drawn)
        (setq pending nil)
        (agent-hq-clock--tick)
        (should (equal drawn '(2)))))))

(ert-deftest agent-hq-clock-test-a-failing-animation-stops-alone ()
  (agent-hq-clock-test--fresh
    (let (drawn (inhibit-message t))
      (agent-hq-clock-start 'bad (lambda (_) (error "Boom")))
      (agent-hq-clock-start 'good (lambda (beat) (push beat drawn)))
      ;; One that stops itself while the beat goes round is fine too.
      (agent-hq-clock-start 'once (lambda (_) (agent-hq-clock-stop 'once)))
      (agent-hq-clock--tick)
      (agent-hq-clock--tick)
      (should (equal drawn '(2 1)))
      (should-not (agent-hq-clock-running-p 'bad))
      (should-not (agent-hq-clock-running-p 'once))
      (should (agent-hq-clock-running-p 'good)))))

(ert-deftest agent-hq-clock-test-heartbeat-rate ()
  (should (equal (agent-hq-clock--heartbeat-rate-a '(:on-heartbeat ignore))
                 '(:on-heartbeat ignore :beats-per-second 2)))
  ;; A rate given stays; nil leaves agent-shell's own.
  (should (equal (agent-hq-clock--heartbeat-rate-a '(:beats-per-second 5))
                 '(:beats-per-second 5)))
  (let ((agent-hq-clock-heartbeat-rate nil))
    (should (equal (agent-hq-clock--heartbeat-rate-a '(:on-heartbeat ignore))
                   '(:on-heartbeat ignore)))))

(ert-deftest agent-hq-clock-test-turns-join-and-leave ()
  ;; The spinner and the logo draw on the clock from a turn's start to
  ;; its end, which agent-shell's heartbeat still says.
  (agent-hq-clock-test--fresh
    (with-temp-buffer
      (let ((shell (current-buffer)))
        (agent-hq-spinner--beat shell 0 'started)
        (agent-hq-logo--beat shell 0 'started)
        (should (agent-hq-clock-running-p (cons 'agent-hq-spinner shell)))
        (should (agent-hq-clock-running-p (cons 'agent-hq-logo shell)))
        ;; Busy beats of the heartbeat change nothing.
        (agent-hq-spinner--beat shell 3 'busy)
        (should (= (length agent-hq-clock--animations) 2))
        (agent-hq-spinner--beat shell nil 'ended)
        (agent-hq-logo--beat shell nil 'ended)
        (should-not agent-hq-clock--animations)
        (should-not agent-hq-clock--timer)))
    ;; A shell killed mid-turn is let go on the next beat.
    (let ((shell (generate-new-buffer " shell")))
      (agent-hq-spinner--beat shell 0 'started)
      (agent-hq-logo--beat shell 0 'started)
      (kill-buffer shell)
      (agent-hq-clock--tick)
      (should-not agent-hq-clock--animations))))

;;; agent-hq-clock-test.el ends here
