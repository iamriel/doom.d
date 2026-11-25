;;; agent-hq-queue-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-queue-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: the functions used and advised here are
;; stand-ins shaped like agent-shell's.

(require 'ert)
(require 'cl-lib)
(require 'map)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq-spinner.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-queue.el" dir) nil t))

(defvar agent-shell--state nil)
(defvar comint-last-prompt nil)
(defvar shell-maker-finish-output-hook nil)

(define-derived-mode agent-shell-mode fundamental-mode "Agent")

(defvar agent-hq-queue-test--busy t)
(defvar agent-hq-queue-test--steering t)
(defvar agent-hq-queue-test--status 'busy)
(defvar agent-hq-queue-test--steered nil
  "Prompts steered, last first.")
(defvar agent-hq-queue-test--steer-error nil
  "Non-nil to make steering signal.")
(defvar agent-hq-queue-test--shells nil)

(defun shell-maker-busy () agent-hq-queue-test--busy)
(defun agent-shell-buffers () agent-hq-queue-test--shells)
(cl-defun agent-shell--shell-buffer (&key no-create &allow-other-keys)
  (ignore no-create)
  (if (derived-mode-p 'agent-shell-mode)
      (current-buffer)
    (car agent-hq-queue-test--shells)))
(cl-defun agent-shell-steering-supported-p (&key shell-buffer)
  (ignore shell-buffer)
  agent-hq-queue-test--steering)
(defun agent-shell-status () agent-hq-queue-test--status)
(defun agent-shell-busy-submit-steer (prompt)
  (when agent-hq-queue-test--steer-error
    (error "Steer refused"))
  (push prompt agent-hq-queue-test--steered))

(cl-defun agent-shell--prompt-queue-enqueue (&key prompt)
  (map-put! agent-shell--state :pending-prompts
            (append (map-elt agent-shell--state :pending-prompts) (list prompt))))
(defun agent-shell--prompt-queue-process-next ()
  (when-let* ((pending (map-elt agent-shell--state :pending-prompts)))
    (map-put! agent-shell--state :pending-prompts nil)
    (push (string-join pending "\n\n") agent-hq-queue-test--steered)))
(defun agent-shell--prompt-queue-display ())
(defun agent-shell-prompt-queue-remove (&optional index)
  (with-current-buffer (car agent-hq-queue-test--shells)
    (let ((pending (map-elt agent-shell--state :pending-prompts)))
      (map-put! agent-shell--state :pending-prompts
                (and index (append (seq-take pending index)
                                   (seq-drop pending (1+ index))))))))
(defun agent-shell-busy-submit-reroute-queue-to-steer ())
(defun agent-shell--print-prompt ())
(defun agent-shell-chat--label-prompts (&optional _from))

(defun agent-shell--live-input-prompt-p (prompt)
  (and prompt (not (text-property-any (cdr prompt) (point-max) 'field 'output))))
(defun agent-shell--prompt-input-start ()
  (and (agent-shell--live-input-prompt-p comint-last-prompt)
       (marker-position (cdr comint-last-prompt))))
(defun agent-shell--take-prompt-input ()
  (when-let* ((start (agent-shell--prompt-input-start))
              (input (string-trim (buffer-substring start (point-max))))
              ((not (string-empty-p input))))
    (delete-region start (point-max))
    input))

(agent-hq-queue-setup)

(defmacro agent-hq-queue-test--in-shell (&rest body)
  "Run BODY in a busy stub shell ending in a live prompt, \"Claude> \".
The prompt's start is `prompt'."
  (declare (indent 0))
  `(with-temp-buffer
     (agent-shell-mode)
     (agent-hq-queue-setup-shell-h)
     (setq-local agent-shell--state (list (cons :pending-prompts nil)))
     (let ((agent-hq-queue-test--shells (list (current-buffer)))
           (agent-hq-queue-test--busy t)
           (agent-hq-queue-test--steering t)
           (agent-hq-queue-test--status 'busy)
           (agent-hq-queue-test--steered nil)
           (agent-hq-queue-test--steer-error nil)
           (agent-hq-queue-key-hints nil)
           (agent-hq-queue-held-key-hints nil))
       (insert (propertize "answer\n\n" 'field 'output))
       (let ((prompt (point-marker)))
         (insert (propertize "Claude> " 'field 'output))
         (setq-local comint-last-prompt (cons prompt (point-marker)))
         ,@body))))

(defun agent-hq-queue-test--shown ()
  "Return the list drawn above this shell's prompt, or nil."
  (and agent-hq-queue--overlay
       (substring-no-properties (overlay-get agent-hq-queue--overlay 'before-string))))

(defun agent-hq-queue-test--queue (&rest prompts)
  "Queue PROMPTS as submitting each mid-turn does."
  (dolist (prompt prompts)
    (agent-shell--prompt-queue-enqueue :prompt prompt)))

;;;; The list

(ert-deftest agent-hq-queue-test-text ()
  (let ((agent-hq-queue-max-rows 2)
        (agent-hq-queue-key-hints nil))
    (should (equal (substring-no-properties
                    (agent-hq-queue--text '("just the\nfilenames" "sorted by size" "and dates")
                                          t 40))
                   "◷ 3 queued, sent when this turn ends\n  › just the filenames\n  › sorted by size\n  … 1 more\n"))
    ;; A long prompt is cut to the window, on one line.
    (should (equal (substring-no-properties (agent-hq-queue--text (list (make-string 50 ?x)) nil 20))
                   (concat "◷ 1 queued, held: th\n  › " (make-string 14 ?x) "…\n")))))

(ert-deftest agent-hq-queue-test-key-hints-only-when-they-fit ()
  (let ((agent-hq-queue-key-hints "M-RET now")
        (agent-hq-queue-held-key-hints ", q s send"))
    (let ((wide (agent-hq-queue--heading 1 t 80)))
      (should (string-suffix-p "M-RET now" wide))
      ;; Right-aligned: the space before them stretches to the edge.
      (should (equal (get-text-property (- (length wide) 10) 'display wide)
                     '(space :align-to (- right 10)))))
    (should (equal (substring-no-properties (agent-hq-queue--heading 1 t 40))
                   "◷ 1 queued, sent when this turn ends"))
    ;; Once the turn stopped, M-RET would submit the prompt instead.
    (should (string-suffix-p ", q s send" (agent-hq-queue--heading 1 nil 80)))))

(ert-deftest agent-hq-queue-test-drawn-above-the-prompt-while-queued ()
  (agent-hq-queue-test--in-shell
    (should-not (agent-hq-queue-test--shown))
    (agent-hq-queue-test--queue "just the filenames")
    (should (= (overlay-start agent-hq-queue--overlay) (marker-position prompt)))
    (should (eq (overlay-get agent-hq-queue--overlay 'priority) 2))
    (should (equal (agent-hq-queue-test--shown)
                   "◷ 1 queued, sent when this turn ends\n  › just the filenames\n\n"))
    (agent-hq-queue-test--queue "sorted by size")
    (should (string-match-p "2 queued.*\n.*filenames\n.*sorted by size\n\n\\'"
                            (agent-hq-queue-test--shown)))
    ;; Output written above the prompt goes above the list too.
    (save-excursion
      (goto-char prompt)
      (insert "more\n"))
    (should (equal (buffer-substring (overlay-start agent-hq-queue--overlay) (point-max))
                   "Claude> "))
    ;; The turn ends and sends them: the list goes.
    (agent-shell--prompt-queue-process-next)
    (should-not (agent-hq-queue-test--shown))
    (should-not agent-hq-queue--overlay)))

(ert-deftest agent-hq-queue-test-held-once-the-turn-stops ()
  (agent-hq-queue-test--in-shell
    (agent-hq-queue-test--queue "just the filenames")
    (setq agent-hq-queue-test--busy nil)
    (run-hooks 'shell-maker-finish-output-hook)
    (should (string-prefix-p "◷ 1 queued, held: the turn stopped\n" (agent-hq-queue-test--shown)))))

(ert-deftest agent-hq-queue-test-follows-a-new-prompt ()
  (agent-hq-queue-test--in-shell
    (agent-hq-queue-test--queue "just the filenames")
    (insert "hi\n")
    (let ((next (point-marker)))
      (insert (propertize "Claude> " 'field 'output))
      (setq-local comint-last-prompt (cons next (point-marker)))
      (agent-shell--print-prompt)
      (should (= (overlay-start agent-hq-queue--overlay) (marker-position next))))))

(ert-deftest agent-hq-queue-test-removing-from-elsewhere-redraws ()
  (agent-hq-queue-test--in-shell
    (agent-hq-queue-test--queue "a" "b")
    (let ((shell (current-buffer)))
      (with-temp-buffer
        (agent-shell-prompt-queue-remove 0))
      (with-current-buffer shell
        (should (string-match-p "1 queued.*\n  › b\n" (agent-hq-queue-test--shown))))
      (with-temp-buffer
        (agent-shell-prompt-queue-remove))
      (with-current-buffer shell
        (should-not (agent-hq-queue-test--shown))))))

;;;; Sending now

(ert-deftest agent-hq-queue-test-send-now-steers-them-as-one ()
  (agent-hq-queue-test--in-shell
    (agent-hq-queue-test--queue "just the filenames" "sorted by size")
    (agent-hq-queue-send-now)
    (should (equal agent-hq-queue-test--steered '("just the filenames\n\nsorted by size")))
    (should-not (agent-hq-queue--pending))
    (should-not (agent-hq-queue-test--shown))))

(ert-deftest agent-hq-queue-test-a-failed-steer-keeps-the-queue ()
  (agent-hq-queue-test--in-shell
    (agent-hq-queue-test--queue "a" "b")
    (let ((agent-hq-queue-test--steer-error t))
      (should-error (agent-hq-queue-send-now)))
    (should (equal (agent-hq-queue--pending) '("a" "b")))
    (should (string-match-p "2 queued" (agent-hq-queue-test--shown)))))

(ert-deftest agent-hq-queue-test-send-now-without-steering-leaves-the-queue ()
  (agent-hq-queue-test--in-shell
    (agent-hq-queue-test--queue "a")
    (let ((agent-hq-queue-test--steering nil))
      (should-error (agent-hq-queue-send-now) :type 'user-error))
    (should (equal (agent-hq-queue--pending) '("a")))
    (should-not agent-hq-queue-test--steered)))

(ert-deftest agent-hq-queue-test-send-now-asks-while-the-agent-waits-on-you ()
  (agent-hq-queue-test--in-shell
    (agent-hq-queue-test--queue "a")
    (let ((agent-hq-queue-test--status 'blocked))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (should-error (agent-hq-queue-send-now) :type 'user-error))
      (should (equal (agent-hq-queue--pending) '("a")))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
        (agent-hq-queue-send-now))
      (should (equal agent-hq-queue-test--steered '("a"))))))

(ert-deftest agent-hq-queue-test-send-now-when-idle-submits ()
  (agent-hq-queue-test--in-shell
    (agent-hq-queue-test--queue "a" "b")
    (setq agent-hq-queue-test--busy nil)
    (agent-hq-queue-send-now)
    (should (equal agent-hq-queue-test--steered '("a\n\nb")))
    (should-not (agent-hq-queue-test--shown))))

(ert-deftest agent-hq-queue-test-send-now-with-nothing-queued ()
  (agent-hq-queue-test--in-shell
    (should-error (agent-hq-queue-send-now) :type 'user-error)))

;;;; Editing

(ert-deftest agent-hq-queue-test-edit-takes-them-back-ahead-of-the-draft ()
  (agent-hq-queue-test--in-shell
    (agent-hq-queue-test--queue "just the filenames" "sorted by size")
    (goto-char (point-max))
    (insert "and dates")
    (agent-hq-queue-edit)
    (should-not (agent-hq-queue--pending))
    (should-not (agent-hq-queue-test--shown))
    (should (equal (buffer-substring (cdr comint-last-prompt) (point-max))
                   "just the filenames\n\nsorted by size\n\nand dates"))))

(ert-deftest agent-hq-queue-test-edit-needs-a-live-prompt ()
  (agent-hq-queue-test--in-shell
    (agent-hq-queue-test--queue "a")
    (goto-char (point-max))
    (insert (propertize "streaming" 'field 'output))
    (should-error (agent-hq-queue-edit) :type 'user-error)
    (should (equal (agent-hq-queue--pending) '("a")))))

;;; agent-hq-queue-test.el ends here
