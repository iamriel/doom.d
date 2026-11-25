;;; agent-hq-spinner-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-spinner-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: the functions advised here are stand-ins
;; shaped like agent-shell's.

(require 'ert)
(require 'cl-lib)
(require 'map)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq-spinner.el" dir) nil t))

(defvar agent-shell--state nil)

(defun agent-shell--status-config (status)
  (pcase status
    ("pending" '((:label . "wait") (:icon . "◔")))
    ("in_progress" '((:label . "busy") (:icon . "◔")))
    ("completed" '((:label . "done") (:icon . "✓")))
    (_ '((:label . "unknown") (:icon . "?")))))

(defvar agent-hq-spinner-test--word-style nil
  "Non-nil to label as a word style does: \" busy \" in place of an icon.")

(cl-defun agent-shell--make-status-kind-label (&key status kind)
  (let ((config (agent-shell--status-config status)))
    (if agent-hq-spinner-test--word-style
        (format " %s " (map-elt config :label))
      (concat (propertize (map-elt config :icon) 'font-lock-face 'warning)
              (when kind (concat " " (capitalize kind)))))))

(defun agent-shell--format-plan (entries)
  (mapconcat (lambda (entry)
               (concat (agent-shell--make-status-kind-label :status (car entry))
                       " " (cdr entry)))
             entries "\n"))

(defconst agent-hq-spinner-test--icon "✶")
(defun agent-shell--thought-process-icon () agent-hq-spinner-test--icon)

(defun agent-shell--make-heartbeat-handler (_shell-buffer)
  (lambda (_beat _status)))

(defun agent-shell--text-change-unsafe-p () nil)

(defun agent-shell--active-requests-p (state)
  (map-elt state :active-requests))

(defvar comint-last-prompt nil)

(defun agent-shell-chat--label-prompts (&optional _from)
  "Stand-in: the test lays the label's overlays itself.")
(defun agent-shell--live-input-prompt-p (prompt)
  (and prompt (= (cdr prompt) (point-max))))

(agent-hq-spinner-setup)

(defun agent-hq-spinner-test--tags (text)
  "Return TEXT's characters tagged to spin.
Each tag must be the current turn's token."
  (let (tags)
    (dotimes (i (length text))
      (when-let* ((tag (get-text-property i 'agent-hq-spinner text)))
        (should (eq tag agent-hq-spinner--turn))
        (push (substring-no-properties text i (1+ i)) tags)))
    (nreverse tags)))

(defmacro agent-hq-spinner-test--in-turn (&rest body)
  "Run BODY in a buffer whose shell is busy."
  (declare (indent 0))
  `(with-temp-buffer
     (setq agent-hq-spinner--turn (list 'turn))
     ,@body))

(defmacro agent-hq-spinner-test--in-shell (&rest body)
  "Run BODY in a stub shell that every window shows whole.
`beat' sends the shell a heartbeat; a busy one also comes as that beat
of the shared clock, which draws.  `shown' lists what the buffer
displays, overlays drawn."
  (declare (indent 0))
  `(with-temp-buffer
     (setq agent-shell--state (list (cons :last-entry-type nil)
                                    (cons :chunked-group-count 0)
                                    (cons :active-requests nil)))
     (let* ((shell (current-buffer))
            (handler (agent-shell--make-heartbeat-handler shell))
            (visible t))
       (unwind-protect
           (cl-letf (((symbol-function 'get-buffer-window)
                      (lambda (&rest _) (and visible 'window)))
                     ((symbol-function 'agent-hq-spinner--visible-ranges)
                      (lambda () (and visible (list (cons (point-min) (point-max)))))))
             (cl-flet ((beat (value status)
                         (funcall handler value status)
                         (when-let* (((eq status 'busy))
                                     (animation (alist-get (cons 'agent-hq-spinner shell)
                                                           agent-hq-clock--animations
                                                           nil nil #'equal)))
                           (funcall animation value)))
                       (shown () (agent-hq-spinner-test--shown)))
               (ignore #'beat #'shown)
               ,@body))
         (agent-hq-clock-stop (cons 'agent-hq-spinner shell))))))

(defun agent-hq-spinner-test--shown ()
  "Return this buffer's text as displayed, spinner overlays in place."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (dolist (overlay (sort (copy-sequence agent-hq-spinner--overlays)
                           (lambda (a b) (> (overlay-start a) (overlay-start b)))))
      (when (overlay-buffer overlay)
        (setq text (concat (substring text 0 (1- (overlay-start overlay)))
                           (overlay-get overlay 'display)
                           (substring text (1- (overlay-end overlay)))))))
    text))

(defun agent-hq-spinner-test--insert-label (status kind title)
  "Insert a tool call's label for STATUS and KIND, then TITLE and a newline."
  (insert (agent-shell--make-status-kind-label :status status :kind kind) " " title "\n"))

;;; Tagging

(ert-deftest agent-hq-spinner-test-tags-only-the-icon-of-work-under-way ()
  (agent-hq-spinner-test--in-turn
    (should (equal (agent-hq-spinner-test--tags
                    (agent-shell--make-status-kind-label :status "pending" :kind "execute"))
                   '("◔")))
    (should (equal (agent-hq-spinner-test--tags
                    (agent-shell--make-status-kind-label :status "in_progress"))
                   '("◔")))
    (should-not (agent-hq-spinner-test--tags
                 (agent-shell--make-status-kind-label :status "completed" :kind "execute")))
    ;; The face agent-shell gave the icon stays.
    (should (eq (get-text-property 0 'font-lock-face
                                   (agent-shell--make-status-kind-label :status "pending"))
                'warning))))

(ert-deftest agent-hq-spinner-test-tags-nothing-while-idle ()
  (with-temp-buffer
    (should-not (get-text-property
                 0 'agent-hq-spinner
                 (agent-shell--make-status-kind-label :status "pending" :kind "read")))))

(ert-deftest agent-hq-spinner-test-plan-steps-spin-only-in-progress ()
  (agent-hq-spinner-test--in-turn
    (should (equal (agent-hq-spinner-test--tags
                    (agent-shell--format-plan '(("completed" . "Read")
                                                ("in_progress" . "Edit")
                                                ("pending" . "Test"))))
                   '("◔")))
    ;; Outside a plan, a pending tool call still spins.
    (should (agent-hq-spinner-test--tags
             (agent-shell--make-status-kind-label :status "pending")))))

(ert-deftest agent-hq-spinner-test-word-styles-are-left-alone ()
  (agent-hq-spinner-test--in-turn
    (let ((agent-hq-spinner-test--word-style t))
      (should-not (agent-hq-spinner-test--tags
                   (agent-shell--make-status-kind-label :status "in_progress" :kind "read"))))))

(ert-deftest agent-hq-spinner-test-leaves-thoughts-still ()
  ;; The working line says the agent is thinking; a thought's icon
  ;; spinning above it would say it twice.
  (agent-hq-spinner-test--in-turn
    (should-not (agent-hq-spinner-test--tags (agent-shell--thought-process-icon)))))

;;; Animating

(ert-deftest agent-hq-spinner-test-spins-while-busy-and-stops-after ()
  (let ((agent-hq-spinner-frames '("a" "b" "c")))
    (agent-hq-spinner-test--in-shell
      (insert "> do it\n")
      (beat 0 'started)
      (agent-hq-spinner-test--insert-label "pending" "execute" "ls")
      (beat 1 'busy)
      (should (equal (shown) "> do it\nb Execute ls\n"))
      (beat 2 'busy)
      (should (equal (shown) "> do it\nc Execute ls\n"))
      (beat 3 'busy)
      (should (equal (shown) "> do it\na Execute ls\n"))
      (beat nil 'ended)
      (should (equal (shown) "> do it\n◔ Execute ls\n"))
      (should-not agent-hq-spinner--overlays)
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest agent-hq-spinner-test-keeps-its-overlays-from-beat-to-beat ()
  (agent-hq-spinner-test--in-shell
    (beat 0 'started)
    (agent-hq-spinner-test--insert-label "pending" "read" "a.el")
    (agent-hq-spinner-test--insert-label "pending" "read" "b.el")
    (beat 1 'busy)
    (let ((overlays (copy-sequence agent-hq-spinner--overlays))
          (starts (mapcar #'overlay-start agent-hq-spinner--overlays)))
      (should (= (length overlays) 2))
      (beat 2 'busy)
      (should (equal agent-hq-spinner--overlays overlays))
      (should (equal (mapcar #'overlay-start agent-hq-spinner--overlays) starts)))))

(ert-deftest agent-hq-spinner-test-a-finished-call-stops-spinning ()
  (let ((agent-hq-spinner-frames '("a" "b")))
    (agent-hq-spinner-test--in-shell
      (beat 0 'started)
      (agent-hq-spinner-test--insert-label "pending" "execute" "ls")
      (agent-hq-spinner-test--insert-label "pending" "read" "a.el")
      (beat 1 'busy)
      (should (equal (shown) "b Execute ls\nb Read a.el\n"))
      ;; agent-shell rewrites a label whose status changed.
      (goto-char (point-min))
      (delete-region (point) (1+ (point)))
      (insert (agent-shell--make-status-kind-label :status "completed"))
      (beat 2 'busy)
      (should (equal (shown) "✓ Execute ls\na Read a.el\n"))
      (should (= (length agent-hq-spinner--overlays) 1)))))

(ert-deftest agent-hq-spinner-test-icons-of-earlier-turns-stay-still ()
  (let ((agent-hq-spinner-frames '("a" "b")))
    (agent-hq-spinner-test--in-shell
      (beat 0 'started)
      ;; Left pending by an interrupted turn.
      (agent-hq-spinner-test--insert-label "pending" "execute" "old")
      (beat nil 'ended)
      (beat 0 'started)
      (agent-hq-spinner-test--insert-label "pending" "execute" "new")
      (beat 1 'busy)
      (should (equal (shown) "◔ Execute old\nb Execute new\n")))))

(ert-deftest agent-hq-spinner-test-spins-output-written-above-the-prompt ()
  ;; agent-shell keeps a live prompt at the end and writes above it.
  (let ((agent-hq-spinner-frames '("a" "b")))
    (agent-hq-spinner-test--in-shell
      (insert "Claude> ")
      (beat 0 'started)
      (goto-char (point-min))
      (agent-hq-spinner-test--insert-label "pending" "execute" "ls")
      (beat 1 'busy)
      (should (equal (shown) "b Execute ls\nClaude> ")))))

(ert-deftest agent-hq-spinner-test-only-the-working-line-moves-while-thinking ()
  (let ((agent-hq-spinner-working-frames '("x" "y")))
    (agent-hq-spinner-test--in-shell
      (map-put! agent-shell--state :active-requests '(prompt))
      (beat 0 'started)
      (insert (agent-shell--thought-process-icon) " Thinking\n")
      (map-put! agent-shell--state :last-entry-type "agent_thought_chunk")
      (beat 1 'busy)
      (should (equal (shown) "✶ Thinking\n"))
      (should-not agent-hq-spinner--overlays)
      (should (equal (cdr (agent-hq-spinner-test--working)) "y Working…\n\n")))))

(defun agent-hq-spinner-test--working ()
  "Return (POSITION . TEXT) of the working line shown, or nil."
  (when agent-hq-spinner--working
    (cons (overlay-start agent-hq-spinner--working)
          (substring-no-properties (overlay-get agent-hq-spinner--working 'before-string)))))

(defun agent-hq-spinner-test--write-above (prompt-start text)
  "Insert TEXT above the prompt at PROMPT-START, as agent-shell does."
  (save-excursion
    (set-marker-insertion-type prompt-start t)
    (goto-char prompt-start)
    (insert text)
    (set-marker-insertion-type prompt-start nil)))

(ert-deftest agent-hq-spinner-test-working-line-spins-through-the-turn ()
  (let ((agent-hq-spinner-working-frames '("x" "y")))
    (agent-hq-spinner-test--in-shell
      (insert "answer\n\n")
      (let ((start (point-marker)))
        (insert "Claude> ")
        (setq-local comint-last-prompt (cons start (point-marker)))
        ;; Starting up, with no prompt to answer: no line.
        (beat 0 'started)
        (beat 1 'busy)
        (should-not (agent-hq-spinner-test--working))
        (map-put! agent-shell--state :active-requests '(prompt))
        (beat 2 'busy)
        (should (equal (agent-hq-spinner-test--working) '(9 . "x Working…\n\n")))
        (should (eq (get-text-property 2 'face (overlay-get agent-hq-spinner--working
                                                            'before-string))
                    'agent-hq-spinner-working))
        (beat 3 'busy)
        (should (equal (agent-hq-spinner-test--working) '(9 . "y Working…\n\n")))
        ;; The turn's output goes above it, through commands and gaps alike.
        (agent-hq-spinner-test--write-above start "✓ Command ls\n\n")
        (beat 4 'busy)
        (should (equal (agent-hq-spinner-test--working) '(23 . "x Working…\n\n")))
        (should (equal (buffer-substring-no-properties 23 (point-max)) "Claude> "))
        ;; Mid-line, it starts a line of its own.
        (agent-hq-spinner-test--write-above start "streaming")
        (beat 5 'busy)
        (should (equal (agent-hq-spinner-test--working) '(32 . "\ny Working…\n\n")))
        ;; Frames are made once, for every shell.
        (let ((shown (overlay-get agent-hq-spinner--working 'before-string)))
          (beat 7 'busy)
          (should (eq (overlay-get agent-hq-spinner--working 'before-string) shown)))
        (beat nil 'ended)
        (should-not agent-hq-spinner--working)
        (should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest agent-hq-spinner-test-working-line-goes-to-the-end-without-a-prompt ()
  (agent-hq-spinner-test--in-shell
    (map-put! agent-shell--state :active-requests '(prompt))
    (insert "output\n")
    (beat 0 'started)
    (beat 1 'busy)
    (should (equal (car (agent-hq-spinner-test--working)) (point-max)))
    ;; A prompt printed after it: the line moves above that.
    (let ((start (point-marker)))
      (insert "Claude> ")
      (setq-local comint-last-prompt (cons start (point-marker)))
      (beat 2 'busy)
      (should (equal (car (agent-hq-spinner-test--working)) (marker-position start))))
    ;; And can be turned off.
    (let ((agent-hq-spinner-working-label nil))
      (beat 2 'busy)
      (should-not (agent-hq-spinner-test--working)))))

(defun agent-hq-spinner-test--chat-label (prompt rows)
  "Lay ROWS over the prompt at PROMPT as chat mode does, one per character."
  (remove-overlays (point-min) (point-max) 'agent-shell-chat--tag 'me-label)
  (seq-do-indexed (lambda (row offset)
                    (let ((overlay (make-overlay (+ prompt offset) (+ prompt offset 1))))
                      (overlay-put overlay 'agent-shell-chat--tag 'me-label)
                      (overlay-put overlay 'display row)))
                  rows))

(ert-deftest agent-hq-spinner-test-working-line-stays-below-chat-modes-blank-row ()
  ;; Chat mode draws the blank row above its `Me' label either as the
  ;; buffer's blank line or as the label's first row, switching as it
  ;; relabels: the line goes right before the `Me' row either way.
  (let ((agent-hq-spinner-working-frames '("a" "b")))
    (agent-hq-spinner-test--in-shell
      (map-put! agent-shell--state :active-requests '(prompt))
      (insert "✓ Thinking\n\n")
      (let ((start (point-marker)))
        (insert "Claude> ")
        (setq-local comint-last-prompt (cons start (point-marker)))
        (beat 0 'started)
        ;; The buffer's blank line, then the label.
        (agent-hq-spinner-test--chat-label start '(" Me \n" "\n"))
        (beat 1 'busy)
        (should (equal (agent-hq-spinner-test--working)
                       (cons (marker-position start) "b Working…\n\n")))
        ;; Relabeled: the blank row is now the label's own first row.  The
        ;; line follows at once, not on the next beat.
        (agent-hq-spinner-test--chat-label start '("\n" " Me \n" "\n"))
        (agent-shell-chat--label-prompts)
        (should (equal (agent-hq-spinner-test--working)
                       (cons (1+ start) "b Working…\n\n")))
        ;; And back.
        (agent-hq-spinner-test--chat-label start '(" Me \n" "\n"))
        (agent-shell-chat--label-prompts)
        (should (equal (car (agent-hq-spinner-test--working)) (marker-position start)))
        ;; Without chat mode, the prompt's start, as before.
        (remove-overlays (point-min) (point-max) 'agent-shell-chat--tag 'me-label)
        (beat 2 'busy)
        (should (equal (car (agent-hq-spinner-test--working)) (marker-position start)))))))

(ert-deftest agent-hq-spinner-test-draws-nothing-out-of-view ()
  (agent-hq-spinner-test--in-shell
    (beat 0 'started)
    (agent-hq-spinner-test--insert-label "pending" "execute" "ls")
    (setq visible nil)
    (beat 1 'busy)
    (should-not agent-hq-spinner--overlays)
    (setq visible t)
    (beat 2 'busy)
    (should agent-hq-spinner--overlays)))

(ert-deftest agent-hq-spinner-test-a-failing-beat-stops-the-shell-quietly ()
  (agent-hq-spinner-test--in-shell
    (beat 0 'started)
    (agent-hq-spinner-test--insert-label "pending" "execute" "ls")
    (let ((messages 0))
      (cl-letf (((symbol-function 'agent-hq-spinner--targets)
                 (lambda () (error "Boom")))
                ((symbol-function 'message)
                 (lambda (&rest _) (cl-incf messages))))
        (beat 1 'busy)
        (beat 2 'busy)
        (beat 3 'busy))
      (should (= messages 1))
      (should agent-hq-spinner--failed)
      (should-not agent-hq-spinner--overlays))))

;;; agent-hq-spinner-test.el ends here
