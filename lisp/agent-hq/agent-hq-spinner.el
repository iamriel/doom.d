;;; agent-hq-spinner.el --- Animated busy icons in agent shells -*- lexical-binding: t; -*-

;;; Commentary:

;; agent-shell marks a tool call still running, a plan step in progress
;; and the agent starting up with a static "◔", and a thought with a
;; static "✶".  Here, while the agent is busy, those icons spin instead:
;;
;;   ⠹ Command  git status
;;   ✻ Thinking
;;
;; Only the current turn's icons spin (a call a cancelled turn left
;; pending stays still), only the newest thought while it streams, and
;; only what a window shows.  Once the turn ends every icon is static
;; again.
;;
;; Each busy stretch of a shell (from its heartbeat starting to its
;; stopping) gets a token of its own.  Icons rendered meanwhile are
;; tagged with it in the `agent-hq-spinner' text property, so icons of
;; earlier turns are told apart wherever agent-shell put the output (above
;; its live prompt, for one).  On each beat of the heartbeat (10 a second
;; while busy), overlays show a frame over the tagged icons in view.  An
;; overlay's `display' is a frame string made once, so a beat allocates
;; next to nothing and leaves the header alone: the header's own
;; animation churned GC (agent-shell#718) and stays static.
;;
;; Turn it on with `agent-hq-spinner-setup'.

;;; Code:

(require 'map)

(declare-function agent-shell--status-config "agent-shell-styles")
(declare-function agent-shell--text-change-unsafe-p "agent-shell")
(defvar agent-shell--state)

(defgroup agent-hq-spinner nil
  "Animated busy icons in agent shells."
  :group 'agent-hq
  :prefix "agent-hq-spinner-")

(defcustom agent-hq-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for a running tool call, a plan step in progress, and start-up.
One per heartbeat, 10 a second.  Single-width glyphs keep the kind
beside them from shifting.  A subagent at work spins with them too, in
the list `agent-hq-subagents' shows above the prompt."
  :type '(repeat string))

(defcustom agent-hq-spinner-thinking-frames
  '("·" "✢" "✳" "✶" "✻" "✽" "✻" "✶" "✳" "✢")
  "Frames for the thought the agent is having, one per heartbeat.
Claude Code's thinking glyphs, there and back."
  :type '(repeat string))

(defvar agent-hq-spinner--in-plan nil
  "Non-nil while agent-shell formats a plan.")

(defvar-local agent-hq-spinner--turn nil
  "Token of this shell's busy stretch, nil while it is idle.")

(defvar-local agent-hq-spinner--overlays nil
  "Overlays drawing this shell's spinner frames.")

(defvar-local agent-hq-spinner--thought nil
  "(COUNT . MARKER): MARKER at the icon of thought number COUNT.")

(defvar-local agent-hq-spinner--failed nil
  "Non-nil once a beat failed here; the shell then stops spinning.")

;;; Tagging icons as they are rendered

(defun agent-hq-spinner--tag (text end kind)
  "Return a copy of TEXT, its first END characters tagged as spinner KIND.
Untagged while no turn is under way: nothing would spin them."
  (if agent-hq-spinner--turn
      (let ((text (copy-sequence text)))
        (put-text-property 0 end 'agent-hq-spinner (cons kind agent-hq-spinner--turn) text)
        text)
    text))

(defun agent-hq-spinner--status-label-a (orig &rest args)
  "Around advice for `agent-shell--make-status-kind-label'.
Tags the icon of a label for something under way: a pending or running
tool call, a plan step in progress, start-up.  Styles that show a word
instead of an icon are left as they are.  ORIG is called with ARGS."
  (let* ((label (apply orig args))
         (status (plist-get args :status))
         (icon (and (stringp label)
                    (if agent-hq-spinner--in-plan
                        (equal status "in_progress")
                      (member status '("pending" "in_progress")))
                    (map-elt (agent-shell--status-config status) :icon))))
    (if (and icon (string-prefix-p icon label))
        (agent-hq-spinner--tag label (length icon) 'busy)
      label)))

(defun agent-hq-spinner--format-plan-a (orig &rest args)
  "Around advice for `agent-shell--format-plan': ORIG with ARGS.
A step not started yet stays still; only the one in progress spins."
  (let ((agent-hq-spinner--in-plan t))
    (apply orig args)))

(defun agent-hq-spinner--thought-icon-a (icon)
  "Filter advice for `agent-shell--thought-process-icon': tag ICON."
  (and icon (agent-hq-spinner--tag icon (length icon) 'thought)))

;;; Animating them

(defun agent-hq-spinner--visible-ranges ()
  "Return (START . END) of each window showing this buffer."
  (mapcar (lambda (window)
            (cons (window-start window)
                  (min (point-max) (or (window-end window) (point-max)))))
          (get-buffer-window-list nil nil 'visible)))

(defun agent-hq-spinner--at (position kind)
  "Return non-nil if POSITION holds an icon of this turn tagged KIND."
  (let ((tag (get-text-property position 'agent-hq-spinner)))
    (and (eq (car-safe tag) kind)
         (eq (cdr tag) agent-hq-spinner--turn))))

(defun agent-hq-spinner--runs (kind start end)
  "Return (BEG . END) of each icon of this turn tagged KIND in START..END."
  (let ((position start) runs)
    (while (< position end)
      (let ((next (next-single-property-change position 'agent-hq-spinner nil end)))
        (when (agent-hq-spinner--at position kind)
          (push (cons position next) runs))
        (setq position next)))
    runs))

(defun agent-hq-spinner--current-thought ()
  "Return (BEG . END) of the icon of the thought streaming now, or nil."
  (when (equal (map-elt agent-shell--state :last-entry-type) "agent_thought_chunk")
    (let ((count (map-elt agent-shell--state :chunked-group-count))
          (cached agent-hq-spinner--thought))
      (unless (and (equal (car cached) count)
                   (agent-hq-spinner--at (cdr cached) 'thought))
        (when cached
          (set-marker (cdr cached) nil))
        ;; The thought is the newest output, so the last icon tagged.
        (setq cached
              (when-let* ((end (previous-single-property-change
                                (point-max) 'agent-hq-spinner))
                          ((agent-hq-spinner--at (1- end) 'thought)))
                (cons count (copy-marker (or (previous-single-property-change
                                              end 'agent-hq-spinner)
                                             (point-min))))))
        (setq agent-hq-spinner--thought cached))
      (when cached
        (let ((start (marker-position (cdr cached))))
          (cons start (next-single-property-change start 'agent-hq-spinner nil (point-max))))))))

(defun agent-hq-spinner--targets ()
  "Return the spinners in view: (BEG END . FRAMES) each."
  (let ((thought (agent-hq-spinner--current-thought))
        targets)
    (dolist (range (agent-hq-spinner--visible-ranges))
      (let ((start (car range))
            (end (cdr range)))
        (dolist (run (agent-hq-spinner--runs 'busy start end))
          (unless (assq (car run) targets)
            (push (cons (car run) (cons (cdr run) agent-hq-spinner-frames)) targets)))
        (when (and thought (< (car thought) end) (>= (car thought) start)
                   (not (assq (car thought) targets)))
          (push (cons (car thought) (cons (cdr thought) agent-hq-spinner-thinking-frames))
                targets))))
    targets))

(defun agent-hq-spinner--draw (beat)
  "Show frame BEAT of each spinner in view, reusing this shell's overlays."
  (let ((overlays agent-hq-spinner--overlays)
        kept)
    (dolist (target (agent-hq-spinner--targets))
      (let ((overlay (pop overlays))
            (start (car target))
            (end (cadr target))
            (frames (cddr target)))
        (cond ((null overlay)
               (setq overlay (make-overlay start end))
               ;; Gone with the icon when agent-shell rewrites the label.
               (overlay-put overlay 'evaporate t))
              ;; A beat mostly finds the overlay where the last left it:
              ;; leave it be rather than ask for a redisplay.  The frame
              ;; strings are the same objects beat to beat, so putting an
              ;; unchanged one is free as well.
              ((not (and (eq (overlay-buffer overlay) (current-buffer))
                         (eql (overlay-start overlay) start)
                         (eql (overlay-end overlay) end)))
               (move-overlay overlay start end (current-buffer))))
        (overlay-put overlay 'display (nth (mod beat (length frames)) frames))
        (push overlay kept)))
    (mapc #'delete-overlay overlays)
    ;; In target order, so the next beat pairs them up the same way.
    (setq agent-hq-spinner--overlays (nreverse kept))))

(defun agent-hq-spinner--stop ()
  "Put this shell's icons back to their static selves."
  (mapc #'delete-overlay agent-hq-spinner--overlays)
  (when agent-hq-spinner--thought
    (set-marker (cdr agent-hq-spinner--thought) nil))
  (setq agent-hq-spinner--overlays nil
        agent-hq-spinner--thought nil
        agent-hq-spinner--turn nil))

(defun agent-hq-spinner--beat (shell beat status)
  "Animate SHELL's spinners for heartbeat BEAT, whose STATUS is given.
STATUS is `started', `busy' or `ended', as `agent-shell-heartbeat-make'
says."
  (when (buffer-live-p shell)
    (with-current-buffer shell
      (pcase status
        ('started
         (agent-hq-spinner--stop)
         (setq agent-hq-spinner--turn (list 'turn)))
        ('ended (agent-hq-spinner--stop))
        ('busy
         (when (and agent-hq-spinner--turn
                    (not agent-hq-spinner--failed)
                    (integerp beat)
                    ;; Not mid-redisplay: agent-shell skips such beats too.
                    (not (agent-shell--text-change-unsafe-p))
                    (get-buffer-window shell 'visible))
           (condition-case err
               (agent-hq-spinner--draw beat)
             (error
              ;; Beats come 10 a second: say so once, not on each.
              (setq agent-hq-spinner--failed t)
              (agent-hq-spinner--stop)
              (message "agent-hq-spinner: stopped in %s: %S" (buffer-name) err)))))))))

(defun agent-hq-spinner--heartbeat-handler-a (orig shell-buffer)
  "Around advice for `agent-shell--make-heartbeat-handler'.
The handler ORIG makes for SHELL-BUFFER also animates its spinners."
  (let ((handler (funcall orig shell-buffer)))
    (lambda (beat status)
      (funcall handler beat status)
      (agent-hq-spinner--beat shell-buffer beat status))))

;;;###autoload
(defun agent-hq-spinner-setup ()
  "Animate busy icons in agent shells started from now on."
  (advice-add 'agent-shell--make-status-kind-label :around #'agent-hq-spinner--status-label-a)
  (advice-add 'agent-shell--format-plan :around #'agent-hq-spinner--format-plan-a)
  (advice-add 'agent-shell--thought-process-icon :filter-return
              #'agent-hq-spinner--thought-icon-a)
  (advice-add 'agent-shell--make-heartbeat-handler :around
              #'agent-hq-spinner--heartbeat-handler-a))

(provide 'agent-hq-spinner)

;;; agent-hq-spinner.el ends here
