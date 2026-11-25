;;; agent-hq-spinner.el --- Animated busy icons in agent shells -*- lexical-binding: t; -*-

;;; Commentary:

;; agent-shell marks a tool call still running, a plan step in progress
;; and the agent starting up with a static "◔".  Here, while the agent is
;; busy, that icon spins instead, and as in Claude Code a line at the end
;; of the turn's output spins for as long as the turn runs:
;;
;;   ⠹ Command  git status
;;
;;   ✻ Working…
;;
;; Only the current turn's icons spin (a call a cancelled turn left
;; pending stays still), and only what a window shows.  Once the turn
;; ends every icon is static again and the line goes.
;;
;; A thought's icon stays still: the working line already says the agent
;; is thinking, and a thought only shows up once the thinking is done
;; (Claude Code sends a summary), right before what it led to.  The
;; thinking itself happens in the gaps between tool calls, which the
;; working line covers.
;;
;; Each busy stretch of a shell (from its heartbeat starting to its
;; stopping) gets a token of its own.  Icons rendered meanwhile are
;; tagged with it in the `agent-hq-spinner' text property, so icons of
;; earlier turns are told apart wherever agent-shell put the output (above
;; its live prompt, for one).  On each beat of the clock every animation
;; shares (agent-hq-clock, 10 a second; the heartbeat only says when a
;; turn starts and ends), overlays show a frame over the tagged icons in
;; view, and the working line's frame.  Frames are strings made once, so a beat
;; allocates next to nothing and leaves the header alone: the header's
;; own animation churned GC (agent-shell#718) and stays static, but for
;; its logo (see agent-hq-logo).
;;
;; Turn it on with `agent-hq-spinner-setup'.

;;; Code:

(require 'map)
(require 'agent-hq-clock)

(declare-function agent-shell--status-config "agent-shell-styles")
(declare-function agent-shell--text-change-unsafe-p "agent-shell")
(declare-function agent-shell--active-requests-p "agent-shell")
(declare-function agent-shell--live-input-prompt-p "agent-shell-prompt")
(defvar agent-shell--state)
(defvar comint-last-prompt)

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

(defcustom agent-hq-spinner-working-frames
  '("·" "✢" "✳" "✶" "✻" "✽" "✻" "✶" "✳" "✢")
  "Frames for the line ending a turn's output while it runs, one per heartbeat.
Claude Code's thinking glyphs, there and back."
  :type '(repeat string))

(defcustom agent-hq-spinner-working-label "Working…"
  "What follows the spinner on the line ending a turn's output while it runs.
Nil for no such line."
  :type '(choice (string :tag "Label") (const :tag "No working line" nil)))

(defface agent-hq-spinner-working '((t :inherit shadow))
  "The label of the line that spins at the end of a running turn.")

(defvar agent-hq-spinner--in-plan nil
  "Non-nil while agent-shell formats a plan.")

(defvar-local agent-hq-spinner--turn nil
  "Token of this shell's busy stretch, nil while it is idle.")

(defvar-local agent-hq-spinner--overlays nil
  "Overlays drawing this shell's spinner frames.")

(defvar-local agent-hq-spinner--failed nil
  "Non-nil once a beat failed here; the shell then stops spinning.")

(defvar-local agent-hq-spinner--working nil
  "Overlay drawing the working line at the end of this shell's turn.")

(defvar-local agent-hq-spinner--working-beat 0
  "The beat whose frame the working line shows.")

(defvar agent-hq-spinner--working-strings nil
  "(LABEL FRAMES FRESH . MID): the working line's frames, made for LABEL and FRAMES.
FRESH holds one string per frame for a spot at a line's start; MID, for
one mid-line.")

;;; Tagging icons as they are rendered

(defun agent-hq-spinner--tag (text end)
  "Return a copy of TEXT, its first END characters tagged to spin this turn.
Untagged while no turn is under way: nothing would spin them."
  (if agent-hq-spinner--turn
      (let ((text (copy-sequence text)))
        (put-text-property 0 end 'agent-hq-spinner agent-hq-spinner--turn text)
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
        (agent-hq-spinner--tag label (length icon))
      label)))

(defun agent-hq-spinner--format-plan-a (orig &rest args)
  "Around advice for `agent-shell--format-plan': ORIG with ARGS.
A step not started yet stays still; only the one in progress spins."
  (let ((agent-hq-spinner--in-plan t))
    (apply orig args)))

;;; Animating them

(defun agent-hq-spinner--visible-ranges ()
  "Return (START . END) of each window showing this buffer."
  (mapcar (lambda (window)
            (cons (window-start window)
                  (min (point-max) (or (window-end window) (point-max)))))
          (get-buffer-window-list nil nil 'visible)))

(defun agent-hq-spinner--runs (start end)
  "Return (BEG . END) of each icon tagged this turn in START..END."
  (let ((position start) runs)
    (while (< position end)
      (let ((next (next-single-property-change position 'agent-hq-spinner nil end)))
        (when (and agent-hq-spinner--turn
                   (eq (get-text-property position 'agent-hq-spinner)
                       agent-hq-spinner--turn))
          (push (cons position next) runs))
        (setq position next)))
    runs))

(defun agent-hq-spinner--targets ()
  "Return (BEG . END) of each icon to spin in view."
  (let (targets)
    (dolist (range (agent-hq-spinner--visible-ranges))
      (dolist (run (agent-hq-spinner--runs (car range) (cdr range)))
        ;; Windows showing the same stretch find the same icons.
        (unless (assq (car run) targets)
          (push run targets))))
    targets))

(defun agent-hq-spinner--draw (beat)
  "Show frame BEAT of each spinner in view, reusing this shell's overlays."
  (let ((overlays agent-hq-spinner--overlays)
        kept)
    (dolist (target (agent-hq-spinner--targets))
      (let ((overlay (pop overlays))
            (start (car target))
            (end (cdr target)))
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
        (overlay-put overlay 'display
                     (nth (mod beat (length agent-hq-spinner-frames)) agent-hq-spinner-frames))
        (push overlay kept)))
    (mapc #'delete-overlay overlays)
    ;; In target order, so the next beat pairs them up the same way.
    (setq agent-hq-spinner--overlays (nreverse kept))))

(defun agent-hq-spinner--working-frames (fresh)
  "Return the working line once per frame: at a line's start if FRESH.
Made once for the label and frames customized, and shared by shells."
  (let ((cache agent-hq-spinner--working-strings))
    (unless (and (eq (nth 0 cache) agent-hq-spinner-working-label)
                 (eq (nth 1 cache) agent-hq-spinner-working-frames))
      (let ((lines (mapcar (lambda (frame)
                             ;; A blank line after, as between entries.
                             (concat frame " "
                                     (propertize agent-hq-spinner-working-label
                                                 'face 'agent-hq-spinner-working)
                                     "\n\n"))
                           agent-hq-spinner-working-frames)))
        (setq cache (cons agent-hq-spinner-working-label
                          (cons agent-hq-spinner-working-frames
                                (cons lines (mapcar (lambda (line) (concat "\n" line))
                                                    lines))))
              agent-hq-spinner--working-strings cache)))
    (if fresh (nth 2 cache) (nthcdr 3 cache))))

(defun agent-hq-spinner--chat-label-row (prompt)
  "Return where chat mode draws the `Me' row of the prompt at PROMPT, or nil.
Chat mode lays its label over the prompt's first characters, a row per
character, and draws the blank row above it one of two ways depending
on how the output ends: as the buffer's own blank line, above PROMPT,
or as the label's first row, at PROMPT.  It switches as it relabels,
on every agent event, so a line at PROMPT would hop a row either side
of that blank row.  Right before the `Me' row, it is below it either
way."
  (let ((overlays (overlays-in prompt (+ prompt 3)))
        found)
    (while (and overlays (not found))
      (let* ((overlay (car overlays))
             (display (overlay-get overlay 'display)))
        (when (and (eq (overlay-get overlay 'agent-shell-chat--tag) 'me-label)
                   (stringp display)
                   (string-match-p "[^\n]" display))
          (setq found (overlay-start overlay))))
      (setq overlays (cdr overlays)))
    found))

(defun agent-hq-spinner--working-anchor ()
  "Return (POSITION . FRESH) for the working line, FRESH if a row starts there.
Just above the live prompt, where agent-shell writes the turn, or at
the end while there is none.  With chat mode, right before its `Me'
row (see `agent-hq-spinner--chat-label-row')."
  (save-restriction
    (widen)
    (let ((prompt (and comint-last-prompt
                       (ignore-errors (agent-shell--live-input-prompt-p comint-last-prompt))
                       (marker-position (car comint-last-prompt)))))
      (if-let* ((row (and prompt (agent-hq-spinner--chat-label-row prompt))))
          (cons row t)
        (let ((position (or prompt (point-max))))
          (cons position (memq (char-before position) '(nil ?\n))))))))

(defun agent-hq-spinner--place-working ()
  "Show the working line where it goes, at frame `agent-hq-spinner--working-beat'."
  (let* ((anchor (agent-hq-spinner--working-anchor))
         (position (car anchor))
         (frames (agent-hq-spinner--working-frames (cdr anchor)))
         (overlay (or agent-hq-spinner--working
                      ;; Advancing both ends keeps it after whatever is
                      ;; written where it sits.
                      (setq agent-hq-spinner--working
                            (make-overlay position position nil t t)))))
    (unless (eql (overlay-start overlay) position)
      (move-overlay overlay position position))
    (overlay-put overlay 'before-string
                 (nth (mod agent-hq-spinner--working-beat (length frames)) frames))))

(defun agent-hq-spinner--draw-working (beat)
  "Show frame BEAT of the working line, after the turn's output.
Only while a prompt is being answered: start-up has its own spinner."
  (if (not (and agent-hq-spinner-working-label
                (agent-shell--active-requests-p agent-shell--state)))
      (agent-hq-spinner--hide-working)
    (setq agent-hq-spinner--working-beat beat)
    (agent-hq-spinner--place-working)))

(defun agent-hq-spinner--follow-chat-a (&rest _)
  "After advice for `agent-shell-chat--label-prompts'.
Moves the working line along with the prompt's label the moment chat
mode relabels it, rather than on the next beat: a beat later, the
relabeled rows would already have been drawn around it."
  (when (and agent-hq-spinner--working (not agent-hq-spinner--failed))
    (condition-case err
        (agent-hq-spinner--place-working)
      (error
       (setq agent-hq-spinner--failed t)
       (agent-hq-spinner--stop)
       (message "agent-hq-spinner: stopped in %s: %S" (buffer-name) err)))))

(defun agent-hq-spinner--hide-working ()
  "Take the working line away."
  (when agent-hq-spinner--working
    (delete-overlay agent-hq-spinner--working)
    (setq agent-hq-spinner--working nil)))

(defun agent-hq-spinner--stop ()
  "Put this shell's icons back to their static selves."
  (mapc #'delete-overlay agent-hq-spinner--overlays)
  (agent-hq-spinner--hide-working)
  (setq agent-hq-spinner--overlays nil
        agent-hq-spinner--turn nil))

(defun agent-hq-spinner--animate (shell beat)
  "Show SHELL's spinners at BEAT of the shared clock, if a window shows SHELL."
  (if (not (buffer-live-p shell))
      (agent-hq-clock-stop (cons 'agent-hq-spinner shell))
    (with-current-buffer shell
      (when (and agent-hq-spinner--turn
                 (not agent-hq-spinner--failed)
                 ;; Not mid-redisplay: agent-shell skips such beats too.
                 (not (agent-shell--text-change-unsafe-p))
                 (get-buffer-window shell 'visible))
        (condition-case err
            (progn (agent-hq-spinner--draw beat)
                   (agent-hq-spinner--draw-working beat))
          (error
           ;; Beats come 10 a second: say so once, not on each.
           (setq agent-hq-spinner--failed t)
           (agent-hq-spinner--stop)
           (agent-hq-clock-stop (cons 'agent-hq-spinner shell))
           (message "agent-hq-spinner: stopped in %s: %S" (buffer-name) err)))))))

(defun agent-hq-spinner--beat (shell _beat status)
  "Start or stop SHELL's spinners as its heartbeat's STATUS says.
STATUS is `started', `busy' or `ended', as `agent-shell-heartbeat-make'
says.  While busy, the shared clock (agent-hq-clock) turns them, in step
with every other animation; the heartbeat's own beats draw nothing."
  (when (buffer-live-p shell)
    (with-current-buffer shell
      (pcase status
        ('started
         (agent-hq-spinner--stop)
         (setq agent-hq-spinner--turn (list 'turn))
         (agent-hq-clock-start (cons 'agent-hq-spinner shell)
                               (lambda (beat) (agent-hq-spinner--animate shell beat))))
        ('ended
         (agent-hq-clock-stop (cons 'agent-hq-spinner shell))
         (agent-hq-spinner--stop))))))

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
  (advice-add 'agent-shell--make-heartbeat-handler :around
              #'agent-hq-spinner--heartbeat-handler-a)
  (advice-add 'agent-shell-chat--label-prompts :after #'agent-hq-spinner--follow-chat-a))

(provide 'agent-hq-spinner)

;;; agent-hq-spinner.el ends here
