;;; agent-hq-clock.el --- One clock for every agent-hq animation -*- lexical-binding: t; -*-

;;; Commentary:

;; The spinners of running tool calls and the working line
;; (agent-hq-spinner), header logos (agent-hq-logo), subagents at work
;; (agent-hq-subagents) and the dashboard's running rows all animate.
;; Each used to keep its own 10-a-second timer: agent-shell's heartbeat,
;; one per busy shell, or a timer of its own.  Timers that fire apart each
;; get a redisplay of their own, so two busy shells, the subagent list and
;; the dashboard made some 40 redisplays a second, each allocating, which
;; fed garbage collections that paused typing.
;;
;; Here one timer beats for all of them: each beat, every animation
;; draws its frame, and Emacs redisplays once.  A beat that finds a key
;; waiting draws nothing, so typing never waits on an animation; the next
;; beat shows the frame that is due then, so they keep their pace.
;;
;; Animations call `agent-hq-clock-start' with a key and a function, which
;; the clock calls with the beat number until `agent-hq-clock-stop' with
;; that key; the timer runs only while some animation does.
;;
;; agent-shell's heartbeat still says when a turn starts and ends, and it
;; still draws agent-shell's own busy labels, which here are static (see
;; `agent-shell-busy-indicator-frames'): nothing needs it ten times a
;; second.  `agent-hq-clock-setup' slows it to
;; `agent-hq-clock-heartbeat-rate'.

;;; Code:

(require 'cl-lib)

(defgroup agent-hq-clock nil
  "One clock for every agent-hq animation."
  :group 'agent-hq
  :prefix "agent-hq-clock-")

(defcustom agent-hq-clock-fps 10
  "Beats a second of the clock all agent-hq animations draw on."
  :type 'number)

(defcustom agent-hq-clock-heartbeat-rate 2
  "Beats a second of agent-shell's heartbeat, which `agent-hq-clock-setup' sets.
The heartbeat says when a turn starts and ends and draws agent-shell's
busy labels; agent-hq's animations follow this clock instead.  Nil
leaves agent-shell's rate alone."
  :type '(choice (const :tag "agent-shell's own" nil) number))

(defvar agent-hq-clock-beat 0
  "Beats so far: the frame every animation is on.")

(defvar agent-hq-clock--animations nil
  "What draws on each beat, as an alist of (KEY . FUNCTION).")

(defvar agent-hq-clock--timer nil
  "The timer beating while any animation runs.")

(defun agent-hq-clock--tick ()
  "Beat: have every animation draw its frame, unless a key is waiting."
  (cl-incf agent-hq-clock-beat)
  (unless (input-pending-p)
    ;; A copy: an animation may stop itself, or start another, as it draws.
    (dolist (animation (copy-sequence agent-hq-clock--animations))
      (condition-case err
          (funcall (cdr animation) agent-hq-clock-beat)
        (error
         ;; Beats come ten a second: say so once, and stop that one.
         (message "agent-hq-clock: stopped %S: %S" (car animation) err)
         (agent-hq-clock-stop (car animation)))))))

(defun agent-hq-clock-start (key function)
  "Call FUNCTION with the beat on each beat of the clock, as KEY.
Starting KEY again replaces its FUNCTION.  Beats come
`agent-hq-clock-fps' a second, at wall-clock multiples of the interval,
until `agent-hq-clock-stop' KEY."
  (setf (alist-get key agent-hq-clock--animations nil nil #'equal) function)
  (unless (timerp agent-hq-clock--timer)
    (let ((interval (/ 1.0 agent-hq-clock-fps)))
      (setq agent-hq-clock--timer
            (run-at-time (- interval (mod (float-time) interval)) interval
                         #'agent-hq-clock--tick)))))

(defun agent-hq-clock-stop (key)
  "Stop calling KEY's function; stop the clock once no animation runs."
  (setq agent-hq-clock--animations
        (cl-remove key agent-hq-clock--animations :key #'car :test #'equal))
  (when (and (null agent-hq-clock--animations) (timerp agent-hq-clock--timer))
    (cancel-timer agent-hq-clock--timer)
    (setq agent-hq-clock--timer nil)))

(defun agent-hq-clock-running-p (key)
  "Return non-nil while KEY's animation runs."
  (and (assoc key agent-hq-clock--animations) t))

(defun agent-hq-clock--heartbeat-rate-a (args)
  "Filter-args advice for `agent-shell-heartbeat-make': beat at our rate.
ARGS are its keyword arguments."
  (if (and agent-hq-clock-heartbeat-rate
           (not (plist-member args :beats-per-second)))
      (append args (list :beats-per-second agent-hq-clock-heartbeat-rate))
    args))

;;;###autoload
(defun agent-hq-clock-setup ()
  "Slow agent-shell's heartbeat to `agent-hq-clock-heartbeat-rate'.
For shells started from now on."
  (advice-add 'agent-shell-heartbeat-make :filter-args #'agent-hq-clock--heartbeat-rate-a))

(provide 'agent-hq-clock)

;;; agent-hq-clock.el ends here
