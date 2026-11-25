;;; agent-hq-queue.el --- Queued prompts of agent shells, listed and sent now -*- lexical-binding: t; -*-

;;; Commentary:

;; A prompt submitted while the agent works is queued until the turn
;; ends, and agent-shell says so only in the echo area, gone with the
;; next command.  Here, as in Claude Code's terminal, the queue stays in
;; view just above the prompt, under the working line, until it is sent:
;;
;;   ✻ Working…
;;
;;   ◷ 2 queued, sent when this turn ends     M-RET now  , q e edit  , q d drop
;;     › just the filenames
;;     › sorted by size
;;
;;   Claude>
;;
;; Commands, from a shell or its viewport:
;;   `agent-hq-queue-send-now'  steer the queue into the running turn now,
;;                              as the terminal does once a tool call ends;
;;                              with no turn running (one stopped with
;;                              prompts left queued), submit them
;;   `agent-hq-queue-edit'      take the queue back into the prompt, ahead
;;                              of anything typed there, to edit and send
;;                              again (the terminal's ↑)
;; `agent-shell-prompt-queue-remove' drops one or all of them.
;;
;; Set `agent-shell-busy-submit-reroute-function' to
;; `agent-hq-queue-send-now' and M-RET at an empty prompt mid-turn sends
;; the whole queue now, without the question agent-shell's own rerouting
;; asks of the last prompt queued.  Once the turn has stopped, M-RET
;; submits as RET does, so the first line names the keys for each case
;; (`agent-hq-queue-key-hints', `agent-hq-queue-held-key-hints').
;;
;; The queue is agent-shell's `:pending-prompts'; the list is redrawn
;; after whatever changes it and whenever the prompt moves.
;;
;; Turn it on with `agent-hq-queue-setup'.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-hq-spinner)             ; where the working line goes

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-steering-supported-p "agent-shell")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--prompt-input-start "agent-shell-prompt")
(declare-function agent-shell--take-prompt-input "agent-shell-prompt")
(declare-function agent-shell--prompt-queue-process-next "agent-shell-prompt-queue")
(declare-function agent-shell-busy-submit-steer "agent-shell-prompt-queue")
(declare-function shell-maker-busy "shell-maker")
(declare-function evil-insert-state "evil-states")
(defvar agent-shell--state)
(defvar shell-maker-finish-output-hook)

(defgroup agent-hq-queue nil
  "Queued prompts of agent shells, listed above the prompt."
  :group 'agent-hq
  :prefix "agent-hq-queue-")

(defcustom agent-hq-queue-icon "◷"
  "What starts the queue's first line."
  :type 'string)

(defcustom agent-hq-queue-max-rows 5
  "Most queued prompts listed above a shell's prompt; the rest are counted."
  :type 'natnum)

(defcustom agent-hq-queue-key-hints "M-RET now  , q e edit  , q d drop"
  "Keys the queue's first line names mid-turn, at its right end when there's room.
Set them to match your bindings; nil names none."
  :type '(choice string (const :tag "None" nil)))

(defcustom agent-hq-queue-held-key-hints ", q s send  , q e edit  , q d drop"
  "Keys the queue's first line names once the turn stopped, held prompts left.
Then M-RET at an empty prompt submits that, not the queue (agent-shell
reroutes only mid-turn), so name `agent-hq-queue-send-now's own key."
  :type '(choice string (const :tag "None" nil)))

(defface agent-hq-queue-heading '((t :inherit bold))
  "The count of queued prompts.")

(defface agent-hq-queue-prompt '((t :inherit shadow))
  "Queued prompts and the rest of the queue's first line.")

(defvar-local agent-hq-queue--overlay nil
  "Overlay drawing this shell's queue above its prompt.")

(defvar-local agent-hq-queue--drawn nil
  "What `agent-hq-queue--overlay' was drawn for, to skip redrawing it.")

;;;; The list

(defun agent-hq-queue--pending ()
  "Return this shell's queued prompts, first to be sent first."
  (map-elt agent-shell--state :pending-prompts))

(defun agent-hq-queue--one-line (text width)
  "Return TEXT on one line, cut to WIDTH columns."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " (substring-no-properties text)))
   width nil nil "…"))

(defun agent-hq-queue--heading (count busy width)
  "Return the queue's first line for COUNT prompts, WIDTH columns at most.
BUSY says a turn is running, which sends them when it ends."
  (let* ((head (concat (propertize (format "%s %d queued" agent-hq-queue-icon count)
                                   'face 'agent-hq-queue-heading)
                       (propertize (if busy
                                       ", sent when this turn ends"
                                     ", held: the turn stopped")
                                   'face 'agent-hq-queue-prompt)))
         (hints (if busy agent-hq-queue-key-hints agent-hq-queue-held-key-hints)))
    (if (and hints (<= (+ (string-width head) 2 (string-width hints)) width))
        (concat head
                (propertize " " 'display `(space :align-to (- right ,(1+ (string-width hints)))))
                (propertize hints 'face 'agent-hq-queue-prompt))
      (truncate-string-to-width head width))))

(defun agent-hq-queue--text (pending busy width)
  "Return the list of PENDING prompts shown above a prompt, WIDTH columns wide.
BUSY says a turn is running.  Each prompt gets a row, all of it on one
line; past `agent-hq-queue-max-rows', the rest are counted."
  (let ((shown (seq-take pending agent-hq-queue-max-rows))
        (more (- (length pending) agent-hq-queue-max-rows)))
    (concat (agent-hq-queue--heading (length pending) busy width) "\n"
            (mapconcat (lambda (prompt)
                         (propertize (concat "  › " (agent-hq-queue--one-line prompt (- width 5)) "\n")
                                     'face 'agent-hq-queue-prompt))
                       shown "")
            (when (> more 0)
              (propertize (format "  … %d more\n" more) 'face 'agent-hq-queue-prompt)))))

(defun agent-hq-queue--width ()
  "Return how many columns the window showing this shell has, or 80."
  (if-let* ((window (get-buffer-window (current-buffer) t)))
      (1- (window-max-chars-per-line window))
    80))

(defun agent-hq-queue--hide ()
  "Take this shell's list away."
  (when agent-hq-queue--overlay
    (delete-overlay agent-hq-queue--overlay))
  (setq agent-hq-queue--overlay nil
        agent-hq-queue--drawn nil))

(defun agent-hq-queue--draw ()
  "Show this shell's queue where the working line goes, or nothing if empty.
That is right before the prompt (chat mode's `Me' row), so the list sits
under the working line and the list of subagents, nearest the prompt."
  (if-let* ((pending (agent-hq-queue--pending)))
      (let* ((anchor (agent-hq-spinner--working-anchor))
             (position (car anchor))
             (drawn (list pending (shell-maker-busy) (agent-hq-queue--width) (cdr anchor)))
             (overlay (or agent-hq-queue--overlay
                          ;; Advancing both ends keeps the list after
                          ;; whatever is written where it sits.
                          (let ((overlay (make-overlay position position nil t t)))
                            ;; Nearer the prompt than the working line (0)
                            ;; and the subagents (1) drawn there.
                            (overlay-put overlay 'priority 2)
                            (setq agent-hq-queue--overlay overlay)))))
        (unless (eql (overlay-start overlay) position)
          (move-overlay overlay position position))
        (unless (equal drawn agent-hq-queue--drawn)
          (setq agent-hq-queue--drawn drawn)
          (overlay-put overlay 'before-string
                       (concat (unless (cdr anchor) "\n")
                               (agent-hq-queue--text pending (nth 1 drawn) (nth 2 drawn))
                               "\n"))))
    (agent-hq-queue--hide)))

(defun agent-hq-queue-refresh (&optional shell)
  "Redraw SHELL's queue, by default this buffer's, if it is a shell."
  (with-current-buffer (or shell (current-buffer))
    (when (derived-mode-p 'agent-shell-mode)
      (with-demoted-errors "agent-hq-queue: %S"
        (agent-hq-queue--draw)))))

(defun agent-hq-queue--changed-a (&rest _)
  "After advice for what changes a queue: redraw every shell's.
Cheap: a shell whose queue and window are as drawn is left alone.  Some
run in the shell, some (removing) from wherever they were called."
  (mapc #'agent-hq-queue-refresh (agent-shell-buffers)))

(defun agent-hq-queue--follow-a (&rest _)
  "After advice for printing a prompt and chat mode's relabeling.
Keeps the list right before the prompt as the prompt moves."
  (when agent-hq-queue--overlay
    (agent-hq-queue-refresh)))

(defun agent-hq-queue--resized-h (window)
  "Redraw the queue of WINDOW's shell to its new width.
For a buffer-local `window-size-change-functions'."
  (when (and agent-hq-queue--overlay (window-live-p window))
    (agent-hq-queue-refresh (window-buffer window))))

;;;; Commands

(defun agent-hq-queue--shell ()
  "Return the shell queue commands act on.
This one, a viewport's, or else the project's."
  (agent-shell--shell-buffer :no-create t))

(defun agent-hq-queue--joined (pending)
  "Return PENDING prompts as one, as agent-shell merges them."
  (string-join pending "\n\n"))

;;;###autoload
(defun agent-hq-queue-send-now ()
  "Send this shell's queued prompts now instead of when the turn ends.
Mid-turn they're steered into it, together as one prompt, as Claude
Code's terminal hands over what you queued once a tool call ends: the
agent reads them before it carries on.  With no turn running, as when one
stopped with prompts left queued, they're submitted.

Steering a shell that waits on a permission answer asks first: a steer
the agent declines interrupts the turn, rejecting that permission."
  (interactive)
  (with-current-buffer (agent-hq-queue--shell)
    (let ((pending (or (agent-hq-queue--pending) (user-error "No queued prompts"))))
      (cond
       ((not (shell-maker-busy))
        (agent-shell--prompt-queue-process-next))
       ((not (agent-shell-steering-supported-p))
        (user-error "This agent can't take a prompt mid-turn; the queue goes when the turn ends"))
       (t
        (when (and (eq (agent-shell-status) 'blocked)
                   (not (y-or-n-p "The agent waits on you; a declined steer cancels that.  Send now anyway?")))
          (user-error "Not sent"))
        ;; The turn may have ended while the question was up, sending the
        ;; queue with it.
        (unless (and (eq pending (agent-hq-queue--pending)) (shell-maker-busy))
          (user-error "The queue was sent meanwhile"))
        (let ((sent nil))
          (map-put! agent-shell--state :pending-prompts nil)
          (unwind-protect
              (progn (agent-shell-busy-submit-steer (agent-hq-queue--joined pending))
                     (setq sent t))
            (unless sent
              (map-put! agent-shell--state :pending-prompts
                        (append pending (agent-hq-queue--pending))))
            (agent-hq-queue-refresh))))))))

;;;###autoload
(defun agent-hq-queue-edit ()
  "Take this shell's queued prompts back into its prompt, to edit them.
They go ahead of anything typed there, one blank line apart, and leave
the queue.  Submitting again queues them, as one prompt, while the
agent works; M-RET steers them into the turn instead."
  (interactive)
  (let ((shell (agent-hq-queue--shell)))
    (with-current-buffer shell
      (let ((pending (or (agent-hq-queue--pending) (user-error "No queued prompts"))))
        (unless (agent-shell--prompt-input-start)
          (user-error "No prompt to edit them at"))
        (let ((draft (agent-shell--take-prompt-input)))
          (map-put! agent-shell--state :pending-prompts nil)
          (goto-char (point-max))
          (insert (agent-hq-queue--joined (append pending (and draft (list draft)))))
          (agent-hq-queue-refresh))))
    (if-let* ((window (get-buffer-window shell)))
        (progn (select-window window)
               (goto-char (point-max))
               (when (bound-and-true-p evil-local-mode)
                 (evil-insert-state)))
      (message "Queued prompts are back in %s's prompt" (buffer-name shell)))))

;;;; Setup

;;;###autoload
(defun agent-hq-queue-setup-shell-h ()
  "Keep this shell's queue in view.  For `agent-shell-mode-hook'.
A turn's end redraws it: once the shell is idle, what's left is held."
  (add-hook 'shell-maker-finish-output-hook #'agent-hq-queue-refresh nil t)
  (add-hook 'window-size-change-functions #'agent-hq-queue--resized-h nil t))

;;;###autoload
(defun agent-hq-queue-setup ()
  "List each shell's queued prompts above its prompt; see the Commentary."
  (add-hook 'agent-shell-mode-hook #'agent-hq-queue-setup-shell-h)
  (dolist (function '(agent-shell--prompt-queue-enqueue
                      agent-shell--prompt-queue-process-next
                      agent-shell--prompt-queue-display
                      agent-shell-prompt-queue-remove
                      agent-shell-busy-submit-reroute-queue-to-steer))
    (advice-add function :after #'agent-hq-queue--changed-a))
  (advice-add 'agent-shell--print-prompt :after #'agent-hq-queue--follow-a)
  (advice-add 'agent-shell-chat--label-prompts :after #'agent-hq-queue--follow-a))

(provide 'agent-hq-queue)

;;; agent-hq-queue.el ends here
