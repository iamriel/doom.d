;;; agent-hq-attention.el --- See and answer the agent shells waiting on you -*- lexical-binding: t; -*-

;;; Commentary:

;; A shell is "waiting on you" while a permission request or a question
;; form of its turn waits for your answer, and "unseen" once its turn
;; ended while you were elsewhere and you haven't looked at it since (as
;; agent-shell-attention counts it).  `agent-hq-attention-mode' makes
;; both hard to miss and quick to deal with:
;;
;; - A badge leads the selected window's mode line: a red
;;   " ⚠ webapp:wt1 2m +1 " for the shell waiting on you longest (and how
;;   many more wait), then a yellow " ●2 " counting unseen answers.  It
;;   leaves out the shell that window shows, and is gone when nothing
;;   needs you.  It blinks when a shell starts waiting on you while you
;;   look elsewhere.  mouse-1 answers, mouse-3 opens the dashboard.
;; - macOS notifications, in two tiers: a shell waiting on you posts one
;;   with a sound unless you're looking at it; a finished turn posts a
;;   quiet one only while no Emacs frame has focus.  Each goes away once
;;   dealt with.  Clicking one brings Emacs forward, asking at once what
;;   to answer when the shell still waits.
;; - `agent-hq-answer' answers a permission request from wherever you
;;   are, without switching windows or workspaces: y allow, ! always,
;;   n reject, v the diff or the whole request, g go to the shell.  The
;;   answer is sent as the shell's own buttons send it.
;; - `agent-hq-attention-jump' goes to the shell needing you most: those
;;   waiting on you first, then the unseen.
;;
;; Who waits is `agent-hq--status' (agent-shell's own status, which
;; counts question forms too); who's unseen is agent-shell-attention's.

;;; Code:

(require 'agent-hq)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell--send-permission-response "agent-shell")
(declare-function agent-shell--permission-title "agent-shell")
(declare-function agent-shell--make-diff-viewing-function "agent-shell")
(declare-function agent-shell--shorten-paths "agent-shell")
(declare-function agent-shell-elicitation--pending-p "agent-shell-elicitation")
(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")
(declare-function agent-shell-attention-unread-p "agent-shell-attention")
(declare-function agent-shell-attention-acknowledge "agent-shell-attention")
(declare-function agent-shell-attention-dashboard "agent-shell-attention")
(declare-function agent-hq-dashboard "agent-hq-dashboard")
(defvar agent-shell--state)
(defvar agent-shell-attention--pending)
(defvar agent-shell-attention-awaiting-stop-reasons)
(defvar agent-hq-attention-mode)
(defvar agent-hq-dashboard--answer)
(defvar +modeline-format-alist)
(defvar +modeline-format-left)

(defgroup agent-hq-attention nil
  "Seeing and answering the agent shells waiting on you."
  :group 'agent-hq
  :prefix "agent-hq-attention-")

(defcustom agent-hq-attention-notify-blocked 'away
  "When a shell starting to wait on you posts a macOS notification.
`away': unless the selected window of a focused Emacs frame shows it.
`unfocused': only while no Emacs frame has focus.  Nil: never."
  :type '(choice (const :tag "Unless you're looking at it" away)
                 (const :tag "Only while Emacs isn't focused" unfocused)
                 (const :tag "Never" nil)))

(defcustom agent-hq-attention-notify-done 'unfocused
  "When a shell's turn ending posts a macOS notification.
Values as for `agent-hq-attention-notify-blocked'.  The badge counts
such turns either way, once you were away from the shell."
  :type '(choice (const :tag "Unless you're looking at it" away)
                 (const :tag "Only while Emacs isn't focused" unfocused)
                 (const :tag "Never" nil)))

(defcustom agent-hq-attention-sound "default"
  "Sound of the notification about a shell waiting on you, or nil for none.
A name from /System/Library/Sounds, or \"default\".  Finished turns
post silently."
  :type '(choice (const :tag "Silent" nil) string))

(defcustom agent-hq-attention-blinks 3
  "Times the badge blinks when a shell starts waiting while you look elsewhere."
  :type 'natnum)

(defcustom agent-hq-attention-tick 10
  "Seconds between updates of the badge, for its ages."
  :type 'number)

(defcustom agent-hq-attention-details-buffer-name "*Agent request*"
  "Name of the buffer `agent-hq-answer' shows a whole request in."
  :type 'string)

(defface agent-hq-attention-blocked
  '((t :inherit error :inverse-video t :weight bold))
  "The badge's part for the shells waiting on you.")

(defface agent-hq-attention-blink
  '((t :inherit warning :inverse-video t :weight bold))
  "The badge's part for the shells waiting on you, as it blinks.")

(defface agent-hq-attention-unseen
  '((t :inherit warning :weight bold))
  "The badge's count of answers you haven't seen.")

;;;; Who needs you

(defvar-local agent-hq-attention--since nil
  "When this shell started waiting on you, as a float time, or nil.")

(defvar-local agent-hq-attention--label-cache nil
  "(DIRECTORY . LABEL): this shell's repo:worktree while in DIRECTORY.")

(defun agent-hq-attention--label (shell)
  "Return SHELL's repo:worktree, worked out again only when it moves."
  (with-current-buffer shell
    (unless (equal (car agent-hq-attention--label-cache) default-directory)
      (setq agent-hq-attention--label-cache
            (cons default-directory (agent-hq--shell-label shell (agent-hq-context)))))
    (cdr agent-hq-attention--label-cache)))

(defun agent-hq-attention--state (shell)
  "Return SHELL's agent-shell state, or nil."
  (ignore-errors (buffer-local-value 'agent-shell--state shell)))

(defun agent-hq-attention--permission (shell)
  "Return (TOOL-CALL-ID . TOOL-CALL) of SHELL's oldest permission request, or nil.
agent-shell keeps the newest tool call first."
  (car (last (seq-filter (lambda (entry) (map-elt (cdr entry) :permission-request-id))
                         (map-elt (agent-hq-attention--state shell) :tool-calls)))))

(defun agent-hq-attention--form-p (shell)
  "Return non-nil when SHELL shows a question form waiting for your answer."
  (and (fboundp 'agent-shell-elicitation--pending-p)
       (ignore-errors (agent-shell-elicitation--pending-p :shell-buffer shell))))

(defun agent-hq-attention--shell-of (buffer)
  "Return the shell BUFFER is, or stands in for as its viewport, or nil."
  (when (buffer-live-p buffer)
    (let ((mode (buffer-local-value 'major-mode buffer)))
      (cond ((provided-mode-derived-p mode 'agent-shell-mode) buffer)
            ((and (fboundp 'agent-shell-viewport--shell-buffer)
                  (provided-mode-derived-p mode '(agent-shell-viewport-view-mode
                                                  agent-shell-viewport-edit-mode)))
             (ignore-errors (agent-shell-viewport--shell-buffer buffer)))))))

(defun agent-hq-attention--one-line (text)
  "Return TEXT's first non-blank line, trimmed, with \" …\" when more follow."
  (let ((lines (seq-remove #'string-blank-p (split-string (or text "") "\n"))))
    (concat (string-trim (replace-regexp-in-string "[ \t]+" " " (or (car lines) "")))
            (if (cdr lines) " …" ""))))

(defun agent-hq-attention--raw-input (tool-call)
  "Return TOOL-CALL's raw input when it is an alist, else nil."
  (let ((raw (map-elt tool-call :raw-input)))
    (and (consp raw) raw)))

(defun agent-hq-attention--what (shell tool-call)
  "Return one line saying what TOOL-CALL, one of SHELL's, asks to do.
A command shows as \"$ command\", a plan as such, anything else by
its title, with paths shortened as SHELL shows them."
  (let* ((raw (agent-hq-attention--raw-input tool-call))
         (command (map-elt raw 'command))
         (title (map-elt tool-call :title))
         (text (cond ((stringp (map-elt raw 'plan)) "Plan ready for review")
                     ((and (equal (map-elt tool-call :kind) "execute")
                           (or (stringp command) (stringp title)))
                      (concat "$ " (if (stringp command) command title)))
                     ((stringp title) title)
                     (t "A tool call"))))
    (string-replace "`" "" (agent-hq-attention--one-line
                            (or (and (fboundp 'agent-shell--shorten-paths)
                                     (ignore-errors (with-current-buffer shell
                                                      (agent-shell--shorten-paths text))))
                                text)))))

(defun agent-hq-attention--blocked-what (shell)
  "Return one line saying what SHELL waits on you for."
  (if-let* ((permission (agent-hq-attention--permission shell)))
      (agent-hq-attention--what shell (cdr permission))
    "A question waits for your answer"))

(defun agent-hq-attention--unseen-since (shell)
  "Return when SHELL's turn ended unseen, as agent-shell-attention noted it.
Nil when SHELL has no answer you haven't looked at."
  (when (and (fboundp 'agent-shell-attention-unread-p)
             (agent-shell-attention-unread-p shell))
    (let ((entry (and (boundp 'agent-shell-attention--pending)
                      (hash-table-p agent-shell-attention--pending)
                      (gethash shell agent-shell-attention--pending))))
      (if (and (consp entry) (numberp (cdr entry)))
          (cdr entry)
        (float-time)))))

(defun agent-hq-attention--entries (&optional now)
  "Return the shells needing you as of NOW, those waiting on you first.
Each group goes longest waiting first.  An entry is a plist: :shell,
:kind (`blocked' or `unseen'), :since (a float time), :label (its
repo:worktree) and :what (what it waits on you for, or its title)."
  (let ((now (or now (float-time)))
        blocked unseen)
    (dolist (shell (agent-hq--shells))
      (let ((status (agent-hq--status shell)))
        (if (eq status 'blocked)
            (push (list :shell shell :kind 'blocked
                        :since (with-current-buffer shell
                                 (or agent-hq-attention--since
                                     (setq agent-hq-attention--since now)))
                        :label (agent-hq-attention--label shell)
                        :what (agent-hq-attention--blocked-what shell))
                  blocked)
          (with-current-buffer shell
            (setq agent-hq-attention--since nil))
          (when-let* (((eq status 'ready))
                      (since (agent-hq-attention--unseen-since shell)))
            (push (list :shell shell :kind 'unseen :since since
                        :label (agent-hq-attention--label shell)
                        :what (or (agent-hq--session-title shell) "Finished"))
                  unseen)))))
    (let ((by-age (lambda (a b) (< (plist-get a :since) (plist-get b :since)))))
      (append (sort blocked by-age) (sort unseen by-age)))))

(defun agent-hq-attention--of-kind (kind entries)
  "Return the shells of ENTRIES of KIND, in order."
  (delq nil (mapcar (lambda (entry) (and (eq (plist-get entry :kind) kind)
                                         (plist-get entry :shell)))
                    entries)))

(defun agent-hq-attention--blocked-shells ()
  "Return the shells waiting on you, longest waiting first."
  (agent-hq-attention--of-kind 'blocked (agent-hq-attention--entries)))

(defun agent-hq-attention--focused-p ()
  "Return non-nil when an Emacs frame has input focus."
  (seq-some #'frame-focus-state (frame-list)))

(defun agent-hq-attention--looking-at-p (shell)
  "Return non-nil when the selected window of a focused Emacs frame shows SHELL."
  (and (agent-hq-attention--focused-p)
       (eq (agent-hq-attention--shell-of (window-buffer (selected-window))) shell)))

(defun agent-hq-attention--wants-notice-p (shell when)
  "Return non-nil when SHELL calls for a notice, as WHEN says.
WHEN is a value of `agent-hq-attention-notify-blocked'."
  (pcase when
    ('away (not (agent-hq-attention--looking-at-p shell)))
    ('unfocused (not (agent-hq-attention--focused-p)))))

;;;; The badge

(defvar agent-hq-attention--current nil
  "The shells needing you as last worked out, from `agent-hq-attention--entries'.")

(defvar agent-hq-attention--drawn nil
  "What the badge showed as of the last update, to tell when it changes.")

(defvar agent-hq-attention--memo nil
  "Badges drawn since the last update, as (LEFT-OUT-SHELL . STRING).")

(defvar agent-hq-attention--blink-on nil
  "Non-nil while the badge shows its blink face.")

(defvar agent-hq-attention--blink-timer nil
  "Timer blinking the badge, or nil.")

(defun agent-hq-attention--age (seconds)
  "Return SECONDS as \"2m\", \"3h\" or \"2d\", or nil under a minute."
  (cond ((not (numberp seconds)) nil)
        ((< seconds 60) nil)
        ((< seconds 3600) (format "%dm" (floor seconds 60)))
        ((< seconds 86400) (format "%dh" (floor seconds 3600)))
        (t (format "%dd" (floor seconds 86400)))))

(defun agent-hq-attention--key (entries now)
  "Return what drawing ENTRIES as of NOW depends on."
  (mapcar (lambda (entry)
            (list (plist-get entry :shell) (plist-get entry :kind)
                  (plist-get entry :label) (plist-get entry :what)
                  (agent-hq-attention--age (- now (plist-get entry :since)))))
          entries))

(defun agent-hq-attention--help (entries now)
  "Return the badge's tooltip: each of ENTRIES as of NOW, and the clicks."
  (let ((lines (lambda (kind heading)
                 (when-let* ((of-kind (seq-filter (lambda (entry) (eq (plist-get entry :kind) kind))
                                                  entries)))
                   (concat heading "\n"
                           (mapconcat
                            (lambda (entry)
                              (let ((age (agent-hq-attention--age (- now (plist-get entry :since)))))
                                (format "  %s  %s%s" (plist-get entry :label) (plist-get entry :what)
                                        (if age (format "  (%s)" age) ""))))
                            of-kind "\n")
                           "\n")))))
    (concat (funcall lines 'blocked "Waiting on you:")
            (funcall lines 'unseen "Finished, not looked at:")
            "mouse-1: answer or go there   mouse-3: dashboard")))

(defvar agent-hq-attention--mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'agent-hq-attention-mode-line-click)
    (define-key map [mode-line mouse-3] #'agent-hq-attention-mode-line-dashboard)
    map)
  "Keys of the badge.")

(defun agent-hq-attention--mode-line-text (text)
  "Return TEXT with each % doubled, as a mode line shows it as is."
  (string-replace "%" "%%" text))

(defun agent-hq-attention--badge (entries &optional now)
  "Return the badge for ENTRIES, as `agent-hq-attention--entries' gives them.
That is \"\" when ENTRIES is empty."
  (let* ((now (or now (float-time)))
         (blocked (seq-filter (lambda (entry) (eq (plist-get entry :kind) 'blocked)) entries))
         (unseen (seq-filter (lambda (entry) (eq (plist-get entry :kind) 'unseen)) entries))
         (parts
          (delq nil
                (list
                 (when-let* ((first (car blocked)))
                   (let ((age (agent-hq-attention--age (- now (plist-get first :since)))))
                     (propertize
                      (concat " ⚠ " (agent-hq-attention--mode-line-text (plist-get first :label))
                              (if age (concat " " age) "")
                              (if (cdr blocked) (format " +%d" (length (cdr blocked))) "")
                              " ")
                      'face (if agent-hq-attention--blink-on
                                'agent-hq-attention-blink
                              'agent-hq-attention-blocked))))
                 (when unseen
                   (propertize (format " ●%d " (length unseen)) 'face 'agent-hq-attention-unseen))))))
    (if parts
        (propertize (concat (apply #'concat parts) " ")
                    'help-echo (agent-hq-attention--help entries now)
                    'mouse-face 'mode-line-highlight
                    'local-map agent-hq-attention--mode-line-map)
      "")))

(defun agent-hq-attention-mode-line-string ()
  "Return the badge for the mode line being drawn, or \"\".
Only the selected window's mode line has one, and it leaves out the
shell that window shows."
  (if (not (and agent-hq-attention-mode
                agent-hq-attention--current
                (or (not (fboundp 'mode-line-window-selected-p))
                    (mode-line-window-selected-p))))
      ""
    (let ((left-out (agent-hq-attention--shell-of (current-buffer))))
      (with-memoization (alist-get left-out agent-hq-attention--memo)
        (agent-hq-attention--badge
         (seq-remove (lambda (entry) (eq (plist-get entry :shell) left-out))
                     agent-hq-attention--current))))))

(defun agent-hq-attention-mode-line-width ()
  "Return how many columns the badge takes in the mode line being drawn."
  (string-width (agent-hq-attention-mode-line-string)))

(defvar agent-hq-attention-mode-line '(:eval (agent-hq-attention-mode-line-string))
  "The badge, as a mode line construct; see `agent-hq-attention-mode'.")
(put 'agent-hq-attention-mode-line 'risky-local-variable t)

(defun agent-hq-attention-mode-line-click (_event)
  "Answer the shell waiting on you longest, else go to the one needing you."
  (interactive "e")
  (call-interactively (if (agent-hq-attention--blocked-shells)
                          #'agent-hq-answer
                        #'agent-hq-attention-jump)))

(defun agent-hq-attention-mode-line-dashboard (_event)
  "Show the agents dashboard."
  (interactive "e")
  (require 'agent-hq-dashboard)
  (agent-hq-dashboard))

(defun agent-hq-attention--blink ()
  "Blink the badge `agent-hq-attention-blinks' times, unless it already blinks."
  (when (and (natnump agent-hq-attention-blinks)
             (> agent-hq-attention-blinks 0)
             (not (timerp agent-hq-attention--blink-timer)))
    (let ((flips (* 2 agent-hq-attention-blinks)))
      (setq agent-hq-attention--blink-timer
            (run-at-time
             0 0.3
             (lambda ()
               (cl-decf flips)
               (setq agent-hq-attention--blink-on (cl-oddp flips))
               (when (<= flips 0)
                 (cancel-timer agent-hq-attention--blink-timer)
                 (setq agent-hq-attention--blink-timer nil))
               (setq agent-hq-attention--memo nil)
               (force-mode-line-update t)))))))

;;;; Keeping up

(defvar agent-hq-attention--posted nil
  "Notifications up about shells needing you: (SHELL KIND . GROUP).
KIND, `blocked', `unseen' or `stopped', says how long it stays up; see
`agent-hq-attention--still-up-p'.")

(defvar agent-hq-attention--update-timer nil
  "Timer of an update coming once the current command or event is done.")

(defvar agent-hq-attention--tick-timer nil
  "Timer updating the badge every `agent-hq-attention-tick' seconds.")

(defun agent-hq-attention--schedule (&rest _)
  "Update the badge and notifications once the current command or event is done."
  (when (and agent-hq-attention-mode (not (timerp agent-hq-attention--update-timer)))
    (setq agent-hq-attention--update-timer
          (run-at-time 0 nil #'agent-hq-attention--update))))

(defun agent-hq-attention--still-up-p (shell kind entries)
  "Return non-nil while a notification of KIND about SHELL still applies.
ENTRIES are the shells needing you now.  `blocked' and `unseen' last
while SHELL has an entry of that kind; `stopped', a turn that ended in
a way agent-shell-attention doesn't count, until SHELL works again or
you look at it."
  (pcase kind
    ((or 'blocked 'unseen)
     (seq-some (lambda (entry) (and (eq (plist-get entry :shell) shell)
                                    (eq (plist-get entry :kind) kind)))
               entries))
    ('stopped (and (buffer-live-p shell)
                   (eq (agent-hq--status shell) 'ready)
                   (not (agent-hq-attention--looking-at-p shell))))))

(defun agent-hq-attention--withdraw (entries)
  "Take away each notification whose shell no longer needs you as it did.
ENTRIES are the shells needing you now."
  (setq agent-hq-attention--posted
        (seq-filter
         (pcase-lambda (`(,shell ,kind . ,group))
           (or (agent-hq-attention--still-up-p shell kind entries)
               (progn (agent-hq-remove-notification group) nil)))
         agent-hq-attention--posted)))

(defun agent-hq-attention--update ()
  "Work out who needs you again, and redraw the badge if that changed.
A shell starting to wait on you while you look elsewhere makes the
badge blink; one dealt with has its notification taken away."
  (setq agent-hq-attention--update-timer nil)
  (when agent-hq-attention-mode
    (with-demoted-errors "agent-hq-attention: %S"
      (let* ((now (float-time))
             (was (agent-hq-attention--of-kind 'blocked agent-hq-attention--current))
             (entries (agent-hq-attention--entries now))
             (key (agent-hq-attention--key entries now)))
        (setq agent-hq-attention--current entries)
        (when (seq-some (lambda (shell) (and (not (memq shell was))
                                             (agent-hq-attention--wants-notice-p shell 'away)))
                        (agent-hq-attention--of-kind 'blocked entries))
          (agent-hq-attention--blink))
        (agent-hq-attention--withdraw entries)
        (unless (equal key agent-hq-attention--drawn)
          (setq agent-hq-attention--drawn key
                agent-hq-attention--memo nil)
          (force-mode-line-update t))))))

;;;; Notifications

(defun agent-hq-attention--post (shell kind title body &rest keys)
  "Post a notification of KIND about SHELL with TITLE, BODY and KEYS.
KEYS are those of `agent-hq-post-notification'.  Clicking it calls
`agent-hq-attention-clicked'."
  (when-let* ((group (apply #'agent-hq-post-notification shell title body
                            :click `(agent-hq-attention-clicked
                                     ,(agent-hq--shell-id shell) ,(buffer-name shell))
                            keys)))
    (setf (alist-get shell agent-hq-attention--posted) (cons kind group))))

(defun agent-hq-attention--notify-blocked (shell)
  "Post that SHELL waits on you, as `agent-hq-attention-notify-blocked' says."
  (when (and (buffer-live-p shell)
             (agent-hq-attention--wants-notice-p shell agent-hq-attention-notify-blocked))
    (agent-hq-attention--post shell 'blocked
                              (format "%s needs you" (agent-hq-attention--label shell))
                              (agent-hq-attention--blocked-what shell)
                              :subtitle (agent-hq--session-title shell)
                              :sound agent-hq-attention-sound)))

(defun agent-hq-attention--counted-unseen-p (reason)
  "Return non-nil when a turn ending for REASON counts as unseen until looked at.
As agent-shell-attention counts it: by default, a turn that simply ended."
  (member (or reason "end_turn")
          (if (boundp 'agent-shell-attention-awaiting-stop-reasons)
              agent-shell-attention-awaiting-stop-reasons
            '("end_turn"))))

(defun agent-hq-attention--notify-done (shell reason)
  "Post that SHELL's turn ended for REASON, if `agent-hq-attention-notify-done'.
Run once every subscriber has seen the turn end, so its answer is known.
A cancelled turn was your doing and posts nothing; nor does a shell
already working again, as on a queued prompt."
  (when (and agent-hq-attention-mode
             (buffer-live-p shell)
             (not (equal reason "cancelled"))
             (eq (agent-hq--status shell) 'ready)
             (agent-hq-attention--wants-notice-p shell agent-hq-attention-notify-done))
    (let ((answer (and (local-variable-p 'agent-hq-dashboard--answer shell)
                       (buffer-local-value 'agent-hq-dashboard--answer shell)))
          (title (agent-hq--session-title shell))
          (counted (agent-hq-attention--counted-unseen-p reason)))
      (agent-hq-attention--post shell (if counted 'unseen 'stopped)
                                (format "%s %s" (agent-hq-attention--label shell)
                                        (if counted "finished" (format "stopped (%s)" reason)))
                                (or answer title "Your turn")
                                :subtitle (and answer title)))))

(defun agent-hq-attention--find (id name)
  "Return the live shell with ID (see `agent-hq--shell-id'), else named NAME."
  (or (and id (seq-find (lambda (shell) (equal (agent-hq--shell-id shell) id))
                        (agent-hq--shells)))
      (and name (agent-hq-attention--shell-of (get-buffer name)))))

(defun agent-hq-attention--visit (shell)
  "Show SHELL in its workspace, counting its answer as seen."
  (when (fboundp 'agent-shell-attention-acknowledge)
    (agent-shell-attention-acknowledge shell))
  (agent-hq-visit-shell shell))

(defun agent-hq-attention--act-on (shell name)
  "Ask what to answer SHELL if it still waits on you, else visit it.
NAME says which shell is gone, when it is."
  (condition-case err
      (cond ((not (buffer-live-p shell)) (message "Agent shell %s is gone" name))
            ((agent-hq-attention--permission shell) (agent-hq-answer shell))
            (t (agent-hq-attention--visit shell)))
    (user-error (message "%s" (error-message-string err)))))

;;;###autoload
(defun agent-hq-attention-clicked (id name)
  "Bring Emacs forward for the notification about the shell with ID, else NAME.
A shell still waiting on a permission has `agent-hq-answer' ask what to
answer; any other is visited.  Notifications run this through
emacsclient: it returns at once, so the client never waits on you."
  (let ((shell (agent-hq-attention--find id name)))
    (select-frame-set-input-focus (selected-frame))
    (run-at-time 0 nil #'agent-hq-attention--act-on shell name)
    nil))

(defun agent-hq-attention--on-event (shell event)
  "Post what SHELL's EVENT calls for, and update the badge soon.
An error here must not reach agent-shell, which dispatches the event."
  (when agent-hq-attention-mode
    (condition-case err
        (pcase (map-elt event :event)
          ((or 'permission-request 'elicitation-request)
           (with-current-buffer shell
             (setq agent-hq-attention--since (or agent-hq-attention--since (float-time))))
           (agent-hq-attention--notify-blocked shell)
           (agent-hq-attention--schedule))
          ('turn-complete
           (run-at-time 0 nil #'agent-hq-attention--notify-done
                        shell (map-elt (map-elt event :data) :stop-reason))
           (agent-hq-attention--schedule))
          ((or 'permission-response 'elicitation-response 'input-submitted 'error
               'clean-up 'init-finished 'session-restored)
           (agent-hq-attention--schedule)))
      (error (message "agent-hq-attention: %s" (error-message-string err))))))

(defvar-local agent-hq-attention--tracked nil
  "Non-nil once this shell's events reach agent-hq-attention.")

(defun agent-hq-attention--track (shell)
  "Follow SHELL's events, unless already following them."
  (when (and (buffer-live-p shell)
             (not (buffer-local-value 'agent-hq-attention--tracked shell)))
    (with-current-buffer shell
      (setq agent-hq-attention--tracked t)
      (agent-shell-subscribe-to
       :shell-buffer shell
       :on-event (lambda (event) (agent-hq-attention--on-event shell event))))))

(defun agent-hq-attention-track-h ()
  "Follow this shell's events.  For `agent-shell-mode-hook'."
  (agent-hq-attention--track (current-buffer))
  (agent-hq-attention--schedule))

;;;; Answering

(defconst agent-hq-attention--kind-keys
  '(("allow_once" . ?y) ("allow_always" . ?!) ("reject_once" . ?n))
  "The key answering with the first permission option of each kind.")

(defun agent-hq-attention--choices (tool-call)
  "Return (CHOICES . ACTIONS) for answering TOOL-CALL's permission request.
CHOICES are for `read-multiple-choice', named as the agent names its
options; ACTIONS map each answering key to agent-shell's permission
action.  The first option of each kind gets y, ! or n, any other a
digit.  Then come v (the diff, else the whole request), g (go to the
shell) and q (later)."
  (let ((digit ?1) used choices actions)
    (dolist (action (map-elt tool-call :permission-actions))
      (let* ((preferred (alist-get (map-elt action :kind) agent-hq-attention--kind-keys
                                   nil nil #'equal))
             (key (if (and preferred (not (memq preferred used)))
                      preferred
                    (prog1 digit (cl-incf digit)))))
        (push key used)
        (push (list key (or (map-elt action :option) (map-elt action :kind))) choices)
        (push (cons key action) actions)))
    (cons (append (nreverse choices)
                  (list (list ?v (if (map-elt tool-call :diffs) "view diff" "whole request"))
                        (list ?g "go to shell")
                        (list ?q "later")))
          (nreverse actions))))

(defun agent-hq-attention--request-text (tool-call)
  "Return all of TOOL-CALL's permission request, as text to read."
  (let* ((raw (agent-hq-attention--raw-input tool-call))
         (plan (map-elt raw 'plan))
         (command (map-elt raw 'command))
         (title (map-elt tool-call :title))
         (body (or (and (stringp plan) plan)
                   (and (fboundp 'agent-shell--permission-title)
                        (ignore-errors (agent-shell--permission-title :tool-call tool-call)))
                   (and (stringp command) command)
                   (and (stringp title) title)
                   ""))
         (description (map-elt tool-call :description)))
    (concat (string-trim (replace-regexp-in-string "^[ \t]*```.*\n?" "" body))
            (when (and (stringp description) (not (string-search description body)))
              (concat "\n\n" (string-trim description))))))

(defun agent-hq-attention--details (shell tool-call)
  "Return SHELL's permission request TOOL-CALL in full, under where it comes from."
  (let ((ctx (with-current-buffer shell (agent-hq-context)))
        (title (agent-hq--session-title shell)))
    (concat (propertize (agent-hq-attention--label shell) 'face 'bold)
            (when-let* ((branch (and ctx (agent-hq-context-branch ctx))))
              (concat "  " (propertize branch 'face 'font-lock-constant-face)))
            (when title (concat "  " (propertize title 'face 'font-lock-doc-face)))
            "\n\n"
            (agent-hq-attention--request-text tool-call)
            "\n")))

(defun agent-hq-attention--prompt-room (label)
  "Return the columns left for what is asked, after LABEL, in the prompt."
  (max 20 (- (frame-width) (string-width label) 50)))

(defun agent-hq-attention--long-p (shell tool-call)
  "Return non-nil when the prompt can't hold all of SHELL's request TOOL-CALL.
That is a plan, a request of several lines, or a line too long."
  (let ((text (agent-hq-attention--request-text tool-call)))
    (or (stringp (map-elt (agent-hq-attention--raw-input tool-call) 'plan))
        (string-search "\n" text)
        (> (string-width (agent-hq-attention--what shell tool-call))
           (agent-hq-attention--prompt-room (agent-hq-attention--label shell))))))

(defun agent-hq-attention--show-details (text)
  "Show TEXT in the request buffer, below, without selecting it.
Return its window."
  (let ((buffer (get-buffer-create agent-hq-attention-details-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (setq-local word-wrap t truncate-lines nil)
      (goto-char (point-min)))
    (display-buffer buffer '((display-buffer-reuse-window display-buffer-in-side-window)
                             (side . bottom) (slot . 1) (inhibit-same-window . t)
                             (window-height . fit-window-to-buffer)))))

(defun agent-hq-attention--close-details ()
  "Close the request buffer and its windows."
  (when-let* ((buffer (get-buffer agent-hq-attention-details-buffer-name)))
    (dolist (window (get-buffer-window-list buffer nil t))
      (ignore-errors (quit-restore-window window 'kill)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun agent-hq-attention--view-diff (shell id tool-call)
  "Show the diff of SHELL's permission request TOOL-CALL, tool call ID.
Its own keys then accept or reject, as from the shell's View button."
  (let ((state (agent-hq-attention--state shell)))
    (with-current-buffer shell
      (funcall (agent-shell--make-diff-viewing-function
                :diffs (map-elt tool-call :diffs)
                :actions (map-elt tool-call :permission-actions)
                :client (map-elt state :client)
                :request-id (map-elt tool-call :permission-request-id)
                :state state
                :tool-call-id id)))))

(defun agent-hq-attention--more ()
  "Return how many shells wait on you, as a hint to answer them, or \"\"."
  (let ((waiting (length (agent-hq-attention--blocked-shells))))
    (if (> waiting 0)
        (format "; %d more %s on you (%s)" waiting (if (= waiting 1) "waits" "wait")
                (substitute-command-keys "\\[agent-hq-answer]"))
      "")))

(defun agent-hq-attention--send (shell id request action)
  "Answer SHELL's permission REQUEST, of tool call ID, with ACTION.
Nothing goes when REQUEST was answered or withdrawn meanwhile.  A
rejection also stops the turn, as rejecting in the shell does.  Return
non-nil once sent."
  (let ((state (agent-hq-attention--state shell))
        (label (agent-hq-attention--label shell)))
    (if (not (and (buffer-live-p shell)
                  (equal (map-nested-elt state (list :tool-calls id :permission-request-id))
                         request)))
        (progn (message "%s: that request was already answered" label) nil)
      (with-current-buffer shell
        (agent-shell--send-permission-response
         :client (map-elt state :client)
         :request-id request
         :option-id (map-elt action :option-id)
         :state state
         :tool-call-id id)
        (when (equal (map-elt action :kind) "reject_once")
          (agent-shell-interrupt t)))
      (message "%s: %s%s" label (map-elt action :option) (agent-hq-attention--more))
      t)))

(defvar agent-hq-attention--asking nil
  "Non-nil while `agent-hq-answer' asks what to answer.")

(defun agent-hq-attention--ask (shell)
  "Ask what to answer SHELL's oldest permission request, and send it."
  (pcase-let* ((`(,id . ,tool-call) (agent-hq-attention--permission shell))
               (request (map-elt tool-call :permission-request-id))
               (label (agent-hq-attention--label shell))
               (what (agent-hq-attention--what shell tool-call))
               (`(,choices . ,actions) (agent-hq-attention--choices tool-call))
               (prompt (concat (propertize label 'face 'bold) " "
                               (truncate-string-to-width
                                what (agent-hq-attention--prompt-room label) nil nil "…")
                               " "))
               (details (and (agent-hq-attention--long-p shell tool-call)
                             (agent-hq-attention--details shell tool-call)))
               (shown nil)
               (done nil))
    (let ((agent-hq-attention--asking t)
          ;; A click on the badge would otherwise ask in a dialog box.
          (use-dialog-box nil))
      (unwind-protect
          (progn
            (when details
              (setq shown (agent-hq-attention--show-details details)))
            (while (not done)
              (let ((key (car (read-multiple-choice prompt choices))))
                (pcase key
                  (?v (if (map-elt tool-call :diffs)
                          (progn (setq done t)
                                 (agent-hq-attention--close-details)
                                 (agent-hq-attention--view-diff shell id tool-call))
                        (if (window-live-p shown)
                            (progn (agent-hq-attention--close-details) (setq shown nil))
                          (setq shown (agent-hq-attention--show-details
                                       (agent-hq-attention--details shell tool-call))))))
                  (?g (setq done t)
                      (agent-hq-attention--close-details)
                      (agent-hq-attention--visit shell))
                  (?q (setq done t))
                  (_ (setq done t)
                     (agent-hq-attention--send shell id request (alist-get key actions)))))))
        (agent-hq-attention--close-details)
        (agent-hq-attention--schedule)))))

(defun agent-hq-attention--unseen-hint ()
  "Return a hint at the unseen answers, for when nothing waits on you."
  (let ((unseen (length (agent-hq-attention--of-kind 'unseen (agent-hq-attention--entries)))))
    (if (> unseen 0)
        (format "; %d finished, not looked at (%s)" unseen
                (substitute-command-keys "\\[agent-hq-attention-jump]"))
      "")))

(defun agent-hq-attention--shell-to-answer (pick)
  "Return the shell `agent-hq-answer' answers when called interactively.
This shell when it waits on you, else the one waiting longest; with
PICK, one you pick of those waiting."
  (let ((blocked (agent-hq-attention--blocked-shells))
        (here (agent-hq-attention--shell-of (current-buffer))))
    (cond ((null blocked)
           (user-error "No agent shell waits on you%s" (agent-hq-attention--unseen-hint)))
          (pick (agent-hq--pick-shell "Answer: " blocked
                                      (lambda (_ ctx) (if ctx (agent-hq-context-repo ctx) "other"))))
          ((memq here blocked) here)
          (t (car blocked)))))

;;;###autoload
(defun agent-hq-answer (shell)
  "Answer the permission request SHELL waits on, from wherever you are.
Interactively SHELL is this one when it waits on you, else the one
waiting longest; with a prefix argument, one you pick of those waiting.

The prompt names the shell and what it asks; a plan, or a request too
long for the prompt, shows in full in a window below.  Keys: y allow,
! always allow, n reject (which also stops the turn, as rejecting in
the shell does), digits for any other option, v the diff (whose own
keys then answer) or the whole request, g go to the shell, q later.
The answer is sent just as the shell's own buttons send it.

A shell waiting on a question form is visited, to fill the form in."
  (interactive (list (agent-hq-attention--shell-to-answer current-prefix-arg)))
  (cond (agent-hq-attention--asking (user-error "Already asking about a request"))
        ((agent-hq-attention--permission shell) (agent-hq-attention--ask shell))
        ((agent-hq-attention--form-p shell)
         (agent-hq-attention--visit shell)
         (message "%s asks a question: answer it in the form"
                  (agent-hq-attention--label shell)))
        (t (user-error "%s doesn't wait on you" (agent-hq-attention--label shell)))))

;;;###autoload
(defun agent-hq-attention-jump (&optional pick)
  "Go to the agent shell needing you most, in its workspace.
That is the one waiting on you longest, else the one whose answer has
gone unseen longest; the shell you're in comes last, so repeating this
goes round them.  With PICK (\\[universal-argument]), pick one of them;
with two, show agent-shell-attention's own dashboard."
  (interactive "P")
  (if (and (equal pick '(16)) (fboundp 'agent-shell-attention-dashboard))
      (agent-shell-attention-dashboard)
    (let* ((entries (agent-hq-attention--entries))
           (shells (mapcar (lambda (entry) (plist-get entry :shell)) entries))
           (blocked (agent-hq-attention--of-kind 'blocked entries))
           (here (agent-hq-attention--shell-of (current-buffer))))
      (unless shells
        (user-error "No agent shell needs you"))
      (agent-hq-attention--visit
       (if pick
           (agent-hq--pick-shell "Needs you: " shells
                                  (lambda (shell _)
                                    (if (memq shell blocked) "Waiting on you" "Finished, not looked at")))
         (or (seq-find (lambda (shell) (not (eq shell here))) shells)
             (car shells)))))))

;;;; Putting the badge in the mode line

(defvar agent-hq-attention--installed nil
  "Doom mode lines given the badge, as (CELL OLD NEW); or `misc-info'.
CELL is a mode line's (LEFT . RIGHT) in `+modeline-format-alist', OLD
its left side before, NEW after.")

(defun agent-hq-attention--with-badge (left)
  "Return mode line construct LEFT with the badge leading it."
  (cond ((and (consp left) (memq 'agent-hq-attention-mode-line left)) left)
        ((and (consp left) (stringp (car left)))
         (cons (car left) (cons 'agent-hq-attention-mode-line (cdr left))))
        (t (list "" 'agent-hq-attention-mode-line left))))

(defun agent-hq-attention--swap-left (old new)
  "Make every buffer whose mode line's left side is OLD show NEW."
  (when (eq (default-value '+modeline-format-left) old)
    (setq-default +modeline-format-left new))
  (dolist (buffer (buffer-list))
    (when (and (local-variable-p '+modeline-format-left buffer)
               (eq (buffer-local-value '+modeline-format-left buffer) old))
      (with-current-buffer buffer
        (setq +modeline-format-left new)))))

(defun agent-hq-attention--doom-install (&rest _)
  "Lead the left side of each of Doom's mode lines with the badge.
Buffers keep their mode line, now with the badge.  Also after
`def-modeline!', for mode lines defined later."
  (when (boundp '+modeline-format-alist)
    (dolist (entry +modeline-format-alist)
      (let* ((cell (cdr entry))
             (old (car cell))
             (new (agent-hq-attention--with-badge old)))
        (unless (eq old new)
          (setcar cell new)
          (push (list cell old new) agent-hq-attention--installed)
          (agent-hq-attention--swap-left old new))))))

(defun agent-hq-attention--install ()
  "Put the badge in the mode line: Doom's left side, else `mode-line-misc-info'."
  (if (boundp '+modeline-format-alist)
      (progn (agent-hq-attention--doom-install)
             (advice-add 'def-modeline! :after #'agent-hq-attention--doom-install))
    (unless (memq 'agent-hq-attention-mode-line (default-value 'mode-line-misc-info))
      (setq-default mode-line-misc-info
                    (cons 'agent-hq-attention-mode-line (default-value 'mode-line-misc-info))))
    (setq agent-hq-attention--installed 'misc-info))
  (force-mode-line-update t))

(defun agent-hq-attention--uninstall ()
  "Take the badge out of the mode line again."
  (advice-remove 'def-modeline! #'agent-hq-attention--doom-install)
  (if (eq agent-hq-attention--installed 'misc-info)
      (setq-default mode-line-misc-info
                    (delq 'agent-hq-attention-mode-line (default-value 'mode-line-misc-info)))
    (pcase-dolist (`(,cell ,old ,new) agent-hq-attention--installed)
      (when (eq (car cell) new)
        (setcar cell old))
      (agent-hq-attention--swap-left new old)))
  (setq agent-hq-attention--installed nil)
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode agent-hq-attention-mode
  "Show the agent shells waiting on you, notify about them, answer them anywhere.
The selected window's mode line starts with a badge: red for the shells
waiting on a permission or question of yours, the longest waiting named,
and a yellow count of turns that ended while you were elsewhere and
that you haven't looked at.  The badge leaves out the shell its window
shows and blinks when a shell starts waiting while you look elsewhere.

macOS notifications (`agent-hq-notifier') say when a shell starts
waiting on you, with a sound, and when a turn ends while Emacs isn't
focused; see `agent-hq-attention-notify-blocked' and
`agent-hq-attention-notify-done'.  Clicking one comes back to answer
it; once dealt with, it goes away.

\\[agent-hq-answer] answers a permission request from anywhere, and
\\[agent-hq-attention-jump] goes to the shell needing you most."
  :global t
  :group 'agent-hq-attention
  (if agent-hq-attention-mode
      (progn
        (add-hook 'agent-shell-mode-hook #'agent-hq-attention-track-h)
        ;; Looking at a shell counts its answer as seen.
        (add-hook 'window-buffer-change-functions #'agent-hq-attention--schedule)
        (add-hook 'window-selection-change-functions #'agent-hq-attention--schedule)
        (mapc #'agent-hq-attention--track (agent-hq--shells))
        ;; Shells already waiting don't blink the badge.
        (setq agent-hq-attention--current (agent-hq-attention--entries))
        (agent-hq-attention--install)
        (unless (timerp agent-hq-attention--tick-timer)
          (setq agent-hq-attention--tick-timer
                (run-at-time agent-hq-attention-tick agent-hq-attention-tick
                             #'agent-hq-attention--update)))
        (agent-hq-attention--update))
    (remove-hook 'agent-shell-mode-hook #'agent-hq-attention-track-h)
    (remove-hook 'window-buffer-change-functions #'agent-hq-attention--schedule)
    (remove-hook 'window-selection-change-functions #'agent-hq-attention--schedule)
    (dolist (timer (list agent-hq-attention--tick-timer agent-hq-attention--update-timer
                         agent-hq-attention--blink-timer))
      (when (timerp timer)
        (cancel-timer timer)))
    (setq agent-hq-attention--tick-timer nil
          agent-hq-attention--update-timer nil
          agent-hq-attention--blink-timer nil
          agent-hq-attention--blink-on nil
          agent-hq-attention--current nil
          agent-hq-attention--drawn nil
          agent-hq-attention--memo nil)
    (agent-hq-attention--uninstall)))

(provide 'agent-hq-attention)
;;; agent-hq-attention.el ends here
