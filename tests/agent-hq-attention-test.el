;;; agent-hq-attention-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-attention-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: shells are stand-in buffers with their
;; status stubbed, and agent-shell's calls are stubbed where answered.

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-attention.el" dir) nil t))

(defvar agent-shell--state nil)
(defvar agent-shell-attention--pending (make-hash-table :test #'eq))
(defvar server-name)

(unless (fboundp 'agent-shell-mode)
  (define-derived-mode agent-shell-mode fundamental-mode "Agent shell"
    "Stands in for agent-shell's mode."))

(define-derived-mode agent-hq-attention-test-shell-mode agent-shell-mode "Test shell"
  "Stands in for a shell's mode.")

;;;; Stand-in shells

(defvar agent-hq-attention-test--status nil
  "Alist of (SHELL . STATUS) for the stubbed `agent-hq--status'.")

(defvar agent-hq-attention-test--unread nil
  "Shells the stubbed `agent-shell-attention-unread-p' counts unread.")

(defun agent-hq-attention-test--shell (base dir title status &rest state)
  "Return a stand-in shell in BASE/DIR with session TITLE and STATUS.
STATE is more of its `agent-shell--state', as alternating keys and values."
  (let ((shell (generate-new-buffer (format "Claude @ %s" dir)))
        (root (file-name-as-directory (expand-file-name dir base))))
    (make-directory root t)
    (with-current-buffer shell
      (agent-hq-attention-test-shell-mode)
      (setq default-directory root)
      (setq-local agent-shell--state
                  (append (list (cons :session (list (cons :title title))))
                          (cl-loop for (key value) on state by #'cddr
                                   collect (cons key value)))))
    (push (cons shell status) agent-hq-attention-test--status)
    shell))

(defun agent-hq-attention-test--permission (id request &rest tool-call)
  "Return a tool call entry ID waiting on permission REQUEST.
TOOL-CALL is more of it, as alternating keys and values."
  (cons id (append (list (cons :permission-request-id request)
                         (cons :permission-actions
                               (list '((:option . "Allow") (:kind . "allow_once") (:option-id . "allow"))
                                     '((:option . "Always Allow") (:kind . "allow_always")
                                       (:option-id . "always"))
                                     '((:option . "Reject") (:kind . "reject_once")
                                       (:option-id . "reject")))))
                   (cl-loop for (key value) on tool-call by #'cddr
                            collect (cons key value)))))

(defmacro agent-hq-attention-test--with-shells (&rest body)
  "Run BODY with `base', a temp dir for shells, and agent-hq's view stubbed.
Shells made with `agent-hq-attention-test--shell' are the live ones;
the mode is on but draws and posts nowhere."
  (declare (indent 0))
  `(let* ((base (file-name-as-directory (file-truename (make-temp-file "agent-hq-attention" t))))
          (agent-hq-attention-test--status nil)
          (agent-hq-attention-test--unread nil)
          (agent-shell-attention--pending (make-hash-table :test #'eq))
          (agent-hq-attention-mode t)
          (agent-hq-attention--current nil)
          (agent-hq-attention--drawn nil)
          (agent-hq-attention--memo nil)
          (agent-hq-attention--posted nil)
          (agent-hq-attention--blink-on nil)
          (agent-hq-attention--blink-timer nil)
          (agent-hq-attention--update-timer nil)
          (agent-hq-attention-blinks 0)
          (agent-hq-notifier nil))
     (cl-letf (((symbol-function 'agent-hq--shells)
                (lambda () (seq-filter #'buffer-live-p
                                       (reverse (mapcar #'car agent-hq-attention-test--status)))))
               ((symbol-function 'agent-hq--status)
                (lambda (shell) (alist-get shell agent-hq-attention-test--status)))
               ((symbol-function 'agent-shell-attention-unread-p)
                (lambda (shell) (memq shell agent-hq-attention-test--unread))))
       (unwind-protect
           (progn ,@body)
         (dolist (timer (list agent-hq-attention--update-timer agent-hq-attention--blink-timer))
           (when (timerp timer) (cancel-timer timer)))
         (agent-hq-attention--close-details)
         (dolist (entry agent-hq-attention-test--status)
           (when (buffer-live-p (car entry))
             (kill-buffer (car entry))))
         (delete-directory base t)))))

;;;; What a request asks

(ert-deftest agent-hq-attention-test-what ()
  (with-temp-buffer
    (let ((shell (current-buffer)))
      ;; A command shows as such, its first line only.
      (should (equal (agent-hq-attention--what
                      shell '((:kind . "execute") (:title . "`rm -rf build`")
                              (:raw-input (command . "rm -rf build"))))
                     "$ rm -rf build"))
      (should (equal (agent-hq-attention--what
                      shell '((:kind . "execute") (:raw-input (command . "npm run build\nnpm test"))))
                     "$ npm run build …"))
      (should (equal (agent-hq-attention--what shell '((:kind . "execute") (:title . "`ls -la`")))
                     "$ ls -la"))
      ;; Anything else by its title; a plan as such.
      (should (equal (agent-hq-attention--what shell '((:kind . "edit") (:title . "Edit `src/a.ts`")))
                     "Edit src/a.ts"))
      (should (equal (agent-hq-attention--what
                      shell '((:kind . "switch_mode") (:title . "Ready to code?")
                              (:raw-input (plan . "1. Do it\n2. Test it"))))
                     "Plan ready for review"))
      (should (equal (agent-hq-attention--what shell '((:kind . "other"))) "A tool call")))))

(ert-deftest agent-hq-attention-test-request-text ()
  ;; Code fences go; a description not already shown follows.
  (cl-letf (((symbol-function 'agent-shell--permission-title)
             (lambda (&rest _) "```console\nrm -rf build\n```")))
    (should (equal (agent-hq-attention--request-text
                    '((:title . "Bash") (:description . "Clean the build")))
                   "rm -rf build\n\nClean the build")))
  (should (equal (agent-hq-attention--request-text
                  '((:raw-input (plan . "1. Do it\n2. Test it"))))
                 "1. Do it\n2. Test it")))

(ert-deftest agent-hq-attention-test-choices ()
  (pcase-let* ((tool-call (cdr (agent-hq-attention-test--permission "t1" 7)))
               (`(,choices . ,actions) (agent-hq-attention--choices tool-call)))
    ;; The agent's names, under fixed keys by kind; then the extras.
    (should (equal choices '((?y "Allow") (?! "Always Allow") (?n "Reject")
                             (?v "whole request") (?g "go to shell") (?q "later"))))
    (should (equal (map-elt (alist-get ?! actions) :option-id) "always")))
  ;; A second option of a kind gets a digit; with diffs, v is the diff.
  (pcase-let ((`(,choices . ,actions)
               (agent-hq-attention--choices
                '((:diffs . (((:file . "a.ts"))))
                  (:permission-actions . (((:option . "Yes") (:kind . "allow_once") (:option-id . "a"))
                                          ((:option . "Yes, auto") (:kind . "allow_once")
                                           (:option-id . "b"))))))))
    (should (equal (mapcar #'car choices) '(?y ?1 ?v ?g ?q)))
    (should (equal (nth 2 choices) '(?v "view diff")))
    (should (equal (map-elt (alist-get ?1 actions) :option-id) "b"))))

;;;; Who needs you, and the badge

(ert-deftest agent-hq-attention-test-entries ()
  (agent-hq-attention-test--with-shells
    (let* ((idle (agent-hq-attention-test--shell base "idle" "Idle" 'ready))
           (busy (agent-hq-attention-test--shell base "busy" "Busy" 'busy))
           (done (agent-hq-attention-test--shell base "done" "Done" 'ready))
           (late (agent-hq-attention-test--shell
                  base "late" "Late" 'blocked
                  :tool-calls (list (agent-hq-attention-test--permission
                                     "t1" 1 :kind "execute" :raw-input '((command . "make"))))))
           (early (agent-hq-attention-test--shell base "early" "Early" 'blocked)))
      (with-current-buffer late (setq agent-hq-attention--since 200.0))
      (with-current-buffer early (setq agent-hq-attention--since 100.0))
      (push done agent-hq-attention-test--unread)
      (puthash done (cons "Finished" 150.0) agent-shell-attention--pending)
      (let ((entries (agent-hq-attention--entries 300.0)))
        ;; Waiting on you first, longest waiting first; then the unseen.
        (should (equal (mapcar (lambda (entry) (plist-get entry :shell)) entries)
                       (list early late done)))
        (should (equal (mapcar (lambda (entry) (plist-get entry :kind)) entries)
                       '(blocked blocked unseen)))
        (should (equal (plist-get (nth 1 entries) :what) "$ make"))
        (should (equal (plist-get (nth 1 entries) :label) "late"))
        (should (equal (plist-get (car entries) :what) "A question waits for your answer"))
        (should (equal (plist-get (nth 2 entries) :what) "Done"))
        (should (equal (plist-get (nth 2 entries) :since) 150.0)))
      (should-not (memq idle (mapcar (lambda (entry) (plist-get entry :shell))
                                     (agent-hq-attention--entries))))
      (should-not (memq busy (mapcar (lambda (entry) (plist-get entry :shell))
                                     (agent-hq-attention--entries))))
      ;; A shell no longer waiting forgets since when; waiting anew, it
      ;; starts from now.
      (setf (alist-get early agent-hq-attention-test--status) 'busy)
      (agent-hq-attention--entries 400.0)
      (should-not (buffer-local-value 'agent-hq-attention--since early))
      (setf (alist-get early agent-hq-attention-test--status) 'blocked)
      (agent-hq-attention--entries 500.0)
      (should (equal (buffer-local-value 'agent-hq-attention--since early) 500.0)))))

(ert-deftest agent-hq-attention-test-badge ()
  (should (equal (agent-hq-attention--badge nil) ""))
  (let* ((one (list :shell 'a :kind 'blocked :since 100.0 :label "webapp:wt1" :what "$ make"))
         (two (list :shell 'b :kind 'blocked :since 250.0 :label "doom.d" :what "Edit a.el"))
         (seen (list :shell 'c :kind 'unseen :since 200.0 :label "100%" :what "Done"))
         (badge (agent-hq-attention--badge (list one two seen) 300.0)))
    ;; The longest waiting, for how long, how many more; then the unseen.
    (should (equal (substring-no-properties badge) " ⚠ webapp:wt1 3m +1  ●1  "))
    (should (eq (get-text-property 1 'face badge) 'agent-hq-attention-blocked))
    (should (eq (get-text-property (string-search "●" badge) 'face badge)
                'agent-hq-attention-unseen))
    (should (string-match-p "doom.d  Edit a.el" (get-text-property 0 'help-echo badge)))
    ;; Under a minute shows no age; a % stays a %.
    (should (equal (substring-no-properties
                    (agent-hq-attention--badge
                     (list (list :shell 'c :kind 'blocked :since 290.0 :label "100%" :what "x"))
                     300.0))
                   " ⚠ 100%%  "))
    (should (equal (substring-no-properties (agent-hq-attention--badge (list seen) 300.0)) " ●1  "))
    (let ((agent-hq-attention--blink-on t))
      (should (eq (get-text-property 1 'face (agent-hq-attention--badge (list one) 300.0))
                  'agent-hq-attention-blink)))))

(ert-deftest agent-hq-attention-test-mode-line-leaves-out-its-shell ()
  (agent-hq-attention-test--with-shells
    (let* ((a (agent-hq-attention-test--shell base "a" "A" 'blocked))
           (b (agent-hq-attention-test--shell base "b" "B" 'blocked))
           (selected t))
      (with-current-buffer a (setq agent-hq-attention--since (- (float-time) 10)))
      (with-current-buffer b (setq agent-hq-attention--since (- (float-time) 5)))
      (cl-letf (((symbol-function 'mode-line-window-selected-p) (lambda () selected)))
        (agent-hq-attention--update)
        (with-temp-buffer
          (should (equal (substring-no-properties (agent-hq-attention-mode-line-string))
                         " ⚠ a +1  ")))
        ;; In a's mode line, only b.
        (with-current-buffer a
          (should (equal (substring-no-properties (agent-hq-attention-mode-line-string))
                         " ⚠ b  "))
          (should (= (agent-hq-attention-mode-line-width) 6)))
        ;; Only the selected window has one.
        (setq selected nil)
        (with-temp-buffer
          (should (equal (agent-hq-attention-mode-line-string) "")))
        (setq selected t)
        ;; Nothing waiting: no badge.
        (setf (alist-get a agent-hq-attention-test--status) 'ready
              (alist-get b agent-hq-attention-test--status) 'ready)
        (agent-hq-attention--update)
        (with-temp-buffer
          (should (equal (agent-hq-attention-mode-line-string) "")))))))

(ert-deftest agent-hq-attention-test-update-blinks-and-withdraws ()
  (agent-hq-attention-test--with-shells
    (let* ((a (agent-hq-attention-test--shell base "a" "A" 'ready))
           (looking nil)
           blinks removed)
      (cl-letf (((symbol-function 'agent-hq-attention--blink)
                 (lambda () (setq blinks (1+ (or blinks 0)))))
                ((symbol-function 'agent-hq-attention--looking-at-p) (lambda (_) looking))
                ((symbol-function 'agent-hq-remove-notification) (lambda (group) (push group removed))))
        (agent-hq-attention--update)
        (should-not blinks)
        ;; Starting to wait while you look elsewhere blinks, once.
        (setf (alist-get a agent-hq-attention-test--status) 'blocked)
        (setq agent-hq-attention--posted (list (cons a (cons 'blocked "g-a"))))
        (agent-hq-attention--update)
        (agent-hq-attention--update)
        (should (equal blinks 1))
        (should-not removed)
        ;; Answered: its notification goes.
        (setf (alist-get a agent-hq-attention-test--status) 'busy)
        (agent-hq-attention--update)
        (should (equal removed '("g-a")))
        (should-not agent-hq-attention--posted)
        ;; Waiting while you look at it: no blink.
        (setq looking t)
        (setf (alist-get a agent-hq-attention-test--status) 'blocked)
        (agent-hq-attention--update)
        (should (equal blinks 1))))))

;;;; Notifications

(ert-deftest agent-hq-attention-test-notify-tiers ()
  (agent-hq-attention-test--with-shells
    (let* ((a (agent-hq-attention-test--shell
               base "a" "Fix it" 'blocked
               :tool-calls (list (agent-hq-attention-test--permission
                                  "t1" 1 :kind "execute" :raw-input '((command . "make"))))))
           (looking nil)
           (focused t)
           posted)
      (with-current-buffer a (setq agent-hq--shell-number 4))
      (cl-letf (((symbol-function 'agent-hq-attention--looking-at-p) (lambda (_) looking))
                ((symbol-function 'agent-hq-attention--focused-p) (lambda () focused))
                ((symbol-function 'agent-hq-post-notification)
                 (lambda (shell title body &rest keys)
                   (push (list shell title body keys) posted)
                   "group")))
        ;; Waiting on you: posted with a sound, unless you're looking.
        (agent-hq-attention--on-event a '((:event . permission-request)))
        (pcase-let ((`(,shell ,title ,body ,keys) (car posted)))
          (should (eq shell a))
          (should (equal title "a needs you"))
          (should (equal body "$ make"))
          (should (equal (plist-get keys :subtitle) "Fix it"))
          (should (equal (plist-get keys :sound) "default"))
          (should (equal (plist-get keys :click)
                         `(agent-hq-attention-clicked ,(agent-hq--shell-id a) ,(buffer-name a)))))
        (should (equal (alist-get a agent-hq-attention--posted) '(blocked . "group")))
        (should (numberp (buffer-local-value 'agent-hq-attention--since a)))
        (setq posted nil looking t)
        (agent-hq-attention--on-event a '((:event . permission-request)))
        (should-not posted)
        ;; A turn ending: only while Emacs isn't focused, silently, with
        ;; the answer when the dashboard has it.
        (setf (alist-get a agent-hq-attention-test--status) 'ready)
        (agent-hq-attention--notify-done a "end_turn")
        (should-not posted)
        (setq focused nil)
        (with-current-buffer a (setq-local agent-hq-dashboard--answer "All tests pass."))
        (agent-hq-attention--notify-done a "end_turn")
        (pcase-let ((`(,_ ,title ,body ,keys) (car posted)))
          (should (equal title "a finished"))
          (should (equal body "All tests pass."))
          (should (equal (plist-get keys :subtitle) "Fix it"))
          (should-not (plist-get keys :sound)))
        (should (eq (car (alist-get a agent-hq-attention--posted)) 'unseen))
        ;; Cancelled is your doing: nothing.  Another stop says why, and
        ;; stays up until you look, as nothing counts it unseen.
        (setq posted nil looking nil)
        (agent-hq-attention--notify-done a "cancelled")
        (should-not posted)
        (agent-hq-attention--notify-done a "max_tokens")
        (should (equal (nth 1 (car posted)) "a stopped (max_tokens)"))
        (should (eq (car (alist-get a agent-hq-attention--posted)) 'stopped))
        (should (agent-hq-attention--still-up-p a 'stopped nil))
        (setq looking t)
        (should-not (agent-hq-attention--still-up-p a 'stopped nil))
        (setq looking nil)
        ;; Already working on a queued prompt: nothing.
        (setq posted nil)
        (setf (alist-get a agent-hq-attention-test--status) 'busy)
        (agent-hq-attention--notify-done a "end_turn")
        (should-not posted)))))

(ert-deftest agent-hq-attention-test-clicked-acts-on-the-shell ()
  (agent-hq-attention-test--with-shells
    (let* ((a (agent-hq-attention-test--shell
               base "a" "A" 'blocked :tool-calls (list (agent-hq-attention-test--permission "t1" 1))))
           (b (agent-hq-attention-test--shell base "b" "B" 'ready))
           answered visited)
      (with-current-buffer a (setq agent-hq--shell-number 1))
      (cl-letf (((symbol-function 'agent-hq-answer) (lambda (shell) (push shell answered)))
                ((symbol-function 'agent-hq-visit-shell) (lambda (shell) (push shell visited))))
        (should (eq (agent-hq-attention--find (agent-hq--shell-id a) "renamed") a))
        (should (eq (agent-hq-attention--find nil (buffer-name b)) b))
        (agent-hq-attention--act-on a "a")
        (agent-hq-attention--act-on b "b")
        (should (equal answered (list a)))
        (should (equal visited (list b)))))))

(ert-deftest agent-hq-attention-test-notifier ()
  (let ((agent-hq--notifier-warned nil)
        warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _) (push message warnings))))
      ;; A failing notifier is reported, once.
      (let ((agent-hq-notifier "false"))
        (dolist (_ '(1 2))
          (let ((process (agent-hq--run-notifier '("-message" "hi"))))
            (while (process-live-p process)
              (accept-process-output process 0.05))
            (accept-process-output nil 0.05))))
      (should (= (length warnings) 1))
      (should (string-match-p "false failed (exit 1)" (car warnings)))
      ;; As is a missing one.
      (setq agent-hq--notifier-warned nil warnings nil)
      (let ((agent-hq-notifier "agent-hq-no-such-notifier"))
        (should-not (agent-hq--run-notifier '("-message" "hi")))
        (should-not (agent-hq--run-notifier '("-message" "hi"))))
      (should (= (length warnings) 1))
      (should (string-match-p "No agent-hq-no-such-notifier" (car warnings)))
      ;; None set: nothing, and no warning.
      (setq warnings nil)
      (let ((agent-hq-notifier nil))
        (should-not (agent-hq--run-notifier '("-message" "hi"))))
      (should-not warnings))))

(ert-deftest agent-hq-attention-test-post-notification-args ()
  (with-temp-buffer
    (setq agent-hq--shell-number 3)
    (let ((shell (current-buffer))
          (server-name "server")
          ran)
      (cl-letf (((symbol-function 'agent-hq--run-notifier) (lambda (args) (push args ran) t))
                ((symbol-function 'agent-hq--emacsclient) (lambda () "/bin/emacsclient")))
        (should (equal (agent-hq-post-notification shell "-x needs you" "[draft] go"
                                                   :subtitle "Title" :sound "default"
                                                   :click '(agent-hq-attention-clicked "1.3" "n"))
                       (agent-hq-notification-group shell)))
        (let ((args (car ran)))
          ;; Leading - and [ shown as is.
          (should (equal (cadr (member "-title" args)) "​-x needs you"))
          (should (equal (cadr (member "-message" args)) "​[draft] go"))
          (should (equal (cadr (member "-subtitle" args)) "Title"))
          (should (equal (cadr (member "-sound" args)) "default"))
          (should (equal (cadr (member "-group" args)) (format "agent-hq-%d.3" (emacs-pid))))
          (should (equal (cadr (member "-execute" args))
                         (concat "/bin/emacsclient -s server -n -e "
                                 (shell-quote-argument "(agent-hq-attention-clicked \"1.3\" \"n\")")))))
        ;; agent-hq-notify posts in a group of its own.
        (agent-hq-notify shell "Claude limit" "")
        (let ((args (car ran)))
          (should (equal (cadr (member "-group" args)) (format "agent-hq-%d.3-note" (emacs-pid))))
          (should (equal (cadr (member "-message" args)) "Needs you"))
          (should-not (member "-sound" args)))))))

;;;; Answering

(ert-deftest agent-hq-attention-test-answer ()
  (agent-hq-attention-test--with-shells
    (let* ((tool-call (agent-hq-attention-test--permission
                       "t1" 9 :kind "execute" :raw-input '((command . "make"))))
           (a (agent-hq-attention-test--shell
               base "a" "A" 'blocked :client 'the-client
               ;; Newest first: t1 is the oldest request.
               :tool-calls (list (agent-hq-attention-test--permission "t2" 10) tool-call)))
           (keys nil)
           sent interrupted visited prompts)
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (prompt choices &rest _)
                   (push (substring-no-properties prompt) prompts)
                   (assq (pop keys) choices)))
                ((symbol-function 'agent-shell--send-permission-response)
                 (lambda (&rest args) (push args sent)))
                ((symbol-function 'agent-shell-interrupt)
                 (lambda (&optional force) (push (list (current-buffer) force) interrupted)))
                ((symbol-function 'agent-hq-visit-shell) (lambda (shell) (push shell visited))))
        ;; y: the oldest request is allowed, as the shell's button would.
        (setq keys '(?y))
        (agent-hq-answer a)
        (should (equal (car prompts) "a $ make "))
        (should (equal sent `((:client the-client :request-id 9 :option-id "allow"
                                       :state ,(buffer-local-value 'agent-shell--state a)
                                       :tool-call-id "t1"))))
        (should-not interrupted)
        ;; n: rejected, and the turn stopped.
        (setq keys '(?n) sent nil)
        (agent-hq-answer a)
        (should (equal (plist-get (car sent) :option-id) "reject"))
        (should (equal interrupted (list (list a t))))
        ;; v shows the whole request, then q leaves it unanswered.
        (setq keys '(?v ?q) sent nil)
        (let (shown)
          (cl-letf (((symbol-function 'agent-hq-attention--show-details)
                     (lambda (text) (push text shown) (selected-window))))
            (agent-hq-answer a))
          (should (string-match-p "make" (car shown))))
        (should-not sent)
        ;; g goes to the shell.
        (setq keys '(?g))
        (agent-hq-answer a)
        (should (equal visited (list a)))
        ;; A request answered meanwhile isn't answered again.
        (setq keys '(?y) sent nil)
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (_prompt choices &rest _)
                     (with-current-buffer a
                       (setf (alist-get :tool-calls agent-shell--state)
                             (list (agent-hq-attention-test--permission "t2" 10)
                                   (cons "t1" '((:kind . "execute"))))))
                     (assq ?y choices))))
          (agent-hq-answer a))
        (should-not sent)
        ;; Nothing waiting: says so.
        (setf (alist-get a agent-hq-attention-test--status) 'ready)
        (with-current-buffer a (setf (alist-get :tool-calls agent-shell--state) nil))
        (should-error (agent-hq-answer a) :type 'user-error)))))

(ert-deftest agent-hq-attention-test-long-request-shows-in-full ()
  (agent-hq-attention-test--with-shells
    (let* ((a (agent-hq-attention-test--shell
               base "a" "A" 'blocked
               :tool-calls (list (agent-hq-attention-test--permission
                                  "t1" 1 :raw-input '((plan . "1. Read\n2. Write"))))))
           (b (agent-hq-attention-test--shell
               base "b" "B" 'blocked
               :tool-calls (list (agent-hq-attention-test--permission
                                  "t1" 1 :kind "execute" :raw-input '((command . "ls")))))))
      (should (agent-hq-attention--long-p a (cdr (agent-hq-attention--permission a))))
      (should-not (agent-hq-attention--long-p b (cdr (agent-hq-attention--permission b))))
      ;; Shown below while asking, gone after.
      (let (during)
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (_prompt choices &rest _)
                     (setq during (with-current-buffer agent-hq-attention-details-buffer-name
                                    (buffer-string)))
                     (assq ?q choices))))
          (agent-hq-answer a))
        (should (string-match-p "2\\. Write" during))
        (should-not (get-buffer agent-hq-attention-details-buffer-name))))))

(ert-deftest agent-hq-attention-test-which-shell ()
  (agent-hq-attention-test--with-shells
    (let ((ready (agent-hq-attention-test--shell base "ready" "R" 'ready))
          (late (agent-hq-attention-test--shell base "late" "L" 'blocked))
          (early (agent-hq-attention-test--shell base "early" "E" 'blocked)))
      (with-current-buffer late (setq agent-hq-attention--since 200.0))
      (with-current-buffer early (setq agent-hq-attention--since 100.0))
      ;; The one waiting longest; from a shell that waits, that one.
      (with-temp-buffer
        (should (eq (agent-hq-attention--shell-to-answer nil) early)))
      (with-current-buffer late
        (should (eq (agent-hq-attention--shell-to-answer nil) late)))
      ;; Jumping: waiting first, then unseen; the one you're in last.
      (push ready agent-hq-attention-test--unread)
      (let (visited)
        (cl-letf (((symbol-function 'agent-hq-visit-shell) (lambda (shell) (push shell visited))))
          (with-temp-buffer (agent-hq-attention-jump))
          (with-current-buffer early (agent-hq-attention-jump))
          (setf (alist-get early agent-hq-attention-test--status) 'busy
                (alist-get late agent-hq-attention-test--status) 'busy)
          (with-temp-buffer (agent-hq-attention-jump)))
        (should (equal (reverse visited) (list early late ready))))
      ;; Nothing waiting: says so, pointing at the unseen.
      (with-temp-buffer
        (should (string-match-p "1 finished"
                                (cadr (should-error (agent-hq-attention--shell-to-answer nil)
                                                    :type 'user-error)))))
      (setq agent-hq-attention-test--unread nil)
      (setf (alist-get ready agent-hq-attention-test--status) 'busy)
      (with-temp-buffer
        (should-error (agent-hq-attention-jump) :type 'user-error)))))

;;;; The badge in Doom's mode line

(defvar +modeline-format-alist)
(defvar-local +modeline-format-left nil)

(ert-deftest agent-hq-attention-test-doom-mode-line ()
  (let* ((main-left (list "" '+modeline-matches " " '+modeline-buffer-identification))
         (shell-left (list "" '(:eval (agent-hq-mode-line-title))))
         (+modeline-format-alist (list (cons :main (cons main-left '("" mode-line-misc-info)))
                                       (cons 'agent-shell (cons shell-left '("")))))
         (agent-hq-attention--installed nil)
         (default (default-value '+modeline-format-left)))
    (setq-default +modeline-format-left main-left)
    (defalias 'def-modeline! #'ignore)
    (unwind-protect
        (with-temp-buffer
          (setq +modeline-format-left shell-left)
          (progn
            (agent-hq-attention--install)
            ;; Every mode line leads with the badge, buffers' ones too.
            (should (equal (car (alist-get :main +modeline-format-alist))
                           (list "" 'agent-hq-attention-mode-line '+modeline-matches " "
                                 '+modeline-buffer-identification)))
            (should (eq (cadr +modeline-format-left) 'agent-hq-attention-mode-line))
            (should (eq (cadr (default-value '+modeline-format-left)) 'agent-hq-attention-mode-line))
            ;; Installing again adds nothing.
            (agent-hq-attention--doom-install)
            (should (= (cl-count 'agent-hq-attention-mode-line (car (alist-get :main +modeline-format-alist)))
                       1))
            ;; And it comes out again.
            (agent-hq-attention--uninstall)
            (should (eq +modeline-format-left shell-left))
            (should (eq (default-value '+modeline-format-left) main-left))
            (should (eq (car (alist-get 'agent-shell +modeline-format-alist)) shell-left))))
      (advice-remove 'def-modeline! #'agent-hq-attention--doom-install)
      (fmakunbound 'def-modeline!)
      (setq-default +modeline-format-left default))))

;;; agent-hq-attention-test.el ends here
