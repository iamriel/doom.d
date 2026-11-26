;;; agent-hq-restore-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-restore-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: the functions used here are stand-ins
;; shaped like agent-shell's, and `agent-shell--start' only makes a buffer.

(require 'ert)
(require 'cl-lib)
(require 'map)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-restore.el" dir) nil t))

(defvar agent-shell--state nil)
(defvar server-name)
(defvar persp-mode)
(defvar *persp-hash*)
(defvar persp-save-dir)
(defvar persp-before-save-state-to-file-functions)
(define-derived-mode agent-shell-mode fundamental-mode "Agent")
(define-derived-mode agent-shell-viewport-view-mode special-mode "Viewport")
(provide 'agent-shell)

(defvar agent-hq-restore-test--shells nil "Live stand-in shells.")
(defvar agent-hq-restore-test--started nil "Arguments of each `agent-shell--start', last first.")
(defvar agent-hq-restore-test--subscriptions nil "(BUFFER EVENT . FUNCTION), last first.")
(defvar agent-hq-restore-test--status 'idle)
(defvar agent-hq-restore-test--config nil "The test's Claude config dir, which records run on.")

(defun agent-shell-buffers () agent-hq-restore-test--shells)
(cl-defun agent-shell-initialized-p (&key shell-buffer) (ignore shell-buffer) t)
(cl-defun agent-shell-status (&key shell-buffer) (ignore shell-buffer) agent-hq-restore-test--status)
(defun agent-shell--resolve-config-designator (designator)
  (list (cons :identifier designator) (cons :buffer-name "Claude")))
(cl-defun agent-shell-subscribe-to (&key shell-buffer event on-event)
  (push (cons shell-buffer (cons event on-event)) agent-hq-restore-test--subscriptions))
(defun shell-maker-set-buffer-name (buffer name)
  (with-current-buffer buffer (rename-buffer name t)))
(cl-defun agent-shell-unsubscribe (&key subscription) (ignore subscription))

(defvar agent-hq-restore-test--handled nil "Shells `agent-shell--handle' started, last first.")
(cl-defun agent-shell--handle (&key shell-buffer command)
  (ignore command)
  (push shell-buffer agent-hq-restore-test--handled))

(defvar-local agent-hq-restore-test--input nil "Where a stand-in shell's prompt input starts.")
(defun agent-shell--prompt-input-start ()
  (and agent-hq-restore-test--input (marker-position agent-hq-restore-test--input)))
(defun agent-shell--prompt-input ()
  (when-let* ((start (agent-shell--prompt-input-start))
              (text (string-trim (buffer-substring start (point-max))))
              ((not (string-empty-p text))))
    text))

(defun agent-hq-restore-test--make-shell (name &rest state)
  "Return a live stand-in shell named NAME whose `agent-shell--state' is STATE.
STATE is a plist of :agent, :session-id, :resumed, :title and :cwd."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (agent-shell-mode)
      (setq default-directory (file-name-as-directory (or (plist-get state :cwd) temporary-file-directory)))
      (setq-local agent-shell--state
                  (list (cons :agent-config (list (cons :identifier (or (plist-get state :agent)
                                                                        'claude-code))))
                        (cons :session (list (cons :id (plist-get state :session-id))
                                             (cons :title (plist-get state :title))))
                        (cons :resume-session-id (plist-get state :resumed))))
      (insert "Claude> ")
      (setq agent-hq-restore-test--input (point-marker))
      (setq agent-hq--shell-number (cl-incf agent-hq--shells-started)))
    (setq agent-hq-restore-test--shells (append agent-hq-restore-test--shells (list buffer)))
    buffer))

(cl-defun agent-shell--start (&key config session-id cwd no-focus session-strategy
                                   &allow-other-keys)
  (push (list :agent (map-elt config :identifier) :session-id session-id :cwd cwd
              :no-focus no-focus :session-strategy session-strategy
              :override agent-hq--claude-config-override)
        agent-hq-restore-test--started)
  (let ((buffer (agent-hq-restore-test--make-shell "Claude @ restored"
                                                  :agent (map-elt config :identifier)
                                                  :resumed session-id :cwd cwd)))
    ;; As agent-shell does: the prompt is up, then the agent starts.
    (agent-shell--handle :shell-buffer buffer)
    buffer))

(defmacro agent-hq-restore-test--with-env (&rest body)
  "Run BODY with a fresh list file, no shells and clean restore state."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "agent-hq-restore-test" t)))
          (agent-hq-restore-file (expand-file-name "shells.eld" dir))
          (server-name "server")
          (agent-hq-restore--owner nil)
          (agent-hq-restore--other-pid nil)
          (agent-hq-restore--pending nil)
          (agent-hq-restore--declined nil)
          (agent-hq-restore--written nil)
          (agent-hq-restore--timer nil)
          (agent-hq-restore--frozen nil)
          (agent-hq-restore--wake-timer nil)
          (agent-hq-restore--workspaces-written nil)
          (agent-hq-restore--workspaces-claimed nil)
          (agent-hq-restore--workspaces-idle-timer nil)
          (agent-hq-restore--crashed nil)
          (agent-hq-restore-lazy t)
          (window-buffer-change-functions nil)
          (agent-hq-restore-test--handled nil)
          (agent-hq-restore-test--shells nil)
          (agent-hq-restore-test--started nil)
          (agent-hq-restore-test--subscriptions nil)
          (agent-hq-restore-test--status 'idle)
          (agent-hq-restore-test--config (file-name-as-directory (expand-file-name "claude" dir))))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-hq-visit-shell) #'ignore))
           ,@body)
       (dolist (timer (list agent-hq-restore--timer agent-hq-restore--wake-timer
                            agent-hq-restore--workspaces-idle-timer))
         (when (timerp timer) (cancel-timer timer)))
       (mapc #'kill-buffer (seq-filter #'buffer-live-p agent-hq-restore-test--shells))
       (delete-directory dir t))))

(defun agent-hq-restore-test--record (id &rest props)
  "Return a record for session ID in `temporary-file-directory', with PROPS.
Claude Code has stored the session, unless PROPS say :resume nil or
:no-file t."
  (let ((record (append props
                        (list :agent 'claude-code :session-id id :resume t
                              :cwd (file-name-as-directory temporary-file-directory)
                              :account agent-hq-restore-test--config
                              :name (format "Claude @ tmp · %s" id)))))
    (when (and (plist-get record :resume) (not (plist-get record :no-file)))
      (agent-hq-restore-test--store record))
    record))

(defun agent-hq-restore-test--store (record)
  "Make the file Claude Code keeps RECORD's session in."
  (when-let* ((file (agent-hq-restore--claude-session-file record)))
    (make-directory (file-name-directory file) t)
    (write-region "" nil file nil 'silent)))

(defun agent-hq-restore-test--place ()
  "Return the place label of a shell in `temporary-file-directory'."
  (agent-hq-restore--place-label (list :cwd temporary-file-directory)))

(defun agent-hq-restore-test--file-text ()
  (with-temp-buffer
    (insert-file-contents agent-hq-restore-file)
    (buffer-string)))

;;;; Records

(ert-deftest agent-hq-restore-test-record ()
  (agent-hq-restore-test--with-env
    (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "s1"
                                                    :title "Fix the thing\nmore")))
      (with-current-buffer shell
        (setq agent-hq--claude-started-with "/home/me/.claude-work/"))
      (let ((record (agent-hq-restore--record shell)))
        (should (eq (plist-get record :agent) 'claude-code))
        (should (equal (plist-get record :session-id) "s1"))
        (should (equal (plist-get record :title) "Fix the thing"))
        (should (equal (plist-get record :account) "/home/me/.claude-work/"))
        (should (equal (plist-get record :name) "Claude @ tmp"))
        ;; Never prompted, never resumed: the session may not exist yet.
        (should-not (plist-get record :resume))
        (should-not (plist-get record :busy)))
      (with-current-buffer shell (setq agent-hq-restore--prompted t))
      (should (plist-get (agent-hq-restore--record shell) :resume))
      (let ((agent-hq-restore-test--status 'busy))
        (should (eq (plist-get (agent-hq-restore--record shell) :busy) t)))
      ;; Waiting on a permission answer is told apart.
      (let ((agent-hq-restore-test--status 'blocked))
        (should (eq (plist-get (agent-hq-restore--record shell) :busy) 'blocked))))))

(ert-deftest agent-hq-restore-test-record-resumed-shell ()
  (agent-hq-restore-test--with-env
    ;; Still loading: no session id in the state yet, only the one asked for.
    (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :resumed "old")))
      (let ((record (agent-hq-restore--record shell)))
        (should (equal (plist-get record :session-id) "old"))
        (should (plist-get record :resume))))))

(ert-deftest agent-hq-restore-test-record-prompted-before-tracking ()
  (agent-hq-restore-test--with-env
    ;; Started before the restore module was on: the usage shows a prompt.
    (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "s1")))
      (with-current-buffer shell
        (push (cons :usage (list (cons :total-tokens 1200))) agent-shell--state))
      (should (plist-get (agent-hq-restore--record shell) :resume)))))

(ert-deftest agent-hq-restore-test-resumable-from-claude-files ()
  (agent-hq-restore-test--with-env
    ;; Claude Code's own file decides, whatever the shell saw.
    (let ((stored (agent-hq-restore-test--record "stored" :resume nil)))
      (agent-hq-restore-test--store stored)
      (should (agent-hq-restore--resumable-p stored)))
    (let ((lost (agent-hq-restore-test--record "lost" :no-file t)))
      (should-not (agent-hq-restore--resumable-p lost))
      (setq agent-hq-restore--owner t)
      (agent-hq-restore--restore (list lost))
      ;; Started anew rather than failing to load.
      (should-not (plist-get (car agent-hq-restore-test--started) :session-id)))
    ;; Other agents go by what the shell saw.
    (should (agent-hq-restore--resumable-p
             (agent-hq-restore-test--record "c" :agent 'codex :account nil)))
    (should-not (agent-hq-restore--resumable-p
                 (agent-hq-restore-test--record "c" :agent 'codex :account nil :resume nil)))))

(ert-deftest agent-hq-restore-test-record-without-session ()
  (agent-hq-restore-test--with-env
    (should-not (agent-hq-restore--record (agent-hq-restore-test--make-shell "Claude @ tmp")))))

;;;; The list on disk

(ert-deftest agent-hq-restore-test-file-per-server ()
  (let ((agent-hq-restore-file "/tmp/x/agent-hq-shells.eld"))
    (let ((server-name "server"))
      (should (equal (agent-hq-restore--file) "/tmp/x/agent-hq-shells.eld")))
    (let ((server-name "restore test"))
      (should (equal (agent-hq-restore--file) "/tmp/x/agent-hq-shells.restore_test.eld")))))

(ert-deftest agent-hq-restore-test-write-read-round-trip ()
  (agent-hq-restore-test--with-env
    (let ((records (list (agent-hq-restore-test--record "a" :title "Ünïcode · title")
                         (agent-hq-restore-test--record "b" :workspaces '("webapp")))))
      (agent-hq-restore--write records)
      (let ((data (agent-hq-restore--read)))
        (should (eql (plist-get data :pid) (emacs-pid)))
        (should (equal (plist-get data :shells) records)))
      ;; No temporary file is left beside it.
      (should-not (directory-files dir nil "\\`\\.shells\\.eld-")))))

(ert-deftest agent-hq-restore-test-write-skips-unchanged ()
  (agent-hq-restore-test--with-env
    (let ((records (list (agent-hq-restore-test--record "a"))))
      (agent-hq-restore--write records)
      (delete-file agent-hq-restore-file)
      (agent-hq-restore--write records)
      (should-not (file-exists-p agent-hq-restore-file))
      (agent-hq-restore--write nil)
      (should (file-exists-p agent-hq-restore-file)))))

(ert-deftest agent-hq-restore-test-read-bad-file ()
  (agent-hq-restore-test--with-env
    (with-temp-file agent-hq-restore-file (insert "(:version 1 :pid"))
    (should-not (agent-hq-restore--read))
    (with-temp-file agent-hq-restore-file (insert "(:version 2 :pid 1 :shells nil)"))
    (should-not (agent-hq-restore--read))
    ;; Records missing what a restore needs are dropped, not the whole list.
    (with-temp-file agent-hq-restore-file
      (prin1 (list :version 1 :pid 1 :shells (list (list :agent 'codex) (agent-hq-restore-test--record "ok")))
             (current-buffer)))
    (should (equal (mapcar (lambda (r) (plist-get r :session-id))
                           (plist-get (agent-hq-restore--read) :shells))
                   '("ok")))))

(ert-deftest agent-hq-restore-test-load-from-dead-emacs ()
  (agent-hq-restore-test--with-env
    (with-temp-file agent-hq-restore-file
      (prin1 (list :version 1 :pid 999999 :shells (list (agent-hq-restore-test--record "a")))
             (current-buffer)))
    (agent-hq-restore--load)
    (should (eq agent-hq-restore--owner t))
    (should (equal (mapcar (lambda (r) (plist-get r :session-id)) agent-hq-restore--pending)
                   '("a")))))

(ert-deftest agent-hq-restore-test-load-from-running-emacs ()
  (agent-hq-restore-test--with-env
    (let ((other (start-process "other-emacs" nil (expand-file-name invocation-name invocation-directory)
                                "--batch" "-Q" "--eval" "(sleep-for 30)")))
      (unwind-protect
          (progn
            (with-temp-file agent-hq-restore-file
              (prin1 (list :version 1 :pid (process-id other)
                           :shells (list (agent-hq-restore-test--record "a")))
                     (current-buffer)))
            (agent-hq-restore--load)
            (should (eq agent-hq-restore--owner 'other))
            (should-not agent-hq-restore--pending)
            ;; Its list stays as it wrote it.
            (let ((before (agent-hq-restore-test--file-text)))
              (agent-hq-restore-save)
              (should (equal (agent-hq-restore-test--file-text) before)))
            (should-error (agent-hq-restore-shells) :type 'user-error))
        (delete-process other)))))

(ert-deftest agent-hq-restore-test-keeps-pending-until-restored ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t
          agent-hq-restore--pending (list (agent-hq-restore-test--record "old")
                                          (agent-hq-restore-test--record "again")))
    (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "again"
                                                    :resumed "again")))
      (ignore shell)
      (agent-hq-restore-save)
      (should (equal (mapcar (lambda (r) (plist-get r :session-id))
                             (plist-get (agent-hq-restore--read) :shells))
                     '("again" "old"))))))

(ert-deftest agent-hq-restore-test-kill-emacs-freezes ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "s1")))
      (agent-hq-restore-kill-emacs-h)
      (should (equal (mapcar (lambda (r) (plist-get r :session-id))
                             (plist-get (agent-hq-restore--read) :shells))
                     '("s1")))
      ;; Whatever is killed while Emacs quits stays in the list.
      (setq agent-hq-restore-test--shells nil)
      (kill-buffer shell)
      (agent-hq-restore-save)
      (should (equal (length (plist-get (agent-hq-restore--read) :shells)) 1)))))

(ert-deftest agent-hq-restore-test-tracking ()
  (agent-hq-restore-test--with-env
    (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "s1"))
          (agent-hq-restore-save-delay 60))
      (with-current-buffer shell (agent-hq-restore-track-h))
      (should (equal (sort (delete-dups (mapcar #'cadr agent-hq-restore-test--subscriptions))
                           #'string<)
                     '(clean-up init-finished input-submitted permission-request
                       permission-response session-restored session-title-changed
                       turn-complete)))
      (dolist (subscription agent-hq-restore-test--subscriptions)
        (when (eq (cadr subscription) 'input-submitted)
          (funcall (cddr subscription) '((:event . input-submitted)))))
      (should (buffer-local-value 'agent-hq-restore--prompted shell))
      ;; One write scheduled for the lot.
      (should (timerp agent-hq-restore--timer)))))

;;;; Restoring

(ert-deftest agent-hq-restore-test-restore-starts-and-forgets ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let* ((fresh (agent-hq-restore-test--record "fresh" :resume nil :name "Claude @ tmp · new"))
           (work-config (file-name-as-directory (expand-file-name "work" dir)))
           (work (agent-hq-restore-test--record "work" :account work-config
                                                :shown 200 :name "Claude (work) @ tmp · w"))
           (seen nil))
      (setq agent-hq-restore--pending (list fresh work))
      (cl-letf (((symbol-function 'agent-hq-visit-shell) (lambda (shell) (setq seen shell))))
        (agent-hq-restore--restore (list fresh work)))
      (let ((starts (reverse agent-hq-restore-test--started)))
        ;; A session that never had a prompt starts afresh in its directory.
        (should-not (plist-get (nth 0 starts) :session-id))
        (should (equal (plist-get (nth 1 starts) :session-id) "work"))
        ;; On the account it ran on, which names the shell too.
        (should (equal (plist-get (nth 1 starts) :override) work-config))
        (should (plist-get (nth 1 starts) :no-focus)))
      (should (equal (mapcar #'buffer-name agent-hq-restore-test--shells)
                     '("Claude @ tmp · new" "Claude (work) @ tmp · w")))
      ;; The one looked at last is shown.
      (should (equal (buffer-name seen) "Claude (work) @ tmp · w"))
      (should-not agent-hq-restore--pending))))

(ert-deftest agent-hq-restore-test-restore-adopts-open-session ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let ((open (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "s1")))
      (should (eq (agent-hq-restore--start (agent-hq-restore-test--record "s1")) open))
      (should-not agent-hq-restore-test--started)
      (should-not (agent-hq-restore--restorable (list (agent-hq-restore-test--record "s1")))))))

(ert-deftest agent-hq-restore-test-gone-directory ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let ((gone (agent-hq-restore-test--record "gone" :cwd "/no/such/dir/"))
          (here (agent-hq-restore-test--record "here")))
      (setq agent-hq-restore--pending (list gone here))
      (should (equal (agent-hq-restore--gone (list gone here)) (list here)))
      (should (equal agent-hq-restore--pending (list here))))))

(ert-deftest agent-hq-restore-test-offer-declined ()
  (agent-hq-restore-test--with-env
    (let ((record (agent-hq-restore-test--record "a")))
      (with-temp-file agent-hq-restore-file
        (prin1 (list :version 1 :pid 999999 :shells (list record)) (current-buffer)))
      (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?n "no"))))
        (agent-hq-restore--offer t))
      (should-not agent-hq-restore-test--started)
      (should (equal agent-hq-restore--declined (list record)))
      ;; Out of the list for the next Emacs...
      (should-not (plist-get (agent-hq-restore--read) :shells))
      ;; ...but this one still restores it on request.
      (agent-hq-restore-shells)
      (should (equal (plist-get (car agent-hq-restore-test--started) :session-id) "a"))
      (should-not agent-hq-restore--declined))))

(ert-deftest agent-hq-restore-test-offer-quit-keeps-pending ()
  (agent-hq-restore-test--with-env
    (with-temp-file agent-hq-restore-file
      (prin1 (list :version 1 :pid 999999 :shells (list (agent-hq-restore-test--record "a")))
             (current-buffer)))
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) (signal 'quit nil))))
      (agent-hq-restore--offer t))
    (should-not agent-hq-restore-test--started)
    (should (= (length agent-hq-restore--pending) 1))))

(ert-deftest agent-hq-restore-test-offer-pick ()
  (agent-hq-restore-test--with-env
    (with-temp-file agent-hq-restore-file
      (prin1 (list :version 1 :pid 999999
                   :shells (list (agent-hq-restore-test--record "a")
                                 (agent-hq-restore-test--record "b")))
             (current-buffer)))
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?p "pick")))
              ((symbol-function 'map-y-or-n-p)
               (lambda (_prompter actor list &rest _)
                 ;; Yes to b only.
                 (funcall actor (cadr list)))))
      (agent-hq-restore--offer t))
    (should (equal (mapcar (lambda (s) (plist-get s :session-id)) agent-hq-restore-test--started)
                   '("b")))
    (should (equal (mapcar (lambda (r) (plist-get r :session-id)) agent-hq-restore--declined)
                   '("a")))))

(ert-deftest agent-hq-restore-test-offer-without-asking ()
  (agent-hq-restore-test--with-env
    (with-temp-file agent-hq-restore-file
      (prin1 (list :version 1 :pid 999999 :shells (list (agent-hq-restore-test--record "a")))
             (current-buffer)))
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) (error "Asked"))))
      (agent-hq-restore--offer nil))
    (should (= (length agent-hq-restore-test--started) 1))))

;;;; Workspaces

(ert-deftest agent-hq-restore-test-place-creates-missing-workspace ()
  (agent-hq-restore-test--with-env
    (let ((persp-mode t)
          (*persp-hash* (make-hash-table :test #'equal))
          created added params)
      (cl-letf (((symbol-function 'persp-get-by-name)
                 (lambda (name &optional hash default) (gethash name hash default)))
                ;; As in persp-mode: nil, its "none" perspective, passes too.
                ((symbol-function 'persp-p)
                 (lambda (object) (or (null object) (eq (car-safe object) 'persp))))
                ((symbol-function '+workspace-new)
                 (lambda (name)
                   (push name created)
                   (puthash name (list 'persp name) *persp-hash*)))
                ((symbol-function 'set-persp-parameter)
                 (lambda (key value persp) (push (list key value (cadr persp)) params)))
                ((symbol-function 'persp-add-buffer)
                 (lambda (buffer persp &rest _) (push (cons (buffer-name buffer) (cadr persp)) added))))
        (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "s1"))
              (record (agent-hq-restore-test--record "s1" :workspaces '("webapp"))))
          (agent-hq-restore--place shell record)
          (should (equal created '("webapp")))
          (should (equal added '(("Claude @ tmp" . "webapp"))))
          ;; Tagged with its project, so SPC p p finds it.
          (should (equal (car params)
                         (list '+workspace-project
                               (file-truename (plist-get record :cwd)) "webapp")))
          ;; One that exists is joined, not made again.
          (agent-hq-restore--place shell record)
          (should (equal created '("webapp")))
          (should (= (length added) 2)))))))

(ert-deftest agent-hq-restore-test-persp-save ()
  (agent-hq-restore-test--with-env
    (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "s1"))
          (starting (agent-hq-restore-test--make-shell "Claude @ starting"))
          (viewport (generate-new-buffer "Claude @ tmp [viewport]"))
          (stderr (generate-new-buffer "acp-client-stderr(claude-agent-acp)-3"))
          (file (generate-new-buffer "init.el")))
      (unwind-protect
          (progn
            (with-current-buffer viewport (agent-shell-viewport-view-mode))
            (let ((saved (agent-hq-restore-persp-save shell)))
              (should (eq (car saved) 'def-agent-hq-shell))
              (should (equal (nth 1 saved) "Claude @ tmp"))
              (should (equal (plist-get (nth 2 saved) :session-id) "s1")))
            ;; No session yet: nothing to come back to.
            (should (eq (agent-hq-restore-persp-save starting) 'skip))
            (should (eq (agent-hq-restore-persp-save viewport) 'skip))
            (should (eq (agent-hq-restore-persp-save stderr) 'skip))
            (should-not (agent-hq-restore-persp-save file)))
        (mapc #'kill-buffer (list viewport stderr file))))))

(ert-deftest agent-hq-restore-test-persp-load ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let ((buffer (agent-hq-restore-persp-load
                   (list 'def-agent-hq-shell "Claude @ tmp · saved" (agent-hq-restore-test--record "s1")))))
      (should (equal (buffer-name buffer) "Claude @ tmp · saved"))
      (should (equal (plist-get (car agent-hq-restore-test--started) :session-id) "s1"))
      ;; Loading it again finds the shell instead of starting another.
      (should (eq (agent-hq-restore-persp-load
                   (list 'def-agent-hq-shell "Claude @ tmp · saved" (agent-hq-restore-test--record "s1")))
                  buffer))
      (should (= (length agent-hq-restore-test--started) 1)))
    ;; A directory that's gone gives no buffer, and persp-mode goes on.
    (should (eq (agent-hq-restore-persp-load
                 (list 'def-agent-hq-shell "x" (agent-hq-restore-test--record "s2" :cwd "/no/such/dir/")))
                'skip))
    ;; Shells saved as plain buffers would come back as dead ones.
    (should (eq (agent-hq-restore-persp-load '(def-buffer "Claude @ doom.d" nil agent-shell-mode)) 'skip))
    (should (eq (agent-hq-restore-persp-load
                 '(def-buffer "acp-client-stderr(claude-agent-acp)-9" nil fundamental-mode))
                'skip))
    (should-not (agent-hq-restore-persp-load '(def-buffer "init.el" "/tmp/init.el" emacs-lisp-mode)))))

(ert-deftest agent-hq-restore-test-persp-load-prefers-list ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    ;; The workspace was saved a while ago; the list has the draft since.
    (let ((newer (agent-hq-restore-test--record "s1" :draft "typed since")))
      (setq agent-hq-restore--pending (list newer))
      (let ((shell (agent-hq-restore-persp-load
                    (list 'def-agent-hq-shell "Claude @ tmp · saved"
                          (agent-hq-restore-test--record "s1")))))
        (should (equal (with-current-buffer shell (agent-shell--prompt-input)) "typed since"))
        ;; Back, so no longer pending.
        (should-not agent-hq-restore--pending)))))

;;;; Drafts and queues

(ert-deftest agent-hq-restore-test-record-draft-and-queue ()
  (agent-hq-restore-test--with-env
    (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "s1")))
      (should-not (plist-get (agent-hq-restore--record shell) :draft))
      (with-current-buffer shell
        (insert (propertize "half a thought\n" 'face 'bold))
        (push (cons :pending-prompts (list "first" (propertize "second" 'face 'bold)))
              agent-shell--state))
      (let ((record (agent-hq-restore--record shell)))
        (should (equal (plist-get record :draft) "half a thought"))
        (should-not (text-properties-at 0 (plist-get record :draft)))
        (should (equal (plist-get record :queued) '("first" "second")))))))

(ert-deftest agent-hq-restore-test-restore-puts-back-draft-and-queue ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let* ((record (agent-hq-restore-test--record "s1" :draft "line one\nline two"
                                                  :queued '("then this" "and that")))
           (shell (agent-hq-restore--start record)))
      (with-current-buffer shell
        (should (equal (agent-shell--prompt-input) "line one\nline two"))
        (should (eobp))
        ;; Held, not sent: nothing started a turn.
        (should (equal (map-elt agent-shell--state :pending-prompts) '("then this" "and that"))))
      ;; And it is what the list keeps for the next Emacs.
      (let ((saved (agent-hq-restore--record shell)))
        (should (equal (plist-get saved :draft) "line one\nline two"))
        (should (equal (plist-get saved :queued) '("then this" "and that")))))))

(ert-deftest agent-hq-restore-test-draft-goes-before-text-typed-since ()
  (agent-hq-restore-test--with-env
    (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "s1")))
      (with-current-buffer shell
        (insert "typed")
        (agent-hq-restore--insert-draft "saved")
        (should (equal (agent-shell--prompt-input) "saved\n\ntyped"))))))

(ert-deftest agent-hq-restore-test-typing-schedules-a-save ()
  (agent-hq-restore-test--with-env
    (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "s1"))
          (agent-hq-restore-save-delay 60))
      (with-current-buffer shell
        (agent-hq-restore-track-h)
        (should (memq #'agent-hq-restore--schedule post-command-hook))
        (run-hooks 'post-command-hook))
      (should (timerp agent-hq-restore--timer)))))

;;;; Shells mid-turn

(ert-deftest agent-hq-restore-test-mid-turn-gets-continue-prompt ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let* ((agent-hq-restore-continue-prompt "Carry on.")
           (working (agent-hq-restore--start (agent-hq-restore-test--record "w" :busy t)))
           (waiting (agent-hq-restore--start (agent-hq-restore-test--record "b" :busy 'blocked)))
           (drafted (agent-hq-restore--start (agent-hq-restore-test--record "d" :busy t
                                                                           :draft "my own")))
           ;; Never stored: a new session, with nothing to carry on.
           (lost (agent-hq-restore--start (agent-hq-restore-test--record "l" :busy t :no-file t)))
           (idle (agent-hq-restore--start (agent-hq-restore-test--record "i"))))
      (should (equal (with-current-buffer working (agent-shell--prompt-input)) "Carry on."))
      (should (buffer-local-value 'agent-hq-restore--prefilled working))
      (should (equal (with-current-buffer waiting (agent-shell--prompt-input)) "Carry on."))
      (should (equal (with-current-buffer drafted (agent-shell--prompt-input)) "my own"))
      (should-not (buffer-local-value 'agent-hq-restore--prefilled drafted))
      (should-not (with-current-buffer lost (agent-shell--prompt-input)))
      (should-not (with-current-buffer idle (agent-shell--prompt-input)))
      ;; Not sent: no turn started.
      (should-not agent-hq-restore-test--handled)))
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let* ((agent-hq-restore-continue-prompt nil)
           (shell (agent-hq-restore--start (agent-hq-restore-test--record "w" :busy t))))
      (should-not (with-current-buffer shell (agent-shell--prompt-input))))))

(ert-deftest agent-hq-restore-test-restore-names-mid-turn ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let ((records (list (agent-hq-restore-test--record "w" :busy t :name "Claude @ tmp · w")
                         (agent-hq-restore-test--record "b" :busy 'blocked :name "Claude @ tmp · b")
                         (agent-hq-restore-test--record "i" :name "Claude @ tmp · i")))
          message)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (setq message (apply #'format-message format args)))))
        (agent-hq-restore--restore records))
      (should (string-match-p
               "; mid-turn when Emacs quit, each with a prompt to carry on: \
Claude @ tmp · w, Claude @ tmp · b (was waiting on you)\\'"
               message))
      (should (equal (agent-hq-restore--places records)
                     (format "%s ×3; 2 mid-turn" (agent-hq-restore-test--place))))
      (should (equal (mapcar #'agent-hq-restore--state-note records)
                     '(" (was working)" " (was waiting on you)" ""))))))

(ert-deftest agent-hq-restore-test-quit-asks-when-mid-turn ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let ((shell (agent-hq-restore-test--make-shell "Claude @ tmp" :session-id "s1"))
          (agent-hq-restore-confirm-quit t)
          (use-short-answers t)
          (noninteractive nil)
          asked answer)
      (ignore shell)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) answer)))
        ;; Nothing mid-turn: no question.
        (should (agent-hq-restore-kill-emacs-query-h))
        (should-not asked)
        (let ((agent-hq-restore-test--status 'busy))
          ;; No: Emacs stays.
          (should-not (agent-hq-restore-kill-emacs-query-h))
          (should (equal (car asked)
                         (format "1 agent shell is working (%s).  Quitting stops its turn; \
the shell comes back on restart.  Quit anyway? " (agent-hq-restore-test--place))))
          ;; Yes: and Doom's question isn't asked as well.
          (setq answer t)
          (should (agent-hq-restore-kill-emacs-query-h))
          (should (agent-hq-restore--confirm-kill-a (lambda (&rest _) (error "Asked twice"))
                                                    "Really exit Emacs? "))
          ;; Only that once.
          (should (eq (agent-hq-restore--confirm-kill-a #'ignore "Really exit Emacs? ") nil)))
        (let ((agent-hq-restore-confirm-quit nil)
              (agent-hq-restore-test--status 'busy))
          (setq asked nil)
          (should (agent-hq-restore-kill-emacs-query-h))
          (should-not asked))))))

(ert-deftest agent-hq-restore-test-quit-question ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let ((place (agent-hq-restore-test--place))
          (a (agent-hq-restore-test--make-shell "a"))
          (b (agent-hq-restore-test--make-shell "b"))
          (c (agent-hq-restore-test--make-shell "c")))
      (should (equal (agent-hq-restore--quit-question (list (cons a 'busy) (cons b 'busy)
                                                            (cons c 'blocked)))
                     (format "2 agent shells are working (%s ×2) and 1 waits on you (%s).  \
Quitting stops their turns; the shells come back on restart.  Quit anyway? " place place)))
      (should (equal (agent-hq-restore--quit-question (list (cons a 'blocked) (cons b 'blocked)))
                     (format "2 agent shells wait on you (%s ×2).  \
Quitting stops their turns; the shells come back on restart.  Quit anyway? " place)))
      ;; Another Emacs keeps the list: nothing comes back.
      (setq agent-hq-restore--owner 'other)
      (should (equal (agent-hq-restore--quit-question (list (cons a 'busy)))
                     (format "1 agent shell is working (%s).  Quitting stops its turn.  Quit anyway? "
                             place))))))

(ert-deftest agent-hq-restore-test-queued-prompts-kept-at-quit ()
  (agent-hq-restore-test--with-env
    (let ((agent-hq-restore-confirm-quit t))
      ;; agent-shell's own question is moot while the list keeps queues.
      (setq agent-hq-restore--owner t)
      (should (agent-hq-restore--queues-kept-p))
      (setq agent-hq-restore--owner 'other)
      (should-not (agent-hq-restore--queues-kept-p)))))

;;;; Sleeping shells

(ert-deftest agent-hq-restore-test-lazy-start-sleeps ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let ((shell (agent-hq-restore--start (agent-hq-restore-test--record "s1"))))
      ;; The shell is there, its agent isn't.
      (should (agent-hq-restore--asleep-p shell))
      (should-not agent-hq-restore-test--handled)
      (should (eq (agent-hq--status shell) 'asleep))
      (should (memq #'agent-hq-restore--shown-h window-buffer-change-functions))
      ;; Found again by its session, before the agent has one.
      (should (eq (agent-hq-restore--live-shell (agent-hq-restore-test--record "s1")) shell))
      (agent-hq-restore-wake shell)
      (should-not (agent-hq-restore--asleep-p shell))
      (should (equal agent-hq-restore-test--handled (list shell)))
      ;; Nothing left asleep: no more watching windows.
      (should-not (memq #'agent-hq-restore--shown-h window-buffer-change-functions))
      ;; Waking twice starts nothing more.
      (agent-hq-restore-wake shell)
      (should (= (length agent-hq-restore-test--handled) 1)))))

(ert-deftest agent-hq-restore-test-eager-start ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let* ((agent-hq-restore-lazy nil)
           (shell (agent-hq-restore--start (agent-hq-restore-test--record "s1"))))
      (should-not (agent-hq-restore--asleep-p shell))
      (should (equal agent-hq-restore-test--handled (list shell))))))

(ert-deftest agent-hq-restore-test-sleeping-record-keeps-origin ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let* ((work (file-name-as-directory (expand-file-name "work" dir)))
           ;; Never prompted: the new shell has no session id at all.
           (record (agent-hq-restore-test--record "fresh" :account work :resume nil
                                                  :title "Old title" :busy t))
           (shell (agent-hq-restore--start record))
           (saved (agent-hq-restore--record shell)))
      (should-not (plist-get (car agent-hq-restore-test--started) :session-id))
      (should (equal (plist-get saved :session-id) "fresh"))
      (should (equal (plist-get saved :account) work))
      (should (equal (plist-get saved :title) "Old title"))
      (should (plist-get saved :busy))
      (should-not (plist-get saved :resume)))))

(ert-deftest agent-hq-restore-test-shown-shell-wakes ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let ((shown (agent-hq-restore--start (agent-hq-restore-test--record "a")))
          (hidden (agent-hq-restore--start (agent-hq-restore-test--record "b")))
          (window (selected-window))
          (before (window-buffer (selected-window))))
      (unwind-protect
          (progn
            (set-window-buffer window shown)
            (agent-hq-restore--shown-h (selected-frame))
            (should (timerp agent-hq-restore--wake-timer))
            (agent-hq-restore--wake-shown)
            (should-not (agent-hq-restore--asleep-p shown))
            (should (agent-hq-restore--asleep-p hidden))
            ;; Still one asleep, so windows are still watched.
            (should (memq #'agent-hq-restore--shown-h window-buffer-change-functions)))
        (set-window-buffer window before)))))

(ert-deftest agent-hq-restore-test-restore-wakes-the-one-shown ()
  (agent-hq-restore-test--with-env
    (setq agent-hq-restore--owner t)
    (let ((records (list (agent-hq-restore-test--record "a" :shown 100)
                         (agent-hq-restore-test--record "b" :shown 300)
                         (agent-hq-restore-test--record "c" :shown 200)))
          seen message)
      (cl-letf (((symbol-function 'agent-hq-visit-shell) (lambda (shell) (setq seen shell)))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq message (apply #'format-message format args)))))
        (agent-hq-restore--restore records))
      (should (equal agent-hq-restore-test--handled (list seen)))
      (should (equal (buffer-name seen) (plist-get (nth 1 records) :name)))
      (should (string-match-p "; 2 start once shown" message)))))

;;;; Workspaces after a crash

(defmacro agent-hq-restore-test--with-persp (&rest body)
  "Run BODY with persp-mode stand-ins saving to a temporary `persp-save-dir'."
  (declare (indent 0))
  `(let ((persp-mode t)
         (*persp-hash* (make-hash-table :test #'equal))
         (persp-save-dir (file-name-as-directory (expand-file-name "workspaces" dir)))
         (persp-before-save-state-to-file-functions nil)
         (saved-states 0)
         (savelist '((def-persp "main" ((def-buffer "init.el" "/tmp/init.el" emacs-lisp-mode))
                                (def-wconf nil)
                                (def-params ((persp-file . "/tmp/x") (+workspace-project . "/tmp/")))
                                nil nil nil))))
     (make-directory persp-save-dir t)
     (cl-letf (((symbol-function 'persp-persps) (lambda (&rest _) '(main)))
               ((symbol-function 'persp-save-state) (lambda (&rest _) (cl-incf saved-states)))
               ((symbol-function 'persps-to-savelist) (lambda (&rest _) savelist))
               ((symbol-function 'persp-list-persp-names-in-file)
                (lambda (file &rest _)
                  (with-temp-buffer
                    (insert-file-contents file)
                    (mapcar #'cadr (read (current-buffer)))))))
       ,@body)))

(ert-deftest agent-hq-restore-test-workspaces-save ()
  (agent-hq-restore-test--with-env
    (agent-hq-restore-test--with-persp
      (setq agent-hq-restore--owner t)
      (agent-hq-restore-save-workspaces)
      (let ((file (agent-hq-restore--workspaces-file)))
        (should (equal (file-name-nondirectory file) "autosave-live"))
        (should (= saved-states 1))
        (let ((saved (with-temp-buffer (insert-file-contents file) (read (current-buffer)))))
          ;; As persp-mode saves it: no file of its own for each workspace.
          (should (equal (cadr (assq 'def-params (car saved)))
                         '((+workspace-project . "/tmp/")))))
        ;; Nothing changed: nothing written.
        (delete-file file)
        (agent-hq-restore-save-workspaces)
        (should-not (file-exists-p file))
        (setq savelist (list (list 'def-persp "other" nil nil nil nil nil nil)))
        (agent-hq-restore-save-workspaces)
        (should (file-exists-p file))))))

(ert-deftest agent-hq-restore-test-workspaces-save-quietly ()
  ;; persp-mode reports each parameter it can't save, in full, every save.
  (agent-hq-restore-test--with-env
    (agent-hq-restore-test--with-persp
      (setq agent-hq-restore--owner t)
      (cl-letf (((symbol-function 'persps-to-savelist)
                 (lambda (&rest _)
                   (message "[persp-mode] Info: The parameter (winner-ring) can't be saved.")
                   savelist)))
        (let ((logged (lambda () (with-current-buffer (messages-buffer) (buffer-size)))))
          (let ((before (funcall logged)))
            (agent-hq-restore-save-workspaces)
            (should (file-exists-p (agent-hq-restore--workspaces-file)))
            (should (= (funcall logged) before))))))))

(ert-deftest agent-hq-restore-test-workspaces-not-another-emacs-s ()
  (agent-hq-restore-test--with-env
    (agent-hq-restore-test--with-persp
      (setq agent-hq-restore--owner 'other
            agent-hq-restore--other-pid 1)
      (cl-letf (((symbol-function 'agent-hq-restore--load) #'ignore))
        (agent-hq-restore-save-workspaces))
      (should-not (file-exists-p (agent-hq-restore--workspaces-file))))))

(ert-deftest agent-hq-restore-test-workspaces-clean-quit-deletes ()
  (agent-hq-restore-test--with-env
    (agent-hq-restore-test--with-persp
      (setq agent-hq-restore--owner t)
      (agent-hq-restore-save-workspaces)
      (should (file-exists-p (agent-hq-restore--workspaces-file)))
      (agent-hq-restore-kill-emacs-h)
      (should-not (file-exists-p (agent-hq-restore--workspaces-file))))))

(ert-deftest agent-hq-restore-test-workspaces-crash-moved-aside ()
  (agent-hq-restore-test--with-env
    (agent-hq-restore-test--with-persp
      ;; What the Emacs that crashed left.
      (with-temp-file (agent-hq-restore--workspaces-file) (prin1 savelist (current-buffer)))
      (agent-hq-restore--claim)
      (let ((crashed (agent-hq-restore--workspaces-file t)))
        (should (equal agent-hq-restore--crashed crashed))
        (should (file-exists-p crashed))
        (should-not (file-exists-p (agent-hq-restore--workspaces-file)))
        ;; This Emacs's own saves don't touch it.
        (agent-hq-restore-save-workspaces)
        (should (file-exists-p (agent-hq-restore--workspaces-file)))
        (should (equal (with-temp-buffer (insert-file-contents crashed) (read (current-buffer)))
                       savelist))
        ;; The list is this Emacs's now, for one started later.
        (should (eql (plist-get (agent-hq-restore--read) :pid) (emacs-pid)))))))

(ert-deftest agent-hq-restore-test-offer-after-crash ()
  (agent-hq-restore-test--with-env
    (agent-hq-restore-test--with-persp
      (let ((in-workspace (agent-hq-restore-test--record "a" :workspaces '("main")))
            (elsewhere (agent-hq-restore-test--record "b"))
            loaded seen asked)
        (with-temp-file agent-hq-restore-file
          (prin1 (list :version 1 :pid 999999 :shells (list in-workspace elsewhere))
                 (current-buffer)))
        (with-temp-file (agent-hq-restore--workspaces-file) (prin1 savelist (current-buffer)))
        (agent-hq-restore--claim)
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (prompt choices &rest _)
                     (setq asked (cons prompt (mapcar #'car choices)))
                     '(?y "yes")))
                  ((symbol-function 'doom-load-session)
                   (lambda (file)
                     (setq loaded file)
                     ;; Loading the workspace brings back the shell it held.
                     (agent-hq-restore-persp-load
                      (list 'def-agent-hq-shell "Claude @ tmp · a" in-workspace))))
                  ((symbol-function 'agent-hq-visit-shell) (lambda (shell) (setq seen shell))))
          (agent-hq-restore--offer t))
        (should (string-match-p "\\`Emacs quit unexpectedly.  Restore its workspace (main) and 2 agent shells"
                                (car asked)))
        (should (equal (cdr asked) '(?y ?s ?p ?n)))
        (should (equal loaded (agent-hq-restore--workspaces-file t)))
        ;; One shell each: the workspace's adopted, the other started.
        (should (= (length agent-hq-restore-test--started) 2))
        (should-not agent-hq-restore--pending)
        ;; The windows stay as the workspaces had them.
        (should-not seen)
        (should-not agent-hq-restore--crashed)))))

(ert-deftest agent-hq-restore-test-offer-after-crash-shells-only ()
  (agent-hq-restore-test--with-env
    (agent-hq-restore-test--with-persp
      (with-temp-file agent-hq-restore-file
        (prin1 (list :version 1 :pid 999999 :shells (list (agent-hq-restore-test--record "a")))
               (current-buffer)))
      (with-temp-file (agent-hq-restore--workspaces-file) (prin1 savelist (current-buffer)))
      (agent-hq-restore--claim)
      (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?s "shells")))
                ((symbol-function 'doom-load-session) (lambda (_) (error "Loaded"))))
        (agent-hq-restore--offer t))
      (should (= (length agent-hq-restore-test--started) 1))
      ;; Still there for SPC q L.
      (should (file-exists-p (agent-hq-restore--workspaces-file t))))))

;;; agent-hq-restore-test.el ends here
