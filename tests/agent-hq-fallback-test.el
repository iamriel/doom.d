;;; agent-hq-fallback-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-fallback-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: shells are plain buffers, and what agent-shell
;; would do is stubbed.

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-fallback.el" dir) nil t))

(defvar agent-shell-anthropic-claude-environment)

(defmacro agent-hq-fallback-test--with-accounts (&rest body)
  "Run BODY with temp accounts `work' and `personal' and a temp `repo' dir.
Shells in `repo' map to `work', which falls back to `personal'.  No limit
is known at the start, and every timer made is cancelled at the end."
  (declare (indent 0))
  `(let* ((base (file-name-as-directory (file-truename (make-temp-file "agent-hq-fallback" t))))
          (work (file-name-as-directory (expand-file-name "claude-work" base)))
          (personal (file-name-as-directory (expand-file-name "claude-personal" base)))
          (repo (file-name-as-directory (expand-file-name "repo" base)))
          (agent-hq-claude-config-dirs (list (cons (concat (regexp-quote repo) "\\'") work)))
          (agent-hq-claude-fallback-dirs (list (cons work personal)))
          (agent-hq-claude-config-function #'agent-hq-fallback--new-shell-config)
          (agent-hq-claude-shared-memory nil)
          (agent-shell-anthropic-claude-environment nil)
          (agent-hq-fallback--limits (make-hash-table :test #'equal))
          (agent-hq-fallback--queue nil)
          (agent-hq-fallback--asking nil)
          (agent-hq-fallback--timer nil))
     (make-directory repo t)
     (unwind-protect
         (progn ,@body)
       (maphash (lambda (_ limit)
                  (when (timerp (plist-get limit :timer))
                    (cancel-timer (plist-get limit :timer))))
                agent-hq-fallback--limits)
       (when (timerp agent-hq-fallback--timer)
         (cancel-timer agent-hq-fallback--timer))
       (delete-directory base t))))

(defmacro agent-hq-fallback-test--with-shells (specs &rest body)
  "Run BODY with stub Claude shells; SPECS is a list of (VAR ACCOUNT STATUS).
Each shell works in `repo', runs on ACCOUNT and has status STATUS."
  (declare (indent 1))
  `(let* (,@(mapcar (lambda (spec)
                      `(,(car spec)
                        (with-current-buffer (generate-new-buffer ,(format " %s" (car spec)))
                          (setq default-directory repo)
                          (setq-local agent-hq--claude-started-with ,(nth 1 spec))
                          (setq-local agent-hq-test-status ,(nth 2 spec))
                          (current-buffer))))
                    specs)
          (all (list ,@(mapcar #'car specs))))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-hq--shells)
                    (lambda () (seq-filter #'buffer-live-p all)))
                   ((symbol-function 'agent-hq--status)
                    (lambda (buffer) (buffer-local-value 'agent-hq-test-status buffer)))
                   ((symbol-function 'agent-hq-notify) #'ignore))
           ,@body)
       (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer))) all))))

(defvar-local agent-hq-test-status nil
  "A stub shell's status.")

(defun agent-hq-fallback-test--rejected (&rest more)
  "Return rejected rate limit info resetting in an hour, plus MORE pairs."
  (append more `((status . "rejected") (resetsAt . ,(+ (truncate (float-time)) 3600))
                 (rateLimitType . "five_hour"))))

;;;; Accounts

(ert-deftest agent-hq-fallback-test-target ()
  (agent-hq-fallback-test--with-accounts
    (should (equal (agent-hq-fallback-target work) personal))
    (should-not (agent-hq-fallback-target personal))
    (should-not (agent-hq-fallback-target nil))
    ;; ~/.claude stands for the default account.
    (let ((agent-hq-claude-fallback-dirs (list (cons work "~/.claude"))))
      (should (eq (agent-hq-fallback-target work) 'default)))))

(ert-deftest agent-hq-fallback-test-config-for-rewraps ()
  ;; Wrapping a config made for one account again moves it to the other:
  ;; the old wrapper's binding must not win.
  (let* (seen
         (maker (lambda (_buffer) (push agent-hq--claude-config-override seen)))
         (config (list (cons :client-maker maker) (cons :buffer-name "Claude")))
         (once (agent-hq--claude-config-for 'default config))
         (twice (agent-hq--claude-config-for "/tmp/work/" once)))
    (funcall (map-elt once :client-maker) nil)
    (funcall (map-elt twice :client-maker) nil)
    (should (equal seen '("/tmp/work/" default)))
    (should (eq (map-elt twice :agent-hq-client-maker) maker))
    (should (= (seq-count (lambda (entry) (eq (car entry) :client-maker)) twice) 1))
    (should (equal (map-elt twice :buffer-name) "Claude"))
    ;; One wrapped the old way, in front of the base maker it kept.
    (setq seen nil)
    (let* ((old (cons (cons :client-maker
                            (lambda (buffer)
                              (let ((agent-hq--claude-config-override "/tmp/work/"))
                                (funcall maker buffer))))
                      config))
           (moved (agent-hq--claude-config-for 'default old)))
      (funcall (map-elt moved :client-maker) nil)
      (should (equal seen '(default)))
      (should (eq (map-elt moved :agent-hq-client-maker) maker)))))

;;;; Limits

(ert-deftest agent-hq-fallback-test-note-limit ()
  (agent-hq-fallback-test--with-accounts
    (let ((limit (agent-hq-fallback--note
                  work (agent-hq-fallback-test--rejected '(isUsingOverage . t)))))
      (should limit)
      (should (plist-get limit :overage))
      (should (equal (plist-get limit :type) "five_hour"))
      (should-not (plist-get limit :guessed))
      (should (timerp (plist-get limit :timer)))
      (should (agent-hq-fallback--limit work)))
    ;; A later answer keeps the reset time it doesn't give, and the answer.
    (agent-hq-fallback--answer work 'stay)
    (let ((resets (plist-get (agent-hq-fallback--limit work) :resets-at))
          (limit (agent-hq-fallback--note work '((status . "rejected")))))
      (should (= (plist-get limit :resets-at) resets))
      (should (eq (plist-get limit :answer) 'stay))
      (should-not (plist-get limit :overage)))
    ;; Allowed again: the limit is over.
    (should-not (agent-hq-fallback--note work '((status . "allowed"))))
    (should-not (agent-hq-fallback--limit work))
    ;; Without a reset time the limit is a guess, and still on.
    (should (plist-get (agent-hq-fallback--note work '((status . "rejected"))) :guessed))
    (should (agent-hq-fallback--limit work))
    ;; Allowed info for an account with no limit records nothing.
    (should-not (agent-hq-fallback--note personal '((status . "allowed"))))
    (should-not (gethash personal agent-hq-fallback--limits))))

(ert-deftest agent-hq-fallback-test-expired-limit ()
  (agent-hq-fallback-test--with-accounts
    (agent-hq-fallback--note work `((status . "rejected")
                                    (resetsAt . ,(- (truncate (float-time)) 10))))
    (should-not (agent-hq-fallback--limit work))
    ;; A new limit after an expired one starts unanswered.
    (agent-hq-fallback--answer work 'all)
    (should-not (plist-get (agent-hq-fallback--note work (agent-hq-fallback-test--rejected))
                           :answer))))

(ert-deftest agent-hq-fallback-test-describe ()
  (let ((agent-hq-claude-default-account-name "personal"))
    (should (equal (agent-hq-fallback--describe "/x/.claude-work/" '(:type "seven_day" :guessed t))
                   "work is at its weekly limit"))
    (should (string-match-p "\\`personal is at its 5-hour limit until [0-9]+:[0-9]\\{2\\}[ap]m\\'"
                            (agent-hq-fallback--describe
                             'default (list :type "five_hour" :resets-at (+ (float-time) 60)))))))

(ert-deftest agent-hq-fallback-test-usage-limit-error ()
  (should (agent-hq-fallback--usage-limit-p
           `((code . -32603) (message . "Internal error")
             (data . ((errorKind . "rate_limit")
                      (rateLimit . ,(agent-hq-fallback-test--rejected)))))))
  ;; A rate limit that isn't the plan's (a 429 to retry) is not one.
  (should-not (agent-hq-fallback--usage-limit-p
               '((code . -32603) (message . "Internal error")
                 (data . ((errorKind . "rate_limit") (rateLimit . ((status . "allowed"))))))))
  ;; Without data, the text tells.
  (should (agent-hq-fallback--usage-limit-p
           '((code . -32603) (message . "Internal error: You've hit your limit · resets 4:40pm"))))
  (should-not (agent-hq-fallback--usage-limit-p
               '((code . -32603) (message . "Internal error: Overloaded") (data . "x")))))

;;;; Noticing

(ert-deftest agent-hq-fallback-test-usage-update-marks-shell ()
  (agent-hq-fallback-test--with-accounts
    (agent-hq-fallback-test--with-shells ((on-work work 'busy) (on-personal personal 'busy))
      (let ((event `((:event . usage-update)
                     (:data . ((:meta . ((_claude/rateLimit . ,(agent-hq-fallback-test--rejected))))))))
            offered)
        (cl-letf (((symbol-function 'agent-hq-fallback--offer)
                   (lambda (buffer failed) (push (list buffer failed) offered))))
          (agent-hq-fallback--on-usage on-work event)
          (agent-hq-fallback--on-usage on-personal event)
          (should (buffer-local-value 'agent-hq-fallback--hit on-work))
          (should (agent-hq-fallback--limit work))
          ;; personal has no fallback, so its limits aren't tracked.
          (should-not (buffer-local-value 'agent-hq-fallback--hit on-personal))
          (should-not (gethash personal agent-hq-fallback--limits))
          ;; The question waits for the end of the turn.
          (should-not offered)
          (agent-hq-fallback--on-turn-complete on-work)
          (agent-hq-fallback--on-turn-complete on-personal)
          (should (equal offered (list (list on-work nil))))
          (should-not (buffer-local-value 'agent-hq-fallback--hit on-work)))))))

(ert-deftest agent-hq-fallback-test-refused-turn-offers ()
  (agent-hq-fallback-test--with-accounts
    (agent-hq-fallback-test--with-shells ((shell work 'ready))
      (let (offered)
        (cl-letf (((symbol-function 'agent-hq-fallback--offer)
                   (lambda (buffer failed) (push (list buffer failed) offered))))
          (with-current-buffer shell (setq agent-hq-fallback--hit t))
          (agent-hq-fallback--on-error shell '((code . -32603) (message . "Internal error: Boom")))
          (should-not offered)
          (agent-hq-fallback--on-error
           shell `((code . -32603) (message . "Internal error")
                   (data . ((errorKind . "rate_limit")
                            (rateLimit . ,(agent-hq-fallback-test--rejected))))))
          (should (equal offered (list (list shell t))))
          (should (agent-hq-fallback--limit work))
          (should-not (buffer-local-value 'agent-hq-fallback--hit shell)))))))

(ert-deftest agent-hq-fallback-test-error-handler-advice ()
  (agent-hq-fallback-test--with-accounts
    (agent-hq-fallback-test--with-shells ((shell work 'ready))
      (let (handled offered)
        (cl-letf (((symbol-function 'agent-hq-fallback--offer)
                   (lambda (buffer failed) (push (list buffer failed) offered))))
          (funcall (agent-hq-fallback-error-handler-a
                    (cl-function (lambda (&key state shell-buffer)
                                   (ignore state shell-buffer)
                                   (lambda (acp-error raw) (push (list acp-error raw) handled))))
                    :state nil :shell-buffer shell)
                   '((code . -32603) (message . "Internal error: You've hit your limit"))
                   "raw")
          (should (equal (cadr (car handled)) "raw"))
          (should (equal offered (list (list shell t)))))))))

;;;; Asking

(ert-deftest agent-hq-fallback-test-answer-all-moves-every-shell ()
  (agent-hq-fallback-test--with-accounts
    (agent-hq-fallback-test--with-shells ((asked work 'ready) (idle work 'ready)
                                          (busy work 'busy) (other personal 'ready))
      (let (moved questions)
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (prompt _choices &rest _) (push prompt questions) '(?a "all")))
                  ((symbol-function 'agent-hq-switch-account)
                   (lambda (buffer target continue)
                     (push (list buffer target continue) moved)
                     (with-current-buffer buffer
                       (setq-local agent-hq--claude-started-with target)))))
          (agent-hq-fallback--note work (agent-hq-fallback-test--rejected '(isUsingOverage . t)))
          (agent-hq-fallback--handle asked nil)
          (should (string-match-p "\\`Work is at its 5-hour limit until .*, now on extra usage\\.  Move .* to personal\\?\\'"
                                  (car questions)))
          (should (equal (reverse moved) (list (list asked personal nil) (list idle personal nil))))
          ;; The busy one moves once its turn is over.
          (should (buffer-local-value 'agent-hq-fallback--hit busy))
          (should (eq (plist-get (agent-hq-fallback--limit work) :answer) 'all))
          ;; Then later turns move without asking.
          (setq moved nil questions nil)
          (with-current-buffer busy (setq agent-hq-test-status 'ready))
          (agent-hq-fallback--handle busy t)
          (should-not questions)
          (should (equal moved (list (list busy personal t)))))))))

(ert-deftest agent-hq-fallback-test-answer-no-stays ()
  (agent-hq-fallback-test--with-accounts
    (agent-hq-fallback-test--with-shells ((shell work 'ready))
      (let (moved (asked 0) (answer ?n))
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) (cl-incf asked) (list answer)))
                  ((symbol-function 'agent-hq-switch-account)
                   (lambda (&rest args) (push args moved))))
          (agent-hq-fallback--note work (agent-hq-fallback-test--rejected '(isUsingOverage . t)))
          (agent-hq-fallback--handle shell nil)
          (should (= asked 1))
          (should (eq (plist-get (agent-hq-fallback--limit work) :answer) 'stay))
          ;; More turns on extra usage don't ask again...
          (agent-hq-fallback--handle shell nil)
          (should (= asked 1))
          ;; ...but a refused one does.
          (setq answer ?y)
          (agent-hq-fallback--handle shell t)
          (should (= asked 2))
          (should (equal moved (list (list shell personal t)))))))))

(ert-deftest agent-hq-fallback-test-busy-or-quit ()
  (agent-hq-fallback-test--with-accounts
    (agent-hq-fallback-test--with-shells ((shell work 'busy))
      (let (moved (asked 0))
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) (cl-incf asked) (signal 'quit nil)))
                  ((symbol-function 'agent-hq-switch-account)
                   (lambda (&rest args) (push args moved))))
          (agent-hq-fallback--note work (agent-hq-fallback-test--rejected))
          ;; A turn began before the question: ask after it.
          (agent-hq-fallback--handle shell t)
          (should (= asked 0))
          (should (buffer-local-value 'agent-hq-fallback--hit shell))
          ;; Quitting the question changes nothing.
          (with-current-buffer shell (setq agent-hq-test-status 'ready))
          (agent-hq-fallback--handle shell t)
          (should (= asked 1))
          (should-not moved)
          (should-not (plist-get (agent-hq-fallback--limit work) :answer)))))))

(ert-deftest agent-hq-fallback-test-no-limit-no-question ()
  (agent-hq-fallback-test--with-accounts
    (agent-hq-fallback-test--with-shells ((shell work 'ready))
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (&rest _) (error "Asked"))))
        (agent-hq-fallback--handle shell t)))))

(ert-deftest agent-hq-fallback-test-queue-asks-one-at-a-time ()
  (agent-hq-fallback-test--with-accounts
    (agent-hq-fallback-test--with-shells ((a work 'ready) (b work 'ready))
      (let (handled)
        (cl-letf (((symbol-function 'agent-hq-fallback--handle)
                   (lambda (buffer failed) (push (list buffer failed) handled))))
          (agent-hq-fallback--offer a nil)
          (agent-hq-fallback--offer b nil)
          (agent-hq-fallback--offer a t)
          (should (equal agent-hq-fallback--queue (list (cons a t) (cons b nil))))
          (agent-hq-fallback--ask-next)
          (should (equal handled (list (list a t))))
          ;; The next one is asked on a timer of its own.
          (should (timerp agent-hq-fallback--timer))
          (agent-hq-fallback--ask-next)
          (should (equal (car handled) (list b nil)))
          (should-not agent-hq-fallback--queue))))))

;;;; Reset

(ert-deftest agent-hq-fallback-test-reset-names-shells-away ()
  (agent-hq-fallback-test--with-accounts
    (agent-hq-fallback-test--with-shells ((away personal 'ready) (home work 'ready))
      (let (notified)
        (cl-letf (((symbol-function 'agent-hq-notify)
                   (lambda (buffer title body) (push (list buffer title body) notified))))
          (agent-hq-fallback--note work (agent-hq-fallback-test--rejected))
          (agent-hq-fallback--limit-over work)
          (should-not (gethash work agent-hq-fallback--limits))
          (should (= (length notified) 1))
          (pcase-let ((`(,buffer ,title ,body) (car notified)))
            (should (eq buffer away))
            (should (equal title "Claude work: 5-hour limit reset"))
            (should (string-prefix-p "1 shell still on personal: " body))))))))

(ert-deftest agent-hq-fallback-test-new-shells-follow-answer ()
  (agent-hq-fallback-test--with-accounts
    (let ((default-directory repo))
      (should (equal (agent-hq--new-claude-config) work))
      (agent-hq-fallback--note work (agent-hq-fallback-test--rejected))
      (should (equal (agent-hq--new-claude-config) work))
      (agent-hq-fallback--answer work 'all)
      (should (equal (agent-hq--new-claude-config) personal))
      ;; A session resumed on the account that stores it stays there.
      (let ((agent-hq--claude-config-override work))
        (should (equal (agent-hq--new-claude-config) work)))
      (agent-hq-fallback--limit-over work)
      (should (equal (agent-hq--new-claude-config) work)))))

;;;; Moving

(defun agent-hq-fallback-test--write (file text)
  "Write TEXT to FILE, making its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun agent-hq-fallback-test--read (file)
  "Return FILE's text."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest agent-hq-fallback-test-copy-session ()
  (agent-hq-fallback-test--with-accounts
    (let* ((id "11111111-2222-3333-4444-555555555555")
           (key (agent-hq--claude-project-key repo))
           (source (expand-file-name (format "projects/%s/%s.jsonl" key id) work))
           (subagent (expand-file-name (format "projects/%s/%s/subagents/agent-a.jsonl" key id) work))
           (dest (expand-file-name (format "projects/%s/%s.jsonl" key id) personal)))
      (agent-hq-fallback-test--write source "{\"a\":1}\n")
      (agent-hq-fallback-test--write subagent "{\"sub\":1}\n")
      (should (equal (agent-hq-fallback-copy-session work personal id repo) dest))
      (should (equal (agent-hq-fallback-test--read dest) "{\"a\":1}\n"))
      (should (equal (agent-hq-fallback-test--read
                      (expand-file-name (format "projects/%s/%s/subagents/agent-a.jsonl" key id)
                                        personal))
                     "{\"sub\":1}\n"))
      (should-not (file-expand-wildcards (concat dest ".tmp*")))
      ;; It went on under personal: copying the stale work copy over it
      ;; would lose turns.
      (agent-hq-fallback-test--write dest "{\"a\":1}\n{\"b\":2}\n")
      (set-file-times source (time-subtract nil 60))
      (should-error (agent-hq-fallback-copy-session work personal id repo) :type 'user-error)
      ;; Back the other way it's the newer one, so it goes.
      (agent-hq-fallback-copy-session personal work id repo)
      (should (equal (agent-hq-fallback-test--read source) "{\"a\":1}\n{\"b\":2}\n"))
      (should-error (agent-hq-fallback-copy-session work personal "nope" repo) :type 'user-error))))

(ert-deftest agent-hq-fallback-test-copy-session-found-by-id ()
  ;; A folder not named after the directory (long paths get a hash).
  (agent-hq-fallback-test--with-accounts
    (let ((id "abc")
          (source (expand-file-name "projects/-long-hashed-1234/abc.jsonl" work)))
      (agent-hq-fallback-test--write source "{}\n")
      (should (equal (agent-hq-fallback-copy-session work personal id repo)
                     (expand-file-name "projects/-long-hashed-1234/abc.jsonl" personal))))))

(defvar agent-hq-fallback-test-log nil
  "Session ids the stub restart was given, latest first.")

(defun agent-hq-fallback-test--restart (log dir)
  "Return a stub `agent-shell-restart' that records into LOG, a symbol.
The shells it starts work in DIR.
Like the real one, it kills the shell and starts one with its name and
:agent-config, whose client maker runs."
  (cl-function
   (lambda (&key session-id)
     (let ((name (buffer-name))
           (config (map-elt agent-shell--state :agent-config)))
       (set log (cons session-id (symbol-value log)))
       (kill-buffer (current-buffer))
       (with-current-buffer (get-buffer-create name)
         (setq default-directory dir)
         (setq-local agent-shell--state (list (cons :agent-config config)
                                              (cons :session (list (cons :id session-id)))))
         (funcall (map-elt config :client-maker) (current-buffer)))))))

(ert-deftest agent-hq-fallback-test-switch-account ()
  (agent-hq-fallback-test--with-accounts
    (let* ((id "s1")
           (key (agent-hq--claude-project-key repo))
           (maker (lambda (buffer) (agent-hq-claude-environment-a #'ignore :buffer buffer)))
           subscribed
           (shell (with-current-buffer (generate-new-buffer " Claude (work) @ repo")
                    (setq default-directory repo)
                    (setq-local agent-shell--state
                                (list (cons :agent-config (list (cons :client-maker maker)))
                                      (cons :session (list (cons :id id)))))
                    (funcall maker (current-buffer))
                    (current-buffer))))
      (setq agent-hq-fallback-test-log nil)
      (agent-hq-fallback-test--write
       (expand-file-name (format "projects/%s/%s.jsonl" key id) work) "{}\n")
      (unwind-protect
          (cl-letf (((symbol-function 'agent-hq--status) (lambda (_) 'ready))
                    ((symbol-function 'agent-hq-refresh-buffer-name) #'ignore)
                    ((symbol-function 'agent-shell-subscribe-to)
                     (cl-function (lambda (&key event on-event &allow-other-keys)
                                    (push (cons event on-event) subscribed))))
                    ((symbol-function 'agent-shell-restart)
                     (agent-hq-fallback-test--restart 'agent-hq-fallback-test-log repo)))
            (should (equal (agent-hq-fallback--account shell) work))
            (let ((new (agent-hq-switch-account shell personal t)))
              (should (buffer-live-p new))
              (should-not (buffer-live-p shell))
              (should (equal (buffer-name new) " Claude (work) @ repo"))
              (should (equal agent-hq-fallback-test-log (list id)))
              (should (equal (agent-hq-fallback--account new) personal))
              (should (file-exists-p (expand-file-name (format "projects/%s/%s.jsonl" key id)
                                                       personal)))
              (should (buffer-local-value 'agent-hq-fallback--moved new))
              (should (assq 'init-finished subscribed))
              ;; Its restarts stay on personal, and moving back works.
              (setq shell new)
              (let ((back (agent-hq-switch-account new work)))
                (should (equal (agent-hq-fallback--account back) work))
                (setq shell back))
              (should-error (agent-hq-switch-account shell work) :type 'user-error)))
        (when (buffer-live-p shell) (kill-buffer shell))))))

(ert-deftest agent-hq-fallback-test-switch-account-keeps-config-on-failure ()
  (agent-hq-fallback-test--with-accounts
    (let* ((config (list (cons :client-maker #'ignore)))
           (shell (with-current-buffer (generate-new-buffer " shell")
                    (setq default-directory repo)
                    (setq-local agent-hq--claude-started-with work)
                    (setq-local agent-shell--state (list (cons :agent-config config)
                                                         (cons :session nil)))
                    (current-buffer))))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-hq--status) (lambda (_) 'ready))
                    ((symbol-function 'agent-shell-restart)
                     (cl-function (lambda (&key session-id)
                                    (ignore session-id)
                                    (user-error "Cancelled")))))
            (should-error (agent-hq-switch-account shell personal) :type 'user-error)
            (should (eq (map-elt (buffer-local-value 'agent-shell--state shell) :agent-config)
                        config))
            ;; Busy shells don't move.
            (cl-letf (((symbol-function 'agent-hq--status) (lambda (_) 'busy)))
              (should-error (agent-hq-switch-account shell personal) :type 'user-error)))
        (kill-buffer shell)))))

;;; agent-hq-fallback-test.el ends here
