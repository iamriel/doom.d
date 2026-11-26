;;; agent-hq-history-test.el --- Worktree session history tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-hq-history)

(ert-deftest agent-hq-history-test-resume-from-a-shell ()
  ;; Exercise the key command, ACP pagination, and the completion table with
  ;; the real agent-shell package. Only the adapter transport and UI are fake.
  (with-temp-buffer
    (setq-local agent-shell-session-strategy 'new)
    (let* ((root default-directory)
           (agent-shell-preferred-agent-config 'claude-code)
           (make-config (lambda (name)
                          (list (cons :identifier name)
                                (cons :client-maker (lambda (_) (list (cons :test-agent name)))))))
           (agent-shell-agent-configs (mapcar make-config '(claude-code codex another missing)))
           (agent-hq--claude-started-with 'default)
           (agent-shell-session-list-page-limit nil)
           seen stopped started)
      (cl-letf (((symbol-function 'agent-hq--current-root) (lambda () root))
                ((symbol-function 'agent-hq--shells-in) #'ignore)
                ((symbol-function 'acp-shutdown)
                 (cl-function (lambda (&key client) (push (map-elt client :test-agent) stopped))))
                ((symbol-function 'acp-send-request)
                 (cl-function
                  (lambda (&key client request on-success on-failure &allow-other-keys)
                    (let ((name (map-elt client :test-agent)))
                      (pcase (map-elt request :method)
                        ("initialize"
                         (if (eq name 'missing) (funcall on-failure '((message . "unavailable")) nil)
                           (funcall on-success '((agentCapabilities . ((loadSession . t)
                                                                      (sessionCapabilities . ((list)))))))))
                        ("session/list"
                         (should (file-equal-p root (map-nested-elt request '(:params cwd))))
                         (cond
                          ((map-nested-elt request '(:params cursor))
                           (funcall on-success
                                    '((sessions . [((sessionId . "second") (updatedAt . "2026-11-25"))]))))
                          (t
                           (funcall on-success
                                    `((sessions . [((sessionId . "same-id") (title . "Same title")
                                                    (updatedAt . ,(if (eq name 'codex) "2026-11-26" "2026-11-24")))
                                                   ((sessionId . "wrong-worktree") (cwd . "/elsewhere"))])
                                      ,@(when (eq name 'another) '((nextCursor . "page-2"))))))))
                        (_ (ert-fail "Discovery attempted to start or load a session")))))))
                ((symbol-function 'completing-read)
                 (lambda (prompt table &rest _)
                   (should (string-match-p "missing: unavailable" prompt))
                   (should-not started)
                   (setq seen (all-completions "" table))
                   (car seen)))
                ((symbol-function 'agent-shell--start) (lambda (&rest args) (setq started args))))
        (call-interactively #'agent-hq-resume-session)
        (should (= 4 (length seen)))
        (should (string-prefix-p "codex " (car seen)))
        (should (string-match-p "second" (nth 1 seen)))
        (should (equal (map-elt (plist-get started :config) :identifier) 'codex))
        (should (equal (plist-get started :session-id) "same-id"))
        (should (equal (plist-get started :cwd) root))
        (should (eq (plist-get started :session-strategy) 'prompt))
        (should (plist-get started :new-session))
        (should (= 4 (length stopped)))
        (should-not (get-buffer " *agent-hq session list*"))
        (should (eq agent-shell-session-strategy 'new))))))

(ert-deftest agent-hq-history-test-visit-open-session-by-agent ()
  (let* ((config '((:identifier . codex)))
         (source (agent-hq-history--make-source :config config :status 'done
                                                :sessions '(((sessionId . "same-id")))))
         (other (generate-new-buffer " *history-other*"))
         visited)
    (unwind-protect
        (with-temp-buffer
          (setq-local agent-shell--state '((:agent-config . ((:identifier . claude-code)))
                                           (:session . ((:id . "same-id")))))
          (with-current-buffer other
            (setq-local agent-shell--state `((:agent-config . ,config) (:resume-session-id . "same-id"))))
          (cl-letf (((symbol-function 'agent-hq--shells-in) (lambda (_) (list (current-buffer) other)))
                    ((symbol-function 'agent-hq-history--collect) (lambda (_) (list source)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt table &rest _)
                       (let ((row (car (all-completions "" table))))
                         (should (string-match-p (regexp-quote "[open]") row))
                         row)))
                    ((symbol-function 'agent-hq-visit-shell) (lambda (shell) (setq visited shell)))
                    ((symbol-function 'agent-shell--start) (lambda (&rest _) (ert-fail "Duplicate shell"))))
            (agent-hq-history-resume default-directory)
            (should (eq visited other))))
      (kill-buffer other))))

(ert-deftest agent-hq-history-test-empty-and-cancel ()
  (let ((source (agent-hq-history--make-source :config '((:identifier . another)) :status 'done)))
    (cl-letf (((symbol-function 'agent-hq--shells-in) #'ignore)
              ((symbol-function 'agent-hq-history--collect) (lambda (_) (list source)))
              ((symbol-function 'completing-read) (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'agent-shell--start) (lambda (&rest _) (ert-fail "Unexpected shell"))))
      (should-error (agent-hq-history-resume default-directory) :type 'user-error)
      (setf (agent-hq-history--source-sessions source) '(((sessionId . "one"))))
      (should (eq 'cancelled (condition-case nil (agent-hq-history-resume default-directory)
                               (quit 'cancelled)))))))

;;; agent-hq-history-test.el ends here
