;;; agent-hq-history.el --- Resume sessions across ACP agents -*- lexical-binding: t; -*-

;;; Code:

(require 'agent-shell)
(require 'agent-hq)

(cl-defstruct (agent-hq-history--source (:constructor agent-hq-history--make-source))
  config account sessions (status 'pending)
  ;; Only clients started for this lookup belong to it.
  buffer client)

(defun agent-hq-history--name (source)
  "Return the agent name shown for SOURCE."
  (let ((config (agent-hq-history--source-config source)))
    (or (map-elt config :mode-line-name) (map-elt config :buffer-name)
        (symbol-name (map-elt config :identifier)))))

(defun agent-hq-history--matches-p (source shell)
  "Whether SHELL belongs to SOURCE's agent and account."
  (and (buffer-live-p shell)
       (with-current-buffer shell
         (and (eq (map-elt (agent-hq-history--source-config source) :identifier)
                  (map-nested-elt agent-shell--state '(:agent-config :identifier)))
              (or (null (agent-hq-history--source-account source))
                  (equal agent-hq--claude-started-with (agent-hq-history--source-account source)))))))

(defun agent-hq-history--failed (source err &rest _)
  "Record the lookup error ERR for SOURCE."
  (setf (agent-hq-history--source-status source)
        (if (stringp err) err (or (map-elt err 'message) "request failed"))))

(defun agent-hq-history--list (source client root)
  "Fetch all session list pages for ROOT through SOURCE's CLIENT."
  (let ((cwd (agent-shell--resolve-path root)))
    (agent-shell--list-sessions
     :state (list (cons :client client) (cons :active-requests nil))
     :cwd cwd :buffer (current-buffer)
     :on-failure (apply-partially #'agent-hq-history--failed source)
     :on-success
     (lambda (sessions)
       (setf (agent-hq-history--source-sessions source)
             (seq-filter
              (lambda (session)
                (let ((dir (map-elt session 'cwd)))
                  (and (stringp (map-elt session 'sessionId))
                       (or (null dir)
                           (and (stringp dir)
                                (or (equal (directory-file-name dir) (directory-file-name cwd))
                                    (file-equal-p dir cwd)))))))
              sessions)
             (agent-hq-history--source-status source) 'done)))))

(defun agent-hq-history--connect (source root)
  "Initialize a temporary ACP client for SOURCE in ROOT and request history."
  (with-current-buffer
      (setf (agent-hq-history--source-buffer source)
            (generate-new-buffer " *agent-hq session list*"))
    (setq default-directory root)
    (let ((major-mode 'agent-shell-mode)) (hack-dir-local-variables-non-file-buffer))
    (let* ((config (agent-hq-history--source-config source))
           (agent-hq--claude-config-override (agent-hq-history--source-account source)))
      (setq-local agent-shell--state
                  (agent-shell--make-state
                   :buffer (current-buffer) :agent-config config
                   :outgoing-request-decorator agent-shell-outgoing-request-decorator))
      (let ((client (setf (agent-hq-history--source-client source)
                          (funcall (map-elt config :client-maker) (current-buffer))))
            (failed (apply-partially #'agent-hq-history--failed source)))
        (setf (map-elt client :environment-variables)
              (agent-shell--resolve-environment-variables
               :environment-variables (map-elt client :environment-variables)))
        (acp-send-request
         :client client :buffer (current-buffer)
         :request (acp-make-initialize-request :protocol-version 1)
         :on-failure failed
         :on-success
         (lambda (response)
           (condition-case err
               (let* ((caps (map-elt response 'agentCapabilities))
                      (session-caps (or (map-elt response 'sessionCapabilities)
                                        (map-elt caps 'sessionCapabilities))))
                 (cond
                  ((not (and (assq 'list session-caps)
                             (or (eq (map-elt caps 'loadSession) t)
                                 (assq 'resume session-caps))))
                   (funcall failed "adapter cannot list and resume sessions"))
                  ((map-elt config :needs-authentication)
                   (acp-send-request
                    :client client :buffer (current-buffer)
                    :request (funcall (map-elt config :authenticate-request-maker))
                    :on-success (lambda (_) (agent-hq-history--list source client root))
                    :on-failure failed))
                  (t (agent-hq-history--list source client root))))
             (error (funcall failed (error-message-string err))))))))))

(defun agent-hq-history--discover (source root shells)
  "Request SOURCE's sessions for ROOT.  SHELLS are already open in ROOT."
  (if-let* ((live (seq-find
                   (lambda (shell)
                     (and (agent-hq-history--matches-p source shell)
                          (agent-shell-initialized-p :shell-buffer shell)
                          (with-current-buffer shell
                            (process-live-p (map-nested-elt agent-shell--state '(:client :process))))))
                   shells)))
      (with-current-buffer live
        (if (and (map-elt agent-shell--state :supports-session-list)
                 (or (map-elt agent-shell--state :supports-session-load)
                     (map-elt agent-shell--state :supports-session-resume)))
            (agent-hq-history--list source (map-elt agent-shell--state :client) root)
          (agent-hq-history--failed source "adapter cannot list and resume sessions")))
    (agent-hq-history--connect source root)))

(defun agent-hq-history--collect (root)
  "Return history sources for ROOT, releasing temporary clients even on quit."
  (let* ((default-directory root)
         (shells (agent-hq--shells-in root))
         (sources (mapcar
                   (lambda (config)
                     (agent-hq-history--make-source
                      :config config
                      :account (when (eq (map-elt config :identifier) 'claude-code)
                                 (or agent-hq--claude-started-with (agent-hq--new-claude-config)))))
                   (agent-shell--resolved-agent-configs))))
    (unwind-protect
        (progn
          (dolist (source sources)
            (condition-case err
                (agent-hq-history--discover source root shells)
              (error (setf (agent-hq-history--source-status source) (error-message-string err)))))
          ;; Keep completion on the invoking command's stack. C-g cancels the
          ;; lookup, and one unresponsive adapter cannot hide the other rows.
          (let ((deadline (+ (float-time) 10)))
            (while (and (seq-some (lambda (s) (eq (agent-hq-history--source-status s) 'pending))
                                  sources)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (dolist (source sources)
            (when (eq (agent-hq-history--source-status source) 'pending)
              (setf (agent-hq-history--source-status source) "timed out")))
          sources)
      (dolist (source sources)
        (when-let* ((client (agent-hq-history--source-client source)))
          (acp-shutdown :client client))
        (when-let* ((buffer (agent-hq-history--source-buffer source)))
          (kill-buffer buffer))))))

(defun agent-hq-history--open-shell (source id shells)
  "Find the shell that has or is loading SOURCE's session ID among SHELLS."
  (seq-find (lambda (shell)
              (and (agent-hq-history--matches-p source shell)
                   (with-current-buffer shell
                     (or (equal id (map-nested-elt agent-shell--state '(:session :id)))
                         (and (null (map-nested-elt agent-shell--state '(:session :id)))
                              (equal id (map-elt agent-shell--state :resume-session-id)))))))
            shells))

(defun agent-hq-history-resume (root)
  "List previous sessions in ROOT, then visit or resume the selected session."
  (message "Looking for sessions in %s…" (abbreviate-file-name root))
  (let ((shells (agent-hq--shells-in root))
        rows errors)
    (dolist (source (agent-hq-history--collect root))
      (if (eq (agent-hq-history--source-status source) 'done)
          (dolist (session (agent-hq-history--source-sessions source))
            (push (cons (cons :source source) session) rows))
        (push (concat (agent-hq-history--name source) ": "
                      (agent-hq-history--source-status source)) errors)))
    (unless rows
      (user-error "No resumable sessions for %s%s" (abbreviate-file-name root)
                  (if errors (concat " — " (string-join errors ", ")) "")))
    (let* ((candidates
            (mapcar (lambda (row)
                      (let* ((source (map-elt row :source))
                             (id (map-elt row 'sessionId))
                             (label (concat
                                     (agent-hq--col (agent-hq-history--name source) 12)
                                     (agent-hq--col (agent-shell--session-column-value 'date row) 20)
                                     (agent-hq--one-line (or (map-elt row 'title) id) 100)
                                     (when (agent-hq-history--open-shell source id shells)
                                       (propertize "  [open]" 'face 'shadow)))))
                        (cons label (cons (abbreviate-file-name root) row))))
                    (agent-shell--sort-sessions-by-recency rows)))
           (row (agent-hq--read
                 (if errors (format "Resume session [%s]: " (string-join errors ", "))
                   "Resume session: ")
                 candidates))
           (source (map-elt row :source))
           (id (map-elt row 'sessionId))
           (account (agent-hq-history--source-account source))
           (config (agent-hq-history--source-config source)))
      (if-let* ((open (agent-hq-history--open-shell source id (agent-hq--shells-in root))))
          (agent-hq-visit-shell open)
        (let ((default-directory root)
              (agent-hq--claude-config-override account))
          (agent-shell--start :config (if account (agent-hq--claude-config-for account config) config)
                              :session-id id :session-strategy 'prompt :new-session t :cwd root))))))

(provide 'agent-hq-history)
;;; agent-hq-history.el ends here
