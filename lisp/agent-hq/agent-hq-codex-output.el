;;; agent-hq-codex-output.el --- Bound Codex terminal previews -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'agent-shell-openai)

(defvar agent-shell--transcript-file)
(defvar agent-shell--state)
(declare-function agent-shell--longest-backtick-run "agent-shell")
(declare-function agent-shell--tool-call-update-output-markdown "agent-shell")

(defvar agent-hq-codex-output-limit (* 64 1024)
  "Maximum characters of streamed Codex command output to keep inline.")

(defvar-local agent-hq-codex-output--failed-transcript nil
  "Transcript whose log directory last failed, to avoid retrying every chunk.")

(defun agent-hq-codex-output--delta-only-p (update)
  "Whether UPDATE carries just terminal output, with no status or other event."
  (and (seq-every-p (lambda (field) (memq (car field) '(sessionUpdate toolCallId _meta)))
                    update)
       (equal (map-keys (map-elt update '_meta)) '(terminal_output_delta))))

(defun agent-hq-codex-output--start-log (id prefix delta)
  "Write PREFIX and DELTA to a new log and cache its preview for tool ID."
  (let* ((dir (expand-file-name "tool-output/" (file-name-directory agent-shell--transcript-file)))
         (prefix (or prefix ""))
         (delta (or delta ""))
         (snippet (concat (substring prefix 0 (min 2048 (length prefix)))
                          (substring delta 0 (min (max 0 (- 2048 (length prefix))) (length delta)))))
         (fence (make-string (max 3 (1+ (agent-shell--longest-backtick-run snippet))) ?`)))
    (make-directory dir t)
    (let* ((file (make-temp-file (expand-file-name "codex-" dir) nil ".log"))
           (preview (format "[Full command output](%s)\n\n%s\n%s\n%s" file fence snippet fence))
           (ready nil))
      (unwind-protect
          (progn
            (unless (string-empty-p prefix) (write-region prefix nil file t 'silent))
            (unless (string-empty-p delta) (write-region delta nil file t 'silent))
            (unless agent-shell-openai--terminal-output-snapshots
              (agent-shell-openai--subscribe-output-cleanup))
            (put-text-property 0 1 'agent-hq-codex-output-file file preview)
            (setf (map-elt agent-shell-openai--terminal-output-snapshots id) preview)
            (setq ready t)
            preview)
        (unless ready (delete-file file))))))

(defun agent-hq-codex-output--show (notification preview)
  "Put PREVIEW into NOTIFICATION, preserving structured items such as diffs."
  (let ((content (map-nested-elt notification '(params update content))))
    (setf (alist-get 'content (alist-get 'update (alist-get 'params notification)))
          (append (seq-remove (lambda (item) (equal (map-nested-elt item '(content type)) "text"))
                              content)
                  (list `((type . "content")
                          (content (type . "text") (text . ,(substring-no-properties preview))))))))
  notification)

(cl-defun agent-hq-codex-output--adapt (&key acp-notification)
  "Spool large Codex terminal streams beside their local shell transcript.
The upstream adapter owns small streams and the lifetime of its per-tool
snapshot cache.  For a spooled command, the cached string is a bounded
preview with the log filename on its first character."
  (let* ((update (map-nested-elt acp-notification '(params update)))
         (id (map-elt update 'toolCallId))
         (data (map-nested-elt update '(_meta terminal_output_delta data)))
         (delta (and (stringp data) data))
         (previous (map-elt agent-shell-openai--terminal-output-snapshots id))
         (file (and previous (> (length previous) 0)
                    (get-text-property 0 'agent-hq-codex-output-file previous))))
    (if (not (and (equal (map-elt acp-notification 'method) "session/update")
                  (equal (map-elt update 'sessionUpdate) "tool_call_update")
                  (stringp id) (or delta previous)
                  agent-shell--transcript-file (not (file-remote-p agent-shell--transcript-file))
                  (not (equal agent-shell--transcript-file agent-hq-codex-output--failed-transcript))))
        (agent-shell-openai--notification-adapter :acp-notification acp-notification)
      (let* ((content (map-elt update 'content))
             (raw (map-nested-elt update '(rawOutput formatted_output)))
             ;; As in the Codex adapter, an authoritative snapshot supersedes
             ;; even a delta accompanying it.  Diff/terminal items have no text.
             (snapshot (when (or content (stringp raw))
                         (let ((text (agent-shell--tool-call-update-output-markdown update)))
                           (and (or (stringp raw) (not (string-empty-p text))) text))))
             (size (if snapshot (length snapshot) (+ (length previous) (length delta))))
             (coding-system-for-write 'utf-8-unix)
             (write-region-inhibit-fsync t))
        (condition-case err
            (cond
             ;; A small authoritative replacement takes precedence over a delta.
             ((and (or (not file) snapshot) (<= size agent-hq-codex-output-limit))
              (agent-shell-openai--notification-adapter :acp-notification acp-notification))
             ((or (not file) snapshot)
              (agent-hq-codex-output--show
               acp-notification (agent-hq-codex-output--start-log
                                 id (or snapshot previous) (unless snapshot delta))))
             (t
              (when delta (write-region delta nil file t 'silent))
              (if (and delta (agent-hq-codex-output--delta-only-p update))
                  'agent-hq-codex-output-appended
                (agent-hq-codex-output--show acp-notification previous))))
          (file-error
           ;; Keep processing the event (especially completion or failure)
           ;; when the volume is full or the transcript directory is read-only.
           (setq agent-hq-codex-output--failed-transcript agent-shell--transcript-file)
           (message "Codex output log unavailable; resuming inline output: %s" (error-message-string err))
           (agent-shell-openai--notification-adapter :acp-notification acp-notification)))))))

(defun agent-hq-codex-output--handled-p (event)
  "Tell agent-shell when EVENT only appended bytes to an existing log."
  (eq (map-elt event :acp-notification) 'agent-hq-codex-output-appended))

(defun agent-hq-codex-output--configure (config)
  "Install the bounded preview in an otherwise stock Codex CONFIG."
  (if (and (eq (map-elt config :notification-adapter) #'agent-shell-openai--notification-adapter)
           (not (map-elt config :notification-handler)))
      (map-insert (map-insert config :notification-adapter #'agent-hq-codex-output--adapt)
                  :notification-handler #'agent-hq-codex-output--handled-p)
    config))

(defun agent-hq-codex-output-setup ()
  "Install the Codex preview for new and already running shells."
  (advice-add 'agent-shell-openai-make-codex-config :filter-return #'agent-hq-codex-output--configure)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* (((derived-mode-p 'agent-shell-mode))
                  (state (bound-and-true-p agent-shell--state))
                  (config (map-elt state :agent-config))
                  ((eq (map-elt config :identifier) 'codex)))
        (setf (map-elt state :agent-config) (agent-hq-codex-output--configure config))))))

(provide 'agent-hq-codex-output)
;;; agent-hq-codex-output.el ends here
