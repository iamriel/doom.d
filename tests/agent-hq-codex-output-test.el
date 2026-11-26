;;; agent-hq-codex-output-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-hq-codex-output)

(defun agent-hq-codex-output-test--update (id &rest fields)
  ;; Use fresh lists because the upstream adapter modifies its input.
  (let* ((notification `((method . "session/update")
                         (params (update (sessionUpdate . "tool_call_update")
                                         (toolCallId . ,id) ,@(copy-tree fields t)))))
         (config (agent-hq-codex-output--configure (agent-shell-openai-make-codex-config)))
         (result (funcall (map-elt config :notification-adapter) :acp-notification notification)))
    (unless (funcall (map-elt config :notification-handler) `((:acp-notification . ,result)))
      (map-nested-elt result '(params update)))))

(defun agent-hq-codex-output-test--delta (text)
  `(_meta (terminal_output_delta (data . ,text))))

(ert-deftest agent-hq-codex-output-stream-and-status ()
  (let ((dir (make-temp-file "agent-hq-output-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((agent-shell--transcript-file (expand-file-name "turn.md" dir))
                (agent-hq-codex-output-limit 16))
            (cl-letf (((symbol-function 'agent-shell-openai--subscribe-output-cleanup) #'ignore))
              (should (equal (agent-shell--tool-call-update-output-markdown
                              (agent-hq-codex-output-test--update "a" (agent-hq-codex-output-test--delta "small")))
                             "```\nsmall\n```"))
              (let* ((large (make-string 6000 ?λ))
                     (update (agent-hq-codex-output-test--update "a" (agent-hq-codex-output-test--delta large)))
                     (preview (agent-shell--tool-call-update-output-markdown update))
                     (cached (map-elt agent-shell-openai--terminal-output-snapshots "a"))
                     (file (get-text-property 0 'agent-hq-codex-output-file cached)))
                (should (< (length preview) 2500))
                (should (string-search file preview))
                (should (equal (logand (file-modes file) #o777) #o600))
                (should-not (agent-hq-codex-output-test--update "a" (agent-hq-codex-output-test--delta "middle")))
                (should (agent-hq-codex-output-test--update "b" (agent-hq-codex-output-test--delta "other")))
                (let ((end (agent-hq-codex-output-test--update
                            "a" '(status . "failed") (agent-hq-codex-output-test--delta "end"))))
                  (should (equal (map-elt end 'status) "failed"))
                  (should (equal preview (agent-shell--tool-call-update-output-markdown end))))
                (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                               (concat "small" large "middleend")))
                (should (equal (agent-shell--tool-call-update-output-markdown
                                (agent-hq-codex-output-test--update "a" '(rawOutput (formatted_output . "new"))))
                               "new"))
                ;; A final summary does not erase the original streamed log.
                (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                               (concat "small" large "middleend")))))))
      (delete-directory dir t))))

(ert-deftest agent-hq-codex-output-first-large-and-mixed-update ()
  (let ((dir (make-temp-file "agent-hq-output-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((agent-shell--transcript-file (expand-file-name "turn.md" dir))
                (agent-hq-codex-output-limit 8)
                (diff '((type . "diff") (path . "code.el") (oldText . "a") (newText . "b"))))
            (cl-letf (((symbol-function 'agent-shell-openai--subscribe-output-cleanup) #'ignore))
              (let* ((update (agent-hq-codex-output-test--update
                              "c" (cons 'content (vector diff)) (agent-hq-codex-output-test--delta "first large chunk")))
                     (file (get-text-property 0 'agent-hq-codex-output-file
                                              (map-elt agent-shell-openai--terminal-output-snapshots "c"))))
                (should (equal (seq-first (map-elt update 'content)) diff))
                (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "first large chunk"))
                (should (agent-hq-codex-output-test--update "c" '(_meta (terminal_exit (exit_code . 0)))))
                (should (agent-hq-codex-output-test--update "c" '(status . "completed")))))))
      (delete-directory dir t))))

(ert-deftest agent-hq-codex-output-disabled-or-unwritable-transcript ()
  (with-temp-buffer
    (let ((agent-hq-codex-output-limit 1)
          (attempts 0))
      (cl-letf (((symbol-function 'agent-shell-openai--subscribe-output-cleanup) #'ignore)
                ((symbol-function 'agent-hq-codex-output--start-log)
                 (lambda (&rest _) (cl-incf attempts) (signal 'file-error '("Read-only volume")))))
        (dolist (transcript '(nil "/unused/turn.md" "/unused/turn.md"))
          (let ((agent-shell--transcript-file transcript))
            (should (agent-hq-codex-output-test--update "a" (agent-hq-codex-output-test--delta "chunk")))))
        (should (= attempts 1))
        (should (equal (map-elt agent-shell-openai--terminal-output-snapshots "a") "chunkchunkchunk"))))))

(provide 'agent-hq-codex-output-test)
