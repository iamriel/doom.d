;;; emacs-everywhere-input-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-everywhere)
(load (expand-file-name "../lisp/emacs-everywhere-input.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(load (expand-file-name "../lisp/emacs-everywhere-clipboard.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest my-ee-input-large-text-is-imported-via-file ()
  (let* ((system-type 'darwin)
         (path (make-temp-name (expand-file-name "ee-input-" temporary-file-directory)))
         (text (concat (apply #'concat (make-list 20000 "λ 😀 \"quote\" \\slash\t  \n"))
                       "last line  "))
         (params (list "-c" path)))
    (unwind-protect
        (cl-letf (((symbol-function 'my-emacs-everywhere--capture-text)
                   (lambda (_) text)))
          (should (equal params (my-emacs-everywhere--prepare-input
                                 (lambda (&rest _) params) nil)))
          (should (= (file-modes path) #o600))
          (with-temp-buffer
            (insert-file-contents path)
            (setq buffer-file-name path)
            (my-emacs-everywhere--insert-selection
             (lambda () (ert-fail "Imported text would be inserted twice")))
            (should (equal (buffer-string) text))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest my-ee-input-empty-textbox-does-not-import-stale-clipboard ()
  (let* ((system-type 'darwin)
         (path (make-temp-name (expand-file-name "ee-input-" temporary-file-directory))))
    (unwind-protect
        (cl-letf (((symbol-function 'my-emacs-everywhere--capture-text)
                   (lambda (_) "")))
          (my-emacs-everywhere--prepare-input (lambda (&rest _) (list "-c" path)) nil)
          (with-temp-buffer
            (setq buffer-file-name path)
            (my-emacs-everywhere--insert-selection
             (lambda () (insert "stale clipboard")))
            (should (= (buffer-size) 0))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest my-ee-input-external-editor-file-is-preserved ()
  (let ((system-type 'darwin)
        (params '("-c" "/tmp/external-editor")))
    (cl-letf (((symbol-function 'my-emacs-everywhere--capture-text)
               (lambda (_) (ert-fail "External editor should use its supplied file"))))
      (should (equal params (my-emacs-everywhere--prepare-input
                             (lambda (&rest _) params) nil "/tmp/external-editor"))))))

(ert-deftest my-ee-input-no-textbox-opens-empty-clipboard-only-session ()
  (let* ((system-type 'darwin)
         (my-emacs-everywhere--input-files (make-hash-table :test #'equal))
         (path (make-temp-name (expand-file-name "ee-input-" temporary-file-directory))))
    (unwind-protect
        (cl-letf (((symbol-function 'my-emacs-everywhere--capture-text)
                   (lambda (_) nil)))
          (should (equal (list "-c" path)
                         (my-emacs-everywhere--prepare-input
                          (lambda (&rest _) (list "-c" path)) nil)))
          (with-temp-buffer
            (setq buffer-file-name path)
            (setq-local emacs-everywhere--window-focus-command '("focus")
                        emacs-everywhere--paste-command '("paste"))
              (my-emacs-everywhere--insert-selection
               (lambda () (insert "stale clipboard")))
              (my-emacs-everywhere-buffer-setup)
              (should (= 0 (buffer-size)))
              (should-not emacs-everywhere--window-focus-command)
              (should-not emacs-everywhere--paste-command)
              (should (= 0 (hash-table-count my-emacs-everywhere--input-files)))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest my-ee-input-other-platforms-use-original-import ()
  (let ((system-type 'gnu/linux)
        (params '("-c" "/tmp/ee-input")))
    (cl-letf (((symbol-function 'my-emacs-everywhere--capture-text)
               (lambda (_) (ert-fail "Unexpected macOS capture"))))
      (should (equal params (my-emacs-everywhere--prepare-input
                             (lambda (&rest _) params) nil))))
    (with-temp-buffer
      (my-emacs-everywhere--insert-selection (lambda () (insert "selection")))
      (should (equal (buffer-string) "selection")))))

(ert-deftest my-ee-input-default-hooks-are-restored ()
  (dolist (hook '(emacs-everywhere-set-frame-name
                  emacs-everywhere-set-frame-position
                  emacs-everywhere-apply-major-mode
                  emacs-everywhere-insert-selection))
    (should (memq hook emacs-everywhere-init-hooks)))
  (should-not (memq 'emacs-everywhere-remove-trailing-whitespace
                    emacs-everywhere-init-hooks)))

(ert-deftest my-ee-input-json-output-preserves-exact-text ()
  (let ((expected "λ 😀 \"quoted\" \\slash\t  \nlast line  ")
        (real-call-process (symbol-function 'call-process)))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &rest args)
                 (if (equal program "osascript")
                     (progn (insert (decode-coding-string
                                     (json-serialize expected) 'utf-8)
                                    "\n") 0)
                   (apply real-call-process program args)))))
      (should (equal expected
                     (my-emacs-everywhere--capture-text
                      (make-emacs-everywhere-app :id "test-app")))))))

(ert-deftest my-ee-input-capture-error-is-optional ()
  (let ((real-call-process (symbol-function 'call-process)))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &rest args)
                 (if (equal program "osascript")
                     (progn (insert "osascript is not allowed to send keystrokes") 1)
                   (apply real-call-process program args)))))
      (should-not (my-emacs-everywhere--capture-text
                   (make-emacs-everywhere-app :id "test-app"))))))

(ert-deftest my-ee-input-no-active-application-has-fallback-metadata ()
  (let ((system-type 'darwin))
    (dolist (app-info (list (lambda () nil) (lambda () (error "No active application"))))
      (let ((app (my-emacs-everywhere--optional-app-info app-info)))
        (should (emacs-everywhere-app-p app))
        (should (equal "" (emacs-everywhere-app-id app)))
        (should (equal "" (emacs-everywhere-app-class app)))
        (should (equal "" (emacs-everywhere-app-title app)))))))

(ert-deftest my-ee-input-active-textbox-keeps-focus-and-paste ()
  (let ((my-emacs-everywhere--source-textbox t)
        (emacs-everywhere--window-focus-command '("focus"))
        (emacs-everywhere--paste-command '("paste")))
    (with-temp-buffer
      (my-emacs-everywhere-buffer-setup)
      (should (equal emacs-everywhere--window-focus-command '("focus")))
      (should (equal emacs-everywhere--paste-command '("paste"))))))

(ert-deftest my-ee-input-no-source-finish-still-copies-content ()
  (let ((system-type 'darwin)
        (real-call-process (symbol-function 'call-process)))
    (with-temp-buffer
      (insert "New scratch content λ 😀")
      (setq-local emacs-everywhere-mode t
                  my-emacs-everywhere--source-textbox nil
                  emacs-everywhere--window-focus-command '("focus")
                  emacs-everywhere--paste-command '("paste"))
      (let ((emacs-everywhere--contents "")
            (emacs-everywhere-final-hooks nil)
            (emacs-everywhere--copy-command nil)
            (emacs-everywhere-clipboard-sleep-delay 0)
            (interprogram-cut-function nil)
            (kill-ring nil)
            copied done)
        (my-emacs-everywhere-buffer-setup)
        (cl-letf (((symbol-function 'gui-select-text) #'ignore)
                  ((symbol-function 'gui-backend-set-selection) #'ignore)
                  ((symbol-function 'call-process-region)
                   (lambda (start end program &rest _)
                     (should (equal program "pbcopy"))
                     (setq copied (buffer-substring-no-properties start end))
                     0))
                  ((symbol-function 'call-process)
                   (lambda (program &rest args)
                     (if (member program '("focus" "paste" "osascript"))
                         (ert-fail "A scratch session must not focus or paste into another app")
                       (apply real-call-process program args))))
                  ((symbol-function 'server-buffer-done)
                   (lambda (&rest _) (setq done t))))
          (emacs-everywhere--finish-or-ctrl-c-ctrl-c))
        (should (equal copied "New scratch content λ 😀"))
        (should done)))))

;;; emacs-everywhere-input-test.el ends here
