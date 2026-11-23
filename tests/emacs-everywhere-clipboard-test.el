;;; emacs-everywhere-clipboard-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-everywhere)
(load (expand-file-name "../lisp/emacs-everywhere-clipboard.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro my-ee-test--with-session (&rest body)
  "Run BODY with a finishing session, without touching application windows."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((emacs-everywhere--contents "")
           (emacs-everywhere-final-hooks nil)
           (emacs-everywhere--copy-command nil)
           (emacs-everywhere--window-focus-command '("focus" "%w"))
           (emacs-everywhere--paste-command '("paste" "now"))
           (emacs-everywhere-clipboard-sleep-delay 0)
           (emacs-everywhere-current-app
            (make-emacs-everywhere-app :id "test-app"))
           (interprogram-cut-function nil)
           (kill-ring nil)
           events)
       (setq-local emacs-everywhere-mode t)
       (cl-letf (((symbol-function 'gui-select-text) #'ignore)
                 ((symbol-function 'gui-backend-set-selection) #'ignore)
                 ((symbol-function 'frame-parameter) (lambda (&rest _) t))
                 ((symbol-function 'server-buffer-done)
                  (lambda (&rest _) (push 'done events))))
         ,@body))))

(ert-deftest my-ee-clipboard-large-round-trip-before-paste ()
  ;; Sandboxed batch Emacs cannot access the macOS pasteboard.  Also run
  ;; this suite in the existing GUI Emacs for the real clipboard check.
  (skip-unless (and (eq system-type 'darwin)
                   (eq 0 (call-process "pbpaste" nil nil nil))))
  (let ((real-call-process (symbol-function 'call-process))
        (original-clipboard
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (let ((coding-system-for-read 'no-conversion))
             (should (eq 0 (call-process "pbpaste" nil t nil))))
           (buffer-string))))
    (unwind-protect
        (my-ee-test--with-session
          (let* ((text (concat (apply #'concat
                                     (make-list 18000 "λ 😀 \"quoted\" \\slash\t  \n"))
                               "last line  "))
                 (expected (concat text "\nfinal hook  "))
                 (emacs-everywhere-final-hooks
                  (list (lambda () (goto-char (point-max))
                          (insert "\nfinal hook  ")))))
            (insert text)
            (cl-letf (((symbol-function 'call-process)
                       (lambda (program &rest args)
                         (cond
                          ((equal program "focus") (push 'focus events) 0)
                          ((equal program "paste")
                           (with-temp-buffer
                             (let ((coding-system-for-read 'utf-8-unix))
                               (should (eq 0 (funcall real-call-process
                                                     "pbpaste" nil t nil))))
                             (should (equal (buffer-string) expected)))
                           (push 'paste events) 0)
                          ((equal program "osascript")
                           (ert-fail (format "Unexpected osascript arguments: %S"
                                             (mapcar (lambda (arg)
                                                       (if (stringp arg)
                                                           (substring arg 0 (min 80 (length arg)))
                                                         arg))
                                                     args))))
                          (t (apply real-call-process program args))))))
              ;; Exercise the exact command bound to C-c C-c.
              (emacs-everywhere--finish-or-ctrl-c-ctrl-c))
            (should (equal (car kill-ring) expected))
            (should (equal (reverse events) '(focus paste done)))
            (should-not emacs-everywhere-mode)))
      ;; Restore the clipboard even if the regression test fails.
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert original-clipboard)
        (let ((coding-system-for-write 'no-conversion))
          (should (eq 0 (call-process-region (point-min) (point-max)
                                            "pbcopy" nil nil nil))))))))

(ert-deftest my-ee-clipboard-failure-keeps-session-open ()
  (let ((system-type 'darwin))
    (my-ee-test--with-session
      (insert "keep this text")
      (cl-letf (((symbol-function 'call-process-region) (lambda (&rest _) 1)))
        (should-error (emacs-everywhere--finish-or-ctrl-c-ctrl-c)))
      (should emacs-everywhere-mode)
      (should (equal (buffer-string) "keep this text"))
      (should-not events))))

(ert-deftest my-ee-clipboard-abort-does-not-copy-or-paste ()
  (let ((system-type 'darwin)
        (real-call-process (symbol-function 'call-process)))
    (my-ee-test--with-session
      (insert "discard this edit")
      (cl-letf (((symbol-function 'call-process-region)
                 (lambda (&rest _) (ert-fail "Unexpected clipboard write")))
                ((symbol-function 'call-process)
                 (lambda (program &rest args)
                   (cond
                    ((equal program "focus") (push 'focus events) 0)
                    ((member program '("paste" "osascript"))
                     (ert-fail "Unexpected paste or clipboard write"))
                    (t (apply real-call-process program args))))))
        (emacs-everywhere-finish t))
      (should (equal (reverse events) '(focus done))))))

(ert-deftest my-ee-clipboard-unrelated-calls-pass-through ()
  (let ((system-type 'darwin)
        (real-call-process (symbol-function 'call-process))
        calls)
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &rest args)
                 (if (equal program "osascript")
                     (progn (push (cons program args) calls) 0)
                   (apply real-call-process program args)))))
      (my-emacs-everywhere--finish-with-pbcopy
       (lambda () (call-process "osascript" nil nil nil "-e" "activate"))))
    (should (equal calls '(("osascript" nil nil nil "-e" "activate"))))))

;;; emacs-everywhere-clipboard-test.el ends here
