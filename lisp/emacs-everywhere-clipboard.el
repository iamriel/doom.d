;;; emacs-everywhere-clipboard.el --- Safe macOS clipboard writes -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun my-emacs-everywhere--finish-with-pbcopy (finish &rest args)
  "Run FINISH with ARGS, sending macOS clipboard text through stdin.
The installed Emacs Everywhere calls osascript with the entire buffer
as an argument.  Use synchronous pbcopy instead to avoid ARG_MAX and
ensure copying finishes before the original function pastes or closes."
  (if (not (eq system-type 'darwin))
      (apply finish args)
    (let ((original-call-process (symbol-function 'call-process)))
      ;; Scope the replacement to finishing an Emacs Everywhere buffer;
      ;; application focus and paste AppleScripts still run normally.
      (cl-letf (((symbol-function 'call-process)
                 (lambda (program &optional infile destination display &rest argv)
                   (if (and (equal program "osascript")
                            (equal (car argv) "-e")
                            (stringp (cadr argv))
                            (string-match-p "\\`set \\(?:the \\)?clipboard to "
                                            (cadr argv)))
                       (let ((coding-system-for-write 'utf-8-unix)
                             (process-connection-type nil))
                         (let ((status (call-process-region
                                        (point-min) (point-max) "pbcopy"
                                        nil nil nil)))
                           (unless (eq status 0)
                             (error "Emacs Everywhere clipboard copy failed: %s"
                                    status))
                           status))
                     (apply original-call-process
                            program infile destination display argv)))))
        (apply finish args)))))

(advice-add 'emacs-everywhere-finish :around
            #'my-emacs-everywhere--finish-with-pbcopy)

(provide 'emacs-everywhere-clipboard)
;;; emacs-everywhere-clipboard.el ends here
