;;; framework/vue/autoload.el -*- lexical-binding: t; -*-
;;;###autoload
(defun +vue/setup-lsp ()
  "Setup lsp backend."
  (if (modulep! :tools lsp)
      (progn
        ;; error checking from lsp langserver sucks, turn it off
        ;; so eslint won't be overriden
        (setq-local lsp-diagnostics-provider :none)
        (lsp))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))

;;;###autoload
(defun +vue/setup-yasnippet ()
  (progn
    (unless yas-global-mode (yas-global-mode 1))
    (yas-minor-mode 1))
  (yas-activate-extra-mode 'js-mode))

;;;###autoload
(defun +vue/setup-editor-style ()
  "such as indent rules comment style etc"
  ;; https://stackoverflow.com/questions/36701024/how-can-i-indent-inline-javascript-in-web-mode
  (setq web-mode-script-padding 0)
  ;; https://emacs.stackexchange.com/questions/27683/change-comment-style-in-web-mode
  (add-to-list 'web-mode-comment-formats '("javascript" . "//")))
