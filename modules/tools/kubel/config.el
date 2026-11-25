;; In tools/kubel/config.el
(use-package! kubel
  :defer t
  :init
  (setq
   kubel-use-namespace-list 'off
   kubel-namespace "default")
  ;; :config
  ;; (require 'kubel-evil)
  ;; (add-hook 'kubel-mode-hook #'kubel-evil-mode)
  )
