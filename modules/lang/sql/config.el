;;; lang/sql/config.el -*- lexical-binding: t; -*-

;; sqls (lsp-mode's `sqls' client); connections come from
;; `+sql/add-postgres-db' / `+sql/add-mysql-db' in config.org.
(when (modulep! +lsp)
  (add-hook 'sql-mode-local-vars-hook #'lsp! 'append))
