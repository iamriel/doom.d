;; -*- no-byte-compile: t; -*-
;;; lang/sql/packages.el

(package! sql)

(package! sql-indent
  :recipe (:host github :repo "alex-hhh/emacs-sql-indent"
           :files ("sql-indent.el")))

(package! sqlfmt
  :recipe (:local-repo "local/sqlfmt"))
