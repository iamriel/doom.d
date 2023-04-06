;;; lang/sql/autoload.el -*- lexical-binding: t; -*-
;;; taken from https://dev.to/viglioni/emacs-as-sql-client-with-lsp-143l

(defvar +sql-workspace-base-name "<sql-")

;;;###autoload
(defmacro any-nil? (&rest args)
  `(not (and ,@args)))

;;;###autoload
(defmacro throw-if (condition &optional error-description)
  "if condition is true, thrown an error"
  `(if ,condition (error (or ,error-description ""))))

;;;###autoload
(defun +sql/format-postgres-sqls (host port user password db)
  (format "host=%s port=%s user=%s password=%s dbname=%s"
          host port user password db))

;;;###autoload
(defun +sql/format-mysql-sqls (host port user password db)
  (format "%s:%s@tcp(%s:%s)/%s" user password host port db))

;;;###autoload
(defun +sql/format-postgres-uri (host port user password db)
  (format "postgresql://%s:%s@%s:%s/%s" user password host port db))


;;;###autoload
(defun +sql/add-to-sqls-connections (db-type data-src-name)
  (add-to-list 'lsp-sqls-connections
               (list (cons 'driver db-type)
                     (cons 'dataSourceName data-src-name))))

;;;###autoload
(defmacro +sql/add-to-sql-conection-alist (db-type name host port user password db)
  `(add-to-list 'sql-connection-alist
                (list (quote ,name)
                      (list 'sql-product (quote ,db-type))
                      (list 'sql-user ,user)
                      (list 'sql-server ,host)
                      (list 'sql-port ,port)
                      (list 'sql-password ,password)
                      (list 'sql-database ,db))))

;;;###autoload
(defmacro +sql/add-postgres-db (name &rest db-info)
  "Adds a postgres database to emacs and lsp
   This macro expects a name to the database and a p-list of parameters
   :port, :user, :password, :database, :host
   The only optional is :port, its default value is 5432
   e.g.:
   (sql-add-postgres-db
        my-db-name ;; notice that there are no quotes here
        :port 1234
        :user \"username\"
        :host \"my-host\"
        :database \"my-db\"
        :password \"mypassword\")"
  `(let ((port (or ,(plist-get db-info :port) 5432))
         (user ,(plist-get db-info :user))
         (password ,(plist-get db-info :password))
         (host ,(plist-get db-info :host))
         (db ,(plist-get db-info :database)))
     (throw-if (any-nil? user password host db (quote ,name)) "there are info missing!")
     (let ((full-uri (+sql/format-postgres-uri host port user password db))
           (data-src-name (+sql/format-postgres-sqls host port user password db)))
       (+sql/add-to-sqls-connections "postgresql" data-src-name)
       (+sql/add-to-sql-conection-alist 'postgres ,name host port user password full-uri))))

;;;###autoload
(defmacro +sql/add-mysql-db (name &rest db-info)
  "Adds a mysql database to emacs and lsp
   This macro expects a name to the database and a p-list of parameters
   :port, :user, :password, :database, :host
   The only optional is :port, its default value is 3306
   e.g.:
   (sql-add-mysql-db
        my-db-name ;; notice that there are no quotes here
        :port 1234
        :user \"username\"
        :host \"my-host\"
        :database \"my-db\"
        :password \"mypassword\")"
  `(let ((port (or ,(plist-get db-info :port) 3306))
         (user ,(plist-get db-info :user))
         (password ,(plist-get db-info :password))
         (host ,(plist-get db-info :host))
         (db ,(plist-get db-info :database)))
     (throw-if (any-nil? user password host db (quote ,name)) "there are info missing!")
     (+sql/add-to-sqls-connections "mysql" (+sql/format-mysql-sqls host port user password db))
     (+sql/add-to-sql-conection-alist 'mysql ,name host port user password db)))


(defun =sql (connection &optional buf-name)
  "Start sql"
  (interactive
   (if sql-connection-alist
       (list (sql-read-connection "Connection: ")
             current-prefix-arg)
     (user-error "No SQL Connections defined")))
  (require 'sql)
  (when connection
    (if (modulep! :ui workspaces)
        (+workspace-switch (format "SQL: %s" connection) t)
      (delete-other-windows))
    (sql-connect connection)
    ;; (with-current-buffer-window (doom-fallback-buffer)
    ;;     (delete-window))
    (let (
          (buffer-name (format "*SQL: <%s>*" connection)))
      (persp-add-buffer buffer-name))
    ))
