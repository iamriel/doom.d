;;; tools/kubel/autoload.el -*- lexical-binding: t; -*-

(defvar +kubel-workspace-name "*kubel*"
  "TODO")
(defvar +kubel--old-wconf nil)

(add-hook 'kubel-mode-hook #'+kubel-init-h)

;;;###autoload
(defun +kubel/exec-tty (process-name args)
  "Utility function to run commands in the proper context and namespace.
PROCESS-NAME is an identifier for the process.  Default to \"kubel-command\".
ARGS is a ist of arguments.
READONLY If true buffer will be in readonly mode(view-mode)."
  (when (equal process-name "")
    (setq process-name "kubel-command"))
  (let (
        (buffername (format "*%s*" process-name))
        (term-buffer (process-buffer (get-buffer-process (term "/usr/bin/zsh"))))
        ;; (buffername (buffer-name (process-buffer (get-process (term "/usr/bin/zsh")))))
        (error-buffer (kubel--process-error-buffer process-name))
        (cmd (append (list "kubectl") (kubel--get-context-namespace) args)))
    (when (get-buffer buffername)
      (kill-buffer buffername))
    (when (get-buffer error-buffer)
      (kill-buffer error-buffer))
    (with-current-buffer term-buffer
      (rename-buffer buffername))
    (kubel--log-command process-name cmd)
    (comint-send-string
     (get-buffer-process buffername)
     (mapconcat 'identity cmd " "))
    (display-buffer buffername '(display-buffer--maybe-same-window . ()))))


;;;###autoload
(defun +kubel/exec-bash ()
  "Exec into the pod with bash."
  (interactive)
  (let* ((pod (if (kubel--is-pod-view)
                  (kubel--get-resource-under-cursor)
                (kubel--select-resource "Pods")))
         (containers (kubel--get-containers pod))
         (container (if (equal (length containers) 1)
                        (car containers)
                      (completing-read "Select container: " containers)))
         (process-name (format "kubel - bash - %s - %s" pod container)))
    (+kubel/exec-tty process-name
                     (append '("exec") '("-ti") (list pod container "--" "bash" "\n")))))


;;;###autoload
(defun =kubel (&optional namespace)
  "Start kubectl."
  (interactive)
  (require 'kubel)
  (if namespace
      (setq kubel-namespace namespace))
  (if (modulep! :ui workspaces)
      (+workspace-switch (format "*kubel %s" kubel-namespace) t)
    (setq +kubel--old-wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer)))
  (kubel))

;;
;; Hooks

(defun +kubel-init-h ()
  (add-hook 'quit-window-hook #'+kubel-kill-kubel-h nil t))

(defun +kubel-kill-kubel-h ()
  (cond
   ((and (modulep! :ui workspaces) (+workspace-exists-p +kubel-workspace-name))
    (+workspace/delete +kubel-workspace-name))

   (+kubel--old-wconf
    (set-window-configuration +kubel--old-wconf)
    (setq +kubel--old-wconf nil))))

(after! kubel
  (transient-define-prefix kubel-exec-popup ()
    "Kubel Exec Menu"
    ["Actions"
     ("b" "Bash" +kubel/exec-bash)
     ("d" "Dired" kubel-exec-pod)
     ("e" "Eshell" kubel-exec-eshell-pod)
     ("s" "Shell" kubel-exec-shell-pod)])
  (transient-replace-suffix 'kubel-help-popup 'kubel '("r" "Refresh" =kubel)))
