;;; agent-hq-codeblock-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-codeblock-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: the blocks are written with the text
;; properties its markdown renderer leaves on them.

(require 'ert)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq-codeblock.el" dir) nil t))

(defun agent-hq-codeblock-test--block (lang code)
  "Return a code block of LANG as agent-shell renders it: padding, label, CODE."
  (let ((source (format "```%s\n%s\n```\n" lang code)))
    (propertize (concat "\n" (if (string-empty-p lang) "snippet" lang) " ⧉\n\n"
                        (propertize code 'agent-shell-markdown-source-block-body t)
                        "\n\n")
                'agent-shell-markdown-source-block-rendered source)))

(defmacro agent-hq-codeblock-test--with-shell (&rest body)
  "Run BODY in a shell-like buffer showing an elisp block, then a python one.
Flush against each other, as two blocks with no text between them render."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "Reload it:\n"
             (agent-hq-codeblock-test--block
              "elisp" "(progn (load \"x.el\")\n       (mapc #'ignore (list)))")
             (agent-hq-codeblock-test--block "python" "print(1)")
             "Done.\n")
     ,@body))

(defun agent-hq-codeblock-test--goto (text)
  "Move to the start of TEXT in this buffer."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(ert-deftest agent-hq-codeblock-test-at-point ()
  (agent-hq-codeblock-test--with-shell
    (let ((elisp '("elisp" . "(progn (load \"x.el\")\n       (mapc #'ignore (list)))")))
      ;; Anywhere on the block: its code, its label, its padding.
      (agent-hq-codeblock-test--goto "(mapc")
      (should (equal (agent-hq-codeblock-at-point) elisp))
      (agent-hq-codeblock-test--goto "elisp ⧉")
      (should (equal (agent-hq-codeblock-at-point) elisp))
      (should (equal (agent-hq-codeblock-at-point (1- (point))) elisp))
      ;; The next block, flush against it, is its own.
      (agent-hq-codeblock-test--goto "print")
      (should (equal (agent-hq-codeblock-at-point) '("python" . "print(1)")))
      (agent-hq-codeblock-test--goto "python ⧉")
      (should (equal (agent-hq-codeblock-at-point) '("python" . "print(1)")))
      ;; Off any block.
      (agent-hq-codeblock-test--goto "Reload")
      (should-not (agent-hq-codeblock-at-point))
      (agent-hq-codeblock-test--goto "Done")
      (should-not (agent-hq-codeblock-at-point))
      (should-not (agent-hq-codeblock-at-point (point-max))))))

(ert-deftest agent-hq-codeblock-test-mode ()
  (should (eq (agent-hq-codeblock--mode "elisp") 'emacs-lisp-mode))
  (should (eq (agent-hq-codeblock--mode "Emacs-Lisp") 'emacs-lisp-mode))
  (should (eq (agent-hq-codeblock--mode "python") 'python-mode))
  ;; As Org has them.
  (should (eq (agent-hq-codeblock--mode "bash") 'sh-mode))
  (should (eq (agent-hq-codeblock--mode "zsh") 'sh-mode))
  ;; Names Org doesn't cover.
  (should (eq (agent-hq-codeblock--mode "console") 'sh-mode))
  (should (eq (agent-hq-codeblock--mode "json") 'js-json-mode))
  (should-not (agent-hq-codeblock--mode ""))
  (should-not (agent-hq-codeblock--mode "no-such-language")))

(ert-deftest agent-hq-codeblock-test-edit ()
  (let ((window (selected-window)))
    (agent-hq-codeblock-test--with-shell
      (let ((shell (current-buffer))
            (default-directory temporary-file-directory)
            copy)
        (set-window-buffer window shell)
        (agent-hq-codeblock-test--goto "(mapc")
        (unwind-protect
            (progn
              (agent-hq-codeblock-edit)
              (setq copy (current-buffer))
              ;; In this window, in the block's mode and the shell's directory.
              (should (eq (window-buffer window) copy))
              (should (equal (buffer-name copy) "*Code: elisp*"))
              (should (eq major-mode 'emacs-lisp-mode))
              (should agent-hq-codeblock-mode)
              (should lexical-binding)
              (should (equal default-directory temporary-file-directory))
              (should (equal (buffer-string)
                             "(progn (load \"x.el\")\n       (mapc #'ignore (list)))\n"))
              (should-not (buffer-modified-p))
              (should (eq (key-binding (kbd "C-c '")) #'agent-hq-codeblock-close))
              ;; Back: the shell again, the copy gone.
              (agent-hq-codeblock-close)
              (should (eq (window-buffer window) shell))
              (should-not (buffer-live-p copy)))
          (when (buffer-live-p copy)
            (kill-buffer copy)))))))

(ert-deftest agent-hq-codeblock-test-edit-guesses-from-shebang ()
  "A block with no language gets the mode its #! line names."
  (with-temp-buffer
    (insert (agent-hq-codeblock-test--block "" "#!/usr/bin/env python3\nprint(1)"))
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let (copy)
      (unwind-protect
          (progn
            (agent-hq-codeblock-edit)
            (setq copy (current-buffer))
            (should (equal (buffer-name copy) "*Code: snippet*"))
            (should (eq major-mode 'python-mode)))
        (when (buffer-live-p copy)
          (kill-buffer copy))))))

(ert-deftest agent-hq-codeblock-test-edit-off-a-block ()
  (agent-hq-codeblock-test--with-shell
    (agent-hq-codeblock-test--goto "Reload")
    (should-error (agent-hq-codeblock-edit) :type 'user-error)))
