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
              (should-not (buffer-live-p copy))
              ;; At the block still: not sent to the prompt when shown.
              (should (eq (buffer-local-value 'agent-hq-keep-point shell) window)))
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

;;; Running

(defmacro agent-hq-codeblock-test--with-blocks (blocks &rest body)
  "Run BODY in a shell-like buffer showing BLOCKS, (LANG CODE) pairs, after a line of prose."
  (declare (indent 1))
  `(with-temp-buffer
     (insert "Run `echo prose` now:\n")
     (dolist (block ,blocks)
       (insert (apply #'agent-hq-codeblock-test--block block)))
     (insert "Done.\n")
     ,@body))

(defun agent-hq-codeblock-test--wait (buffer)
  "Wait up to 10 s for BUFFER's process to end, and its sentinel to run."
  (let ((process (get-buffer-process buffer))
        (deadline (+ (float-time) 10)))
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output process 0.1))
    (accept-process-output nil 0.1)
    process))

(ert-deftest agent-hq-codeblock-test-to-run ()
  (agent-hq-codeblock-test--with-blocks '(("sh" "echo one\necho two") ("python" "print(3)"))
    ;; Anywhere on a block: all of it.
    (agent-hq-codeblock-test--goto "two")
    (should (equal (agent-hq-codeblock--to-run 'block) '("sh" . "echo one\necho two")))
    (agent-hq-codeblock-test--goto "sh ⧉")
    (should (equal (agent-hq-codeblock--to-run 'block) '("sh" . "echo one\necho two")))
    ;; Its line at point, only on its code.
    (agent-hq-codeblock-test--goto "two")
    (should (equal (agent-hq-codeblock--to-run 'line) '("sh" . "echo two")))
    (agent-hq-codeblock-test--goto "print")
    (end-of-line)
    (should (equal (agent-hq-codeblock--to-run 'line) '("python" . "print(3)")))
    (agent-hq-codeblock-test--goto "sh ⧉")
    (should-error (agent-hq-codeblock--to-run 'line) :type 'user-error)
    ;; Off any block.
    (agent-hq-codeblock-test--goto "Done")
    (should-error (agent-hq-codeblock--to-run 'block) :type 'user-error)
    (should-error (agent-hq-codeblock--to-run 'line) :type 'user-error)
    (let ((transient-mark-mode t))
      ;; A region on a block: its code there, never the label.
      (agent-hq-codeblock-test--goto "sh ⧉")
      (push-mark (point) t t)
      (agent-hq-codeblock-test--goto "two")
      (forward-char 2)
      (should (equal (agent-hq-codeblock--to-run 'block) '("sh" . "echo one\necho tw")))
      (should (equal (agent-hq-codeblock--to-run 'line) '("sh" . "echo one\necho tw")))
      ;; One outside any block: its text, in no language.
      (agent-hq-codeblock-test--goto "echo prose")
      (push-mark (point) t t)
      (forward-char (length "echo prose"))
      (should (equal (agent-hq-codeblock--to-run 'block) '(nil . "echo prose"))))))

(ert-deftest agent-hq-codeblock-test-runner ()
  (should (equal (agent-hq-codeblock--runner "bash" "ls") '("bash" "bash")))
  (should (equal (agent-hq-codeblock--runner "ZSH" "ls") '("zsh" "zsh")))
  (should (equal (car (agent-hq-codeblock--runner "py" "1")) "py"))
  (should (equal (cadr (agent-hq-codeblock--runner "python" "1")) "python3"))
  (should (eq (cdr (agent-hq-codeblock--runner "elisp" "1")) #'agent-hq-codeblock-eval))
  ;; Outside any block: sh.
  (should (equal (agent-hq-codeblock--runner nil "ls") '("sh" "sh")))
  ;; A language with no runner, or none: its #! line, if any.
  (should (equal (agent-hq-codeblock--runner "" "#!/usr/bin/env -S ruby -w \nputs 1")
                 '("#!/usr/bin/env -S ruby -w" "/usr/bin/env" "-S" "ruby" "-w")))
  (should (equal (cdr (agent-hq-codeblock--runner "text" "#!/bin/zsh\nls"))
                 '("/bin/zsh")))
  (should-not (agent-hq-codeblock--runner "" "a -> b"))
  (should-not (agent-hq-codeblock--runner "diff" "-a\n+b")))

(ert-deftest agent-hq-codeblock-test-eval ()
  (agent-hq-codeblock-test--with-blocks
      '(("elisp" ";; Set it, then read it.\n(setq-local agent-hq-codeblock-test--x 1)\n(let ((f (lambda () agent-hq-codeblock-test--x)))\n  (funcall f)) ; last"))
    (let ((shell (current-buffer)))
      (agent-hq-codeblock-test--goto "setq-local")
      (should (equal (agent-hq-codeblock-run) 1))
      ;; In the shell's buffer, as C-x C-e there would.
      (should (local-variable-p 'agent-hq-codeblock-test--x shell))
      ;; A line on its own.
      (should (equal (agent-hq-codeblock-run-line) 1)))))

(ert-deftest agent-hq-codeblock-test-run-errors ()
  (agent-hq-codeblock-test--with-blocks '(("" "a -> b\n\nc") ("diff" "-a\n+b"))
    (agent-hq-codeblock-test--goto "a ->")
    (should-error (agent-hq-codeblock-run) :type 'user-error)
    (forward-line 1)
    (should-error (agent-hq-codeblock-run-line) :type 'user-error)
    (agent-hq-codeblock-test--goto "+b")
    (should-error (agent-hq-codeblock-run) :type 'user-error)))

(ert-deftest agent-hq-codeblock-test-run-program ()
  (let* ((dir (file-name-as-directory (make-temp-file "agent-hq-codeblock-dir" t)))
         (default-directory dir)
         output)
    (unwind-protect
        (agent-hq-codeblock-test--with-blocks
            '(("sh" "echo \"one $((1 + 1))\"\npwd -P")
              ("python" "import sys\nprint(repr(sys.path[0]), __name__)"))
          (agent-hq-codeblock-test--goto "pwd")
          (setq output (agent-hq-codeblock-run))
          (should (equal (buffer-name output) "*Code output*"))
          (let* ((process (agent-hq-codeblock-test--wait output))
                 (file (car (last (process-command process)))))
            (should (eq (process-status process) 'exit))
            (should (zerop (process-exit-status process)))
            (should-not (file-exists-p file)))
          (with-current-buffer output
            (should (derived-mode-p 'comint-mode))
            (should (equal default-directory dir))
            (should (string-match-p "^one 2\r?$" (buffer-string)))
            (should (string-match-p (regexp-quote (directory-file-name (file-truename dir))) (buffer-string)))
            (should (string-match-p "^ sh in " header-line-format)))
          ;; The next run reuses the buffer, emptied.
          (agent-hq-codeblock-test--goto "repr")
          (when (executable-find "python3")
            (should (eq (agent-hq-codeblock-run) output))
            (agent-hq-codeblock-test--wait output)
            (with-current-buffer output
              (should-not (string-match-p "one 2" (buffer-string)))
              ;; The shell's directory is first on the import path.
              (should (string-match-p "^'' __main__\r?$" (buffer-string))))))
      (when (buffer-live-p output)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer output)))
      (delete-directory dir t))))
