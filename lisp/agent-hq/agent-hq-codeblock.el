;;; agent-hq-codeblock.el --- Edit or run an agent shell's code blocks -*- lexical-binding: t; -*-

;;; Commentary:

;; Like Org's C-c ' on a source block, for the code blocks an agent writes
;; in its shell: `agent-hq-codeblock-edit' opens a copy of the block at
;; point in a buffer of its language's major mode (```python: python-mode,
;; ```elisp: emacs-lisp-mode), in the shell's directory, so it can be
;; changed and evaluated or run there.  The shell's output is a record, so
;; edits stay in the copy; C-c ' or C-c C-k goes back to the shell.
;;
;; `agent-hq-codeblock-run' runs the block at point as it stands, or the
;; code in the region; `agent-hq-codeblock-run-line' runs the block's line
;; at point.  Both run in the shell's directory, as the agent would, with
;; the program `agent-hq-codeblock-runners' gives the block's language
;; (sh, bash, zsh, python, node, ...); its output goes to a buffer you can
;; type into.  Emacs Lisp is evaluated in this Emacs instead.
;;
;; agent-shell renders each fenced block in place: it drops the fences,
;; inserts a "LANG ⧉" label, and marks the code with the text property
;; `agent-shell-markdown-source-block-body' and the whole block with
;; `agent-shell-markdown-source-block-rendered', whose value is the
;; block's markdown, opening fence and language included.  Those are what
;; this reads, so point can be anywhere on a block.
;;
;; A language maps to a mode as Org's do (`org-src-lang-modes': bash, sh
;; and zsh are sh-mode), after `agent-hq-codeblock-languages'.  A block
;; with no language, or one Emacs has no mode for, gets the mode its #!
;; line names, if any.

;;; Code:

(require 'seq)
(require 'subr-x)

(declare-function org-src-get-lang-mode "org-src")
(defvar agent-hq-keep-point)

(defgroup agent-hq-codeblock nil
  "Edit an agent shell's code blocks in their language's mode."
  :group 'agent-hq
  :prefix "agent-hq-codeblock-")

(defcustom agent-hq-codeblock-languages
  '(("ts" . "typescript") ("console" . "sh") ("shell-session" . "sh")
    ("json" . "js-json") ("jsonc" . "js-json") ("yml" . "yaml"))
  "Fence languages read as another before they map to a mode.
For names agents write that `org-src-lang-modes' doesn't cover."
  :type '(alist :key-type string :value-type string))

(defun agent-hq-codeblock--bounds (&optional pos)
  "Return (LANG CODE-START CODE-END) for the rendered code block at POS, or nil.
POS defaults to point and can be anywhere on the block: its label,
padding or code.  LANG is \"\" when the fence names none."
  (let* ((pos (or pos (point)))
         (prop 'agent-shell-markdown-source-block-rendered)
         (source (get-text-property pos prop)))
    (when (stringp source)
      ;; Blocks flush against each other carry different sources, so
      ;; these stop at this one's edges.
      (let* ((start (previous-single-property-change (min (1+ pos) (point-max)) prop nil
                                                     (point-min)))
             (end (next-single-property-change pos prop nil (point-max)))
             (code-start (text-property-any start end
                                            'agent-shell-markdown-source-block-body t)))
        (when code-start
          (list (if (string-match "\\`[ \t>]*\\(?:```+\\|~~~+\\)[ \t]*\\([^ \t\n{]*\\)" source)
                    (match-string 1 source)
                  "")
                code-start
                (next-single-property-change code-start
                                             'agent-shell-markdown-source-block-body
                                             nil end)))))))

(defun agent-hq-codeblock-at-point (&optional pos)
  "Return (LANG . CODE) for the rendered code block at POS, or nil.
POS defaults to point and can be anywhere on the block: its label,
padding or code.  LANG is \"\" when the fence names none."
  (pcase (agent-hq-codeblock--bounds pos)
    (`(,lang ,start ,end) (cons lang (buffer-substring-no-properties start end)))))

(defun agent-hq-codeblock--mode (lang)
  "Return the major mode for code of LANG, a fence's language, or nil.
Nil when LANG is empty or Emacs has no mode for it."
  (unless (string-empty-p (string-trim lang))
    (let* ((lang (downcase (string-trim lang)))
           (lang (or (cdr (assoc lang agent-hq-codeblock-languages)) lang))
           (mode (if (require 'org-src nil t)
                     (org-src-get-lang-mode lang)
                   (intern (concat lang "-mode")))))
      (seq-find #'fboundp
                (list (if (fboundp 'major-mode-remap) (major-mode-remap mode) mode)
                      mode
                      (intern (concat lang "-ts-mode")))))))

(defvar-local agent-hq-codeblock--shell nil
  "The shell this copy of a code block came from.")
(put 'agent-hq-codeblock--shell 'permanent-local t)

(defvar-keymap agent-hq-codeblock-mode-map
  :doc "Keys of a code block's copy."
  "C-c '" #'agent-hq-codeblock-close
  "C-c C-k" #'agent-hq-codeblock-close)

;; Name the key Org's source blocks use, in hints and docstrings.
(put 'agent-hq-codeblock-close :advertised-binding (kbd "C-c '"))

(define-minor-mode agent-hq-codeblock-mode
  "A copy of a code block from an agent shell, to edit and run.
\\<agent-hq-codeblock-mode-map>\\[agent-hq-codeblock-close] kills it \
and goes back to the shell.

\\{agent-hq-codeblock-mode-map}"
  :lighter " Code"
  (when agent-hq-codeblock-mode
    (setq header-line-format
          (concat " A copy of the code from "
                  (if (buffer-live-p agent-hq-codeblock--shell)
                      (buffer-name agent-hq-codeblock--shell)
                    "a shell")
                  "    "
                  (propertize
                   (substitute-command-keys
                    "\\<agent-hq-codeblock-mode-map>\\[agent-hq-codeblock-close] back")
                   'face 'shadow)))))

;;;###autoload
(defun agent-hq-codeblock-edit (&optional pos)
  "Edit a copy of the code block at point in its language's major mode.
Like Org's \\[org-edit-special] on a source block.  POS, default point,
can be anywhere on a block an agent shell rendered.  The copy opens in
this window, in this shell's directory, so it runs where the agent's
would.  An elisp block evaluates with `lexical-binding'.
\\<agent-hq-codeblock-mode-map>\\[agent-hq-codeblock-close] goes back."
  (interactive)
  (pcase-let* ((`(,lang . ,code) (or (agent-hq-codeblock-at-point pos)
                                     (user-error "No code block at point")))
               (shell (current-buffer))
               (dir default-directory)
               (mode (agent-hq-codeblock--mode lang))
               (buffer (generate-new-buffer
                        (format "*Code: %s*" (if (string-empty-p lang) "snippet" lang)))))
    (with-current-buffer buffer
      (setq default-directory dir)
      (insert code "\n")
      (goto-char (point-min))
      ;; No mode for its language, or one that fails to start (a
      ;; tree-sitter mode without its grammar): whatever its #! line
      ;; names, if any.
      (condition-case err
          (if mode (funcall mode) (normal-mode))
        (error (message "agent-hq-codeblock: %s: %s" mode (error-message-string err))
               (normal-mode)))
      (when (derived-mode-p 'emacs-lisp-mode)
        (setq lexical-binding t))
      (setq agent-hq-codeblock--shell shell)
      (agent-hq-codeblock-mode)
      (set-buffer-modified-p nil))
    (switch-to-buffer buffer nil t)
    (message "%s" (substitute-command-keys
                   "\\<agent-hq-codeblock-mode-map>A copy: edit and run it here, \\[agent-hq-codeblock-close] to go back"))))

(defun agent-hq-codeblock-close ()
  "Kill this copy of a code block and show its shell in this window.
Point goes back to the block, not to the prompt as when you switch to
the shell (see `agent-hq-keep-point')."
  (interactive)
  (let ((copy (current-buffer))
        (shell agent-hq-codeblock--shell))
    (when (buffer-live-p shell)
      (with-current-buffer shell
        (setq-local agent-hq-keep-point (selected-window)))
      (switch-to-buffer shell nil t))
    (kill-buffer copy)))

;;; Running

(defcustom agent-hq-codeblock-runners
  (let ((python
         ;; Run as `python3 -c' does, so the shell's directory comes first
         ;; on the import path, as for the agent, not the temporary file's.
         '("python3" "-c" "import runpy, sys; sys.argv.pop(0); runpy.run_path(sys.argv[0], run_name='__main__')")))
    `(("sh" "sh") ("shell" "sh") ("bash" "bash") ("zsh" "zsh")
      ("python" ,@python) ("python3" ,@python) ("py" ,@python)
      ("js" "node") ("javascript" "node") ("node" "node")
      ("ruby" "ruby") ("rb" "ruby") ("perl" "perl")
      ("applescript" "osascript") ("osascript" "osascript")
      ("elisp" . agent-hq-codeblock-eval) ("emacs-lisp" . agent-hq-codeblock-eval)
      ;; Agents write ```lisp for Emacs Lisp too.
      ("lisp" . agent-hq-codeblock-eval)))
  "How `agent-hq-codeblock-run' runs code, by its block's language.
A list of strings is a program and its arguments: the code goes in a
temporary file, named after them, and the program runs in the shell's
directory, its output in a buffer you can type into.  A function is
called with the code, in the shell's buffer.

A block in a language not listed here runs with its #! line, if any."
  :type '(alist :key-type string
                :value-type (choice (repeat :tag "Program and arguments" string)
                                    (function :tag "Function called with the code"))))

(defcustom agent-hq-codeblock-region-language "sh"
  "The language `agent-hq-codeblock-run' runs a region outside code blocks as.
Such a region is usually a command quoted in a reply."
  :type 'string)

(defun agent-hq-codeblock--to-run (what)
  "Return (LANG . CODE) to run: WHAT is `line' or `block'.
The region's code when it's active, whatever WHAT is: only the code
of the block it starts in or reaches, if any, else its text with LANG
nil.  Otherwise the line of code at point, or the block at point."
  (cond
   ((use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (code (text-property-any beg end 'agent-shell-markdown-source-block-body t)))
      (pcase (and code (agent-hq-codeblock--bounds code))
        (`(,lang ,start ,stop)
         (cons lang (buffer-substring-no-properties (max beg start) (min end stop))))
        (_ (cons nil (buffer-substring-no-properties beg end))))))
   ((eq what 'line)
    (pcase-let ((`(,lang ,start ,stop) (or (agent-hq-codeblock--bounds)
                                          (user-error "No code block at point"))))
      (unless (<= start (point) stop)
        (user-error "Not on a line of code"))
      (cons lang (buffer-substring-no-properties (max start (line-beginning-position))
                                                 (min stop (line-end-position))))))
   ((agent-hq-codeblock-at-point))
   (t (user-error "No code block at point"))))

(defun agent-hq-codeblock--shebang (code)
  "Return the program and arguments CODE's #! line names, or nil."
  (when (string-match "\\`#![ \t]*\\([^\n]*[^ \t\n]\\)" code)
    (split-string (match-string 1 code))))

(defun agent-hq-codeblock--runner (lang code)
  "Return (LABEL . RUNNER) for CODE from a block of LANG, or nil.
LANG nil is code from outside any block.  RUNNER is an entry of
`agent-hq-codeblock-runners', else the command CODE's #! line names."
  (let ((lang (or lang agent-hq-codeblock-region-language)))
    (if-let* ((runner (cdr (assoc-string lang agent-hq-codeblock-runners t))))
        (cons (downcase lang) runner)
      (when-let* ((command (agent-hq-codeblock--shebang code)))
        (cons (concat "#!" (string-join command " ")) command)))))

(defun agent-hq-codeblock--read-language (lang)
  "Ask which language of `agent-hq-codeblock-runners' to run code as.
LANG, the code's block's, is the default when it's one of them."
  (let ((default (if (and lang (assoc-string lang agent-hq-codeblock-runners t))
                     (downcase lang)
                   agent-hq-codeblock-region-language)))
    (completing-read (format-prompt "Run as" default)
                     agent-hq-codeblock-runners nil t nil nil default)))

(defun agent-hq-codeblock-eval (code)
  "Evaluate CODE, Emacs Lisp, form by form, and show the last one's value.
In the current buffer, with `lexical-binding', as \\[eval-last-sexp] would."
  (let ((forms (with-temp-buffer
                 (insert code)
                 (goto-char (point-min))
                 (with-syntax-table emacs-lisp-mode-syntax-table
                   (let (forms)
                     (while (progn (forward-comment (buffer-size)) (not (eobp)))
                       (push (read (current-buffer)) forms))
                     (nreverse forms)))))
        value)
    (dolist (form forms)
      (setq value (eval form t)))
    (message "%s" (let ((print-length eval-expression-print-length)
                        (print-level eval-expression-print-level))
                    (prin1-to-string value)))
    value))

(defun agent-hq-codeblock--output-buffer ()
  "Return the buffer for a run's output: the last run's, unless it still runs."
  (let ((buffer (get-buffer-create "*Code output*")))
    (if (process-live-p (get-buffer-process buffer))
        (generate-new-buffer "*Code output*")
      buffer)))

(defun agent-hq-codeblock--start (label command code)
  "Run CODE with COMMAND, a program and its arguments, and show its output.
In this buffer's directory, in a comint buffer (it can ask for input,
passwords too) whose header names LABEL, the code's language.  Return
that buffer."
  (let* ((dir default-directory)
         (file (make-temp-file "agent-hq-code-" nil nil (concat code "\n")))
         (buffer (agent-hq-codeblock--output-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq default-directory dir)
      (condition-case err
          (apply #'make-comint-in-buffer (file-name-nondirectory (car command)) buffer
                 (car command) nil (append (cdr command) (list file)))
        (error (delete-file file)
               (signal (car err) (cdr err))))
      (setq header-line-format (format " %s in %s" label (abbreviate-file-name dir)))
      (add-function :after (process-sentinel (get-buffer-process buffer))
                    (lambda (process _event)
                      (unless (process-live-p process)
                        (delete-file file)))))
    (display-buffer buffer)
    buffer))

(defun agent-hq-codeblock--run (what ask)
  "Run the code WHAT names, as `agent-hq-codeblock--to-run' reads it.
ASK non-nil asks which language to run it as."
  (pcase-let ((`(,lang . ,code) (agent-hq-codeblock--to-run what)))
    (when (string-blank-p code)
      (user-error "Nothing to run"))
    (when ask
      (setq lang (agent-hq-codeblock--read-language lang)))
    (pcase-let ((`(,label . ,runner)
                 (or (agent-hq-codeblock--runner lang code)
                     (user-error "Don't know how to run %s: %s first to pick a language"
                                 (if (member lang '(nil ""))
                                     "a block with no language"
                                   (format "%s code" lang))
                                 (substitute-command-keys "\\[universal-argument]")))))
      (deactivate-mark)
      (if (functionp runner)
          (funcall runner code)
        (agent-hq-codeblock--start label runner code)))))

;;;###autoload
(defun agent-hq-codeblock-run (&optional ask)
  "Run the code block at point, or the code in the region.
Point can be anywhere on the block.  In a region, only code blocks'
text runs, if it has any; a region outside them runs as
`agent-hq-codeblock-region-language' (sh), as a quoted command would.

The code runs in this shell's directory, as the agent's would, with
the program `agent-hq-codeblock-runners' names for its language, its
output in a buffer of its own; Emacs Lisp is evaluated here.  With a
prefix argument ASK, ask which language to run it as."
  (interactive "P")
  (agent-hq-codeblock--run 'block ask))

;;;###autoload
(defun agent-hq-codeblock-run-line (&optional ask)
  "Run the line of the code block at point, or the code in the region.
Otherwise as `agent-hq-codeblock-run', which see; with a prefix
argument ASK, ask which language to run it as."
  (interactive "P")
  (agent-hq-codeblock--run 'line ask))

(provide 'agent-hq-codeblock)
;;; agent-hq-codeblock.el ends here
