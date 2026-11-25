;;; agent-hq-codeblock.el --- Edit an agent shell's code block in its language's mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Like Org's C-c ' on a source block, for the code blocks an agent writes
;; in its shell: `agent-hq-codeblock-edit' opens a copy of the block at
;; point in a buffer of its language's major mode (```python: python-mode,
;; ```elisp: emacs-lisp-mode), in the shell's directory, so it can be
;; changed and evaluated or run there.  The shell's output is a record, so
;; edits stay in the copy; C-c ' or C-c C-k goes back to the shell.
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

(defun agent-hq-codeblock-at-point (&optional pos)
  "Return (LANG . CODE) for the rendered code block at POS, or nil.
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
          (cons (if (string-match "\\`[ \t>]*\\(?:```+\\|~~~+\\)[ \t]*\\([^ \t\n{]*\\)" source)
                    (match-string 1 source)
                  "")
                (buffer-substring-no-properties
                 code-start
                 (next-single-property-change code-start
                                              'agent-shell-markdown-source-block-body
                                              nil end))))))))

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
  "Kill this copy of a code block and show its shell in this window."
  (interactive)
  (let ((copy (current-buffer))
        (shell agent-hq-codeblock--shell))
    (when (buffer-live-p shell)
      (switch-to-buffer shell nil t))
    (kill-buffer copy)))

(provide 'agent-hq-codeblock)
;;; agent-hq-codeblock.el ends here
