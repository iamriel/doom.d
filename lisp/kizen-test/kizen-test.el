;;; kizen-test.el --- helpers to run kizen tests -*- lexical-binding: t; -*-

;; Author: Rieljun Liguid <me@iamriel.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4") (dash "2.18.0") (transient "20200719") (projectile "0.14.0") (s "1.12.0"))
;; Keywords: test, python, languages, processes, tools
;;
;; This file is not part of GNU Emacs.

;;; License:

;; 3-clause "new bsd"; see readme for details.

;;; Commentary:

;; This package provides helpers to run kizen-test. See README for details.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'compile)
(require 'python)

(require 'dash)
(require 'transient)
(require 'projectile)
(require 's)

(defvar which-key-inhibit)              ; defined in which-key.el

(defgroup kizen-test nil
  "kizen-test integration"
  :group 'python
  :prefix "kizen-test-")

(defcustom kizen-test-confirm nil
  "Whether to edit the command in the minibuffer before execution.
By default, kizen-test will be executed without showing a minibuffer prompt.
This can be changed on a case by case basis by using a prefix argument
\(\\[universal-argument]\) when invoking a command.
When t, this toggles the behaviour of the prefix argument."
  :group 'kizen-test
  :type 'boolean)

(defcustom kizen-test-executable "ktest"
  "The name of the kizen-test executable."
  :group 'kizen-test
  :type 'string)

(defcustom kizen-test-setup-hook nil
  "Hooks to run before a kizen-test process starts."
  :group 'kizen-test
  :type 'hook)

(defcustom kizen-test-started-hook nil
  "Hooks to run after a kizen-test process starts."
  :group 'kizen-test
  :type 'hook)

(defcustom kizen-test-finished-hook nil
  "Hooks to run after a kizen-test process finishes."
  :group 'kizen-test
  :type 'hook)

(defcustom kizen-test-buffer-name "*kizen-test*"
  "Name of the kizen-test output buffer."
  :group 'kizen-test
  :type 'string)

(defcustom kizen-test-project-name-in-buffer-name t
  "Whether to include the project name in the buffer name.
This is useful when working on multiple projects simultaneously."
  :group 'kizen-test
  :type 'boolean)

(defcustom kizen-test-pdb-track t
  "Whether to automatically track output when pdb is spawned.
This results in automatically opening source files during debugging."
  :group 'kizen-test
  :type 'boolean)

(defcustom kizen-test-strict-test-name-matching nil
  "Whether to require a strict match for the ‘test this function’ heuristic.
This influences the ‘test this function’ behaviour when editing a
non-test function, e.g. ‘foo()’.
When nil (the default), the current function name will be used as
a pattern to run the corresponding tests, which will match
‘test_foo()’ as well as ‘test_foo_xyz()’.
When non-nil only ‘test_foo()’ will match, and nothing else."
  :group 'kizen-test
  :type 'boolean)

(defcustom kizen-test-unsaved-buffers-behavior 'ask-all
  "Whether to ask whether unsaved buffers should be saved before running kizen-test."
  :group 'kizen-test
  :type '(choice (const :tag "Ask for all project buffers" ask-all)
          (const :tag "Ask for current buffer" ask-current)
          (const :tag "Save all project buffers" save-all)
          (const :tag "Save current buffer" save-current)
          (const :tag "Ignore" nil)))

(defvar kizen-test--history nil
  "History for kizen-test invocations.")

(defvar kizen-test--project-last-command (make-hash-table :test 'equal)
  "Last executed command lines, per project.")

(defvar-local kizen-test--current-command nil
  "Current command; used in kizen-test-mode buffers.")

;;;###autoload (autoload 'kizen-test-dispatch "kizen-test" nil t)
(transient-define-prefix kizen-test-dispatch ()
  "Show popup for running kizen-test."
  :man-page "ktest"
  :incompatible '(("--exitfirst" "--maxfail="))
  :value '("--color")
  ["Output"
   [("-c" "color" "--color")
    ("-q" "quiet" "--quiet")
    ("-s" "no output capture" "--capture=no")
    (kizen-test:-v)]]
  ["Selection, filtering, ordering"
   [(kizen-test:-k)
    (kizen-test:-m)
    "                                          "] ;; visual alignment
   [("--dm" "run doctests" "--doctest-modules")
    ("--nf" "new first" "--new-first")
    ("--sw" "stepwise" "--stepwise")]]
  ["Failures, errors, debugging"
   [("-l" "show locals" "--showlocals")
    ("-p" "debug on error" "--pdb")
    ("-x" "exit after first failure" "--exitfirst")]
   [("--ff" "failed first" "--failed-first")
    ("--ft" "full tracebacks" "--full-trace")
    ("--mf" "exit after N failures or errors" "--maxfail=")
    ("--rx" "run xfail tests" "--runxfail")
    (kizen-test:--tb)
    ("--tr" "debug on each test" "--trace")]]
  ["Run tests"
   [("t" "all" kizen-test)]
   [("r" "repeat" kizen-test-repeat)
    ("x" "last failed" kizen-test-last-failed)]
   [("f" "file (dwim)" kizen-test-file-dwim)
    ("F" "file (this)" kizen-test-file)]
   [("m" "files" kizen-test-files)
    ("M" "directories" kizen-test-directories)]
   [("d" "def/class (dwim)" kizen-test-function-dwim)
    ("D" "def/class (this)" kizen-test-function)]])

(define-obsolete-function-alias 'kizen-test-popup 'kizen-test-dispatch "2.0.0")

;;;###autoload
(defun kizen-test (&optional args)
  "Run kizen-test with ARGS.
With a prefix argument, allow editing."
  (interactive (list (transient-args 'kizen-test-dispatch)))
  (kizen-test--run
   :args args
   :edit current-prefix-arg))

;;;###autoload
(defun kizen-test-file (file &optional args)
  "Run kizen-test on FILE, using ARGS.
Additional ARGS are passed along to kizen-test.
With a prefix argument, allow editing."
  (interactive
   (list
    (buffer-file-name)
    (transient-args 'kizen-test-dispatch)))
  (kizen-test--run
   :args args
   :file file
   :edit current-prefix-arg))

;;;###autoload
(defun kizen-test-file-dwim (file &optional args)
  "Run kizen-test on FILE, intelligently finding associated test modules.
When run interactively, this tries to work sensibly using
the current file.
Additional ARGS are passed along to kizen-test.
With a prefix argument, allow editing."
  (interactive
   (list
    (buffer-file-name)
    (transient-args 'kizen-test-dispatch)))
  (kizen-test-file (kizen-test--sensible-test-file file) args))

;;;###autoload
(defun kizen-test-files (files &optional args)
  "Run kizen-test on FILES, using ARGS.
When run interactively, this allows for interactive file selection.
Additional ARGS are passed along to kizen-test.
With a prefix argument, allow editing."
  (interactive
   (list
    (kizen-test--select-test-files :type 'file)
    (transient-args 'kizen-test-dispatch)))
  (setq args (-concat args (-map 'kizen-test--shell-quote files)))
  (kizen-test--run
   :args args
   :edit current-prefix-arg))

;;;###autoload
(defun kizen-test-directories (directories &optional args)
  "Run kizen-test on DIRECTORIES, using ARGS.
When run interactively, this allows for interactive directory selection.
Additional ARGS are passed along to kizen-test.
With a prefix argument, allow editing."
  (interactive
   (list
    (kizen-test--select-test-files :type 'directory)
    (transient-args 'kizen-test-dispatch)))
  (setq args (-concat args (-map 'kizen-test--shell-quote directories)))
  (kizen-test--run
   :args args
   :edit current-prefix-arg))

;;;###autoload
(defun kizen-test-function (file func args &optional pinfo)
  "Run kizen-test on FILE with FUNC (or class).
Additional ARGS are passed along to kizen-test.
When the method at point is parameterized, PINFO (its element list or the
symbol `unknown') triggers a variant-selection prompt and a `-k' filter.
With a prefix argument, allow editing."
  (interactive
   (list
    (buffer-file-name)
    (kizen-test--current-defun)
    (transient-args 'kizen-test-dispatch)
    (kizen-test--parameterized-elements)))
  (if pinfo
      (kizen-test--run-parameterized file func args pinfo)
    (kizen-test--run
     :args args
     :file file
     :func func
     :edit current-prefix-arg)))

;;;autoload
(defun kizen-test-yank-function (file func)
  "Copy function path."
  (interactive
   (list
    (buffer-file-name)
    (kizen-test--current-defun)
    ))
  (when (and file (file-name-absolute-p file))
    (setq file (s-replace "/" "." (kizen-test--relative-file-name file)))
    (setq file (s-replace ".py" "" file))
    (setq file (format "%s.%s" file func))
    (kill-new file)
    ))

;;;###autoload
(defun kizen-test-function-dwim (file func args &optional pinfo)
  "Run kizen-test on FILE with FUNC (or class).
When run interactively, this tries to work sensibly using
the current file and function around point.
Additional ARGS are passed along to kizen-test.
When FILE is a test file and the method at point is parameterized, PINFO
\(its element list or the symbol `unknown') triggers a variant-selection
prompt and a `-k' filter.
With a prefix argument, allow editing."
  (interactive
   (list
    (buffer-file-name)
    (kizen-test--current-defun)
    (transient-args 'kizen-test-dispatch)
    (kizen-test--parameterized-elements)))
  (if (and pinfo (kizen-test--test-file-p file))
      (kizen-test--run-parameterized file func args pinfo)
    (progn
      (unless (kizen-test--test-file-p file)
        (setq
         file (kizen-test--sensible-test-file file)
         func (kizen-test--make-test-name func))
        (unless kizen-test-strict-test-name-matching
          (let ((k-option (-first (-partial #'s-prefix-p "-k") args)))
            (when k-option
              ;; try to use the existing ‘-k’ option in a sensible way
              (setq args (-remove-item k-option args)
                    k-option (-->
                              k-option
                              (s-chop-prefix "-k" it)
                              (s-trim it)
                              (if (s-contains-p " " it) (format "(%s)" it) it))))
            (setq args (-snoc
                        args
                        (kizen-test--shell-quote file)
                        (if k-option
                            (format "--keepdb --no-migrations %s and %s" func k-option)
                          (format "--keepdb --no-migrations %s" func)))
                  file nil
                  func nil))))
      (kizen-test--run
       :args args
       :file file
       :func func
       :edit current-prefix-arg))))

;;;###autoload
(defun kizen-test-last-failed (&optional args)
  "Run kizen-test, only executing previous test failures.
Additional ARGS are passed along to kizen-test.
With a prefix argument, allow editing."
  (interactive (list (transient-args 'kizen-test-dispatch)))
  (kizen-test--run
   :args (-snoc args "--rerun")
   :edit current-prefix-arg))

;;;###autoload
(defun kizen-test-repeat ()
  "Run kizen-test with the same argument as the most recent invocation.
With a prefix ARG, allow editing."
  (interactive)
  (let ((command (gethash
                  (kizen-test--project-root)
                  kizen-test--project-last-command)))
    (when kizen-test--current-command
      ;; existing kizen-test-mode buffer; reuse command
      (setq command kizen-test--current-command))
    (unless command
      (user-error "No previous kizen-test run for this project"))
    (kizen-test--run-command
     :command command
     :edit current-prefix-arg)))


;; internal helpers

(define-derived-mode kizen-test-mode
  comint-mode "kizen-test"
  "Major mode for kizen-test sessions (derived from comint-mode)."
  (compilation-setup)
  ;; Keep Emacs responsive while large/long test output streams in.
  ;; Django's --force-color output produces very long, escape-laden lines
  ;; (logging, SQL, tracebacks).  Wrapping and bidirectional reordering of
  ;; such lines makes redisplay block all input for the whole test run, so
  ;; truncate them and disable the expensive bidi paragraph analysis.
  (setq-local truncate-lines t)
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local bidi-inhibit-bpa t)
  ;; Render the ANSI colors from --force-color (and strip the escape codes,
  ;; which would otherwise bloat line length) instead of leaving raw escapes.
  (require 'ansi-color)
  (setq-local ansi-color-for-comint-mode t)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output nil t))

(cl-defun kizen-test--run (&key args file func edit)
  "Run kizen-test for the given arguments."
  (setq args (kizen-test--transform-arguments args))
  (when (and file (file-name-absolute-p file))
    (setq file (s-replace "/" "." (kizen-test--relative-file-name file)))
    (setq file (s-replace ".py" "" file))
    )
  ;; (when func
  ;;   (setq func (s-replace "." "::" func)))
  (let ((command)
        (thing (cond
                ((and file func) (format "%s.%s" file func))
                (file file))))
    (when thing
      (setq args (-snoc args (kizen-test--shell-quote thing))))
    (setq args (cons kizen-test-executable args)
          command (s-join " " args))
    (kizen-test--run-command
     :command command
     :edit edit)))

(cl-defun kizen-test--run-command (&key command edit)
  "Run a kizen-test command line."
  (kizen-test--maybe-save-buffers)
  (let* ((default-directory (kizen-test--webapp-root)))
    (when kizen-test-confirm
      (setq edit (not edit)))
    (when edit
      (setq command
            (read-from-minibuffer
             "Command: "
             command nil nil 'kizen-test--history)))
    (add-to-history 'kizen-test--history command)
    (setq kizen-test--history (-uniq kizen-test--history))
    (puthash (kizen-test--project-root) command
             kizen-test--project-last-command)
    (kizen-test--run-as-comint :command command)))

(cl-defun kizen-test--run-as-comint (&key command)
  "Run a kizen-test comint session for COMMAND."
  (let* ((buffer (kizen-test--get-buffer))
         (process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (when (comint-check-proc buffer)
        (unless (or compilation-always-kill
                    (yes-or-no-p "Kill running kizen-test process?"))
          (user-error "Aborting; kizen-test still running")))
      (when process
        (delete-process process))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (unless (eq major-mode 'kizen-test-mode)
        (kizen-test-mode))
      (compilation-forget-errors)
      (insert (format "cwd: %s\ncmd: %s\n\n" default-directory command))
      (setq kizen-test--current-command command
            python-shell-interpreter-args "--simple-prompt -i")
      ;; `python-pdbtrack-comint-output-filter-function' re-reads the whole
      ;; output region (comint-last-input-end .. point-max) and runs
      ;; `ansi-color-filter-apply' over it on EVERY output chunk.  With no
      ;; user input during a test run that region is the entire buffer, so
      ;; the cost grows with each chunk -- O(n^2) over the run -- and freezes
      ;; Emacs as output accumulates (dramatically worse with --force-color).
      ;; pdb tracking only matters when a debugger is actually spawned, where
      ;; output is small and interactive, so only enable it for such runs.
      (when (and kizen-test-pdb-track
                 (string-match-p "--pdb\\|--trace" command))
        (add-hook
         'comint-output-filter-functions
         'python-pdbtrack-comint-output-filter-function
         nil t))
      (run-hooks 'kizen-test-setup-hook)
      (make-comint-in-buffer "kizen-test" buffer "/bin/zsh" nil "-c" command)
      (run-hooks 'kizen-test-started-hook)
      (setq process (get-buffer-process buffer))
      (set-process-sentinel process #'kizen-test--process-sentinel)
      ;; While the test runs asynchronously Emacs sits idle waiting for
      ;; output, which lets which-key's *repeating* idle timer fire
      ;; continuously and rebuild its popup (`which-key--create-buffer-and-show'
      ;; -- a redisplay + GC storm that freezes Emacs).  Inhibit which-key for
      ;; the duration of the run; `kizen-test--process-sentinel' clears it when
      ;; the process exits.
      (setq which-key-inhibit t)
      (display-buffer buffer))))

(defun kizen-test--shell-quote (s)
  "Quote S for use in a shell command. Like `shell-quote-argument', but prettier."
  (if (s-equals-p s (shell-quote-argument s))
      s
    (format "'%s'" (s-replace "'" "'\"'\"'" s))))

(defun kizen-test--get-buffer ()
  "Get a create a suitable compilation buffer."
  (if (eq major-mode 'kizen-test-mode)
      (current-buffer)  ;; re-use buffer
    (let ((name kizen-test-buffer-name))
      (when kizen-test-project-name-in-buffer-name
        (setq name (format "%s<%s>" name (kizen-test--project-name))))
      (get-buffer-create name))))

(defun kizen-test--process-sentinel (proc _state)
  "Process sentinel helper to run hooks after PROC finishes."
  ;; Re-enable which-key once the run is over (see `kizen-test--run-as-comint').
  (unless (process-live-p proc)
    (setq which-key-inhibit nil))
  (with-current-buffer (process-buffer proc)
    (run-hooks 'kizen-test-finished-hook)))

(defun kizen-test--transform-arguments (args)
  "Transform ARGS so that kizen-test understands them."
  (-->
   args
   (kizen-test--switch-to-option it "--color" "--force-color" "--color=no")))

(defun kizen-test--switch-to-option (args name on-replacement off-replacement)
  "Look in ARGS for switch NAME and turn it into option with a value.
When present ON-REPLACEMENT is substituted, else OFF-REPLACEMENT is appended."
  (if (-contains-p args name)
      (-replace name on-replacement args)
    (-snoc args off-replacement)))

(defun kizen-test--quote-string-option (args option)
  "Quote all values in ARGS with the prefix OPTION as shell strings."
  (--map-when
   (s-prefix-p option it)
   (let ((s it))
     (--> s
          (substring it (length option))
          (s-trim it)
          (kizen-test--shell-quote it)
          (format "%s %s" option it)))
   args))

(defun kizen-test--read-quoted-argument-for-short-flag (prompt initial-input history)
  "Read a quoted string for use as a argument after a short-form command line flag."
  (let* ((input (read-from-minibuffer prompt initial-input nil nil history))
         (quoted-input (kizen-test--shell-quote input))
         (formatted-input (format " %s" quoted-input)))
    formatted-input))

(transient-define-argument kizen-test:-k ()
  :description "only names matching expression"
  :class 'transient-option
  :argument "--keepdb"
  :allow-empty nil
  :key "-k"
  :reader 'kizen-test--read-quoted-argument-for-short-flag)

(transient-define-argument kizen-test:-m ()
  :description "only marks matching expression"
  :class 'transient-option
  :argument "-m"
  :allow-empty nil
  :key "-m"
  :reader 'kizen-test--read-quoted-argument-for-short-flag)

(transient-define-argument kizen-test:-v ()
  :description "verbosity"
  :class 'transient-switches
  :key "-v"
  :argument-format "%s"
  :argument-regexp "\\(--verbose\\|--verbose --verbose\\)"
  :choices '("--verbose" "--verbose --verbose"))

(transient-define-argument kizen-test:--tb ()
  :description "traceback style"
  :class 'transient-option
  :key "--tb"
  :argument "--tb="
  :choices '("long" "short" "line" "native" "no"))


;; python helpers

(defun kizen-test--current-defun ()
  "Detect the current function/class (if any)."
  (let* ((name
          (or (python-info-current-defun)
              (save-excursion
                ;; As a fallback, jumping seems to make it work on empty lines.
                (python-nav-beginning-of-defun)
                (python-nav-forward-statement)
                (python-info-current-defun))
              (user-error "No class/function found")))
         (name
          ;; Keep at most two parts, e.g. MyClass.do_something
          (s-join "." (-slice (s-split-up-to "\\." name 2) 0 2)))
         (name
          ;; If the first part starts with a lowercase letter, it is likely
          ;; a function, not a class. Keep the first part and discard
          ;; nested function names or nested class names, if any.
          (if (s-lowercase? (substring name 0 1))
              (car (s-split-up-to "\\." name 1))
            name)))
    name))

(defun kizen-test--list-element-strings (open-bracket-pos)
  "Return source strings for the top-level elements of the [...] list
whose `[' is at OPEN-BRACKET-POS.  Strings and nested ()/[]/{} are
skipped when locating the top-level commas.  A trailing comma adds no
element; an empty list returns nil."
  (save-excursion
    (goto-char open-bracket-pos)
    (let* ((list-end (save-excursion (forward-sexp) (point)))  ; char after ]
           (inner-end (1- list-end))                           ; the ] itself
           (elems nil)
           (start nil))
      (forward-char 1)                                          ; step over [
      (skip-chars-forward " \t\n\r" inner-end)
      (setq start (point))
      (while (< (point) inner-end)
        (let ((ch (char-after)))
          (cond
           ((eq ch ?,)
            (let ((s (string-trim (buffer-substring-no-properties start (point)))))
              (when (> (length s) 0) (push s elems)))
            (forward-char 1)
            (skip-chars-forward " \t\n\r" inner-end)
            (setq start (point)))
           ((memq ch '(?\( ?\[ ?\{ ?\" ?\'))
            (condition-case nil (forward-sexp) (error (goto-char inner-end))))
           (t (forward-char 1)))))
      (let ((s (string-trim
                (buffer-substring-no-properties start (min (point) inner-end)))))
        (when (> (length s) 0) (push s elems)))
      (nreverse elems))))

(defun kizen-test--parameterized-elements ()
  "Return parameter info for the parameterized method at point.
The result is a list of element source strings (one per generated
variant), the symbol `unknown' (parameterized but not statically
enumerable: a non-literal list argument, or a `name_func=' kwarg), or
nil (not parameterized)."
  (save-excursion
    (python-nav-beginning-of-defun)
    (let ((deco nil) (go t))
      (forward-line -1)
      (while (and go (not (bobp)))
        (beginning-of-line)
        (let ((ppss (syntax-ppss)))
          (cond
           ((> (nth 0 ppss) 0))                                  ; inside brackets: continuation
           ((looking-at-p "[ \t]*$"))                            ; blank line
           ((looking-at-p "[ \t]*@parameterized\\.expand\\b")
            (setq deco (point) go nil))
           ((looking-at-p "[ \t]*@"))                            ; another decorator
           (t (setq go nil))))                                   ; real code: stop
        (when go (forward-line -1)))
      (when deco
        (goto-char deco)
        (when (re-search-forward "parameterized\\.expand[ \t\n]*(" nil t)
          (let* ((args-open (1- (point)))
                 (call-end (ignore-errors
                             (save-excursion (goto-char args-open)
                                             (forward-sexp) (point)))))
            (cond
             ((and call-end
                   (save-excursion (goto-char args-open)
                                   (re-search-forward "\\bname_func[ \t]*=" call-end t)))
              'unknown)
             (t
              (goto-char (1+ args-open))
              (skip-chars-forward " \t\n")
              (if (not (eq (char-after) ?\[))
                  'unknown
                (kizen-test--list-element-strings (point)))))))))))

(defun kizen-test--prefix-extendable-p (typed n)
  "Return non-nil if some index in [0, N) has TYPED as a strict decimal prefix.
When non-nil, TYPED is ambiguous and the user may type another digit to
disambiguate (or RET to commit TYPED as-is)."
  (let ((found nil) (i 0))
    (while (and (not found) (< i n))
      (let ((s (number-to-string i)))
        (when (and (> (length s) (length typed))
                   (string-prefix-p typed s))
          (setq found t)))
      (setq i (1+ i)))
    found))

(defun kizen-test--parameterized-read (method elements)
  "Quick-select menu for parameterized METHOD over ELEMENTS (source strings).
Return `all' or an integer index.  A pop-up lists each index with the
source text of its parameter set.  A single keypress runs immediately
when unambiguous; a low index that is the prefix of a larger one (only
possible with 11+ variants) is committed with RET."
  (let* ((n (length elements))
         (buf (get-buffer-create "*kizen-test select*"))
         (win nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize (format "Run parameterized: %s\n\n" method)
                                  'face 'bold))
              (insert (format "  a : all (%d)\n" n))
              (let ((i 0))
                (dolist (el elements)
                  (insert (format "  %d : %s\n" i (s-truncate 70 el)))
                  (setq i (1+ i)))))
            (setq buffer-read-only t)
            (goto-char (point-min)))
          (setq win (display-buffer
                     buf '(display-buffer-at-bottom
                           (window-height . fit-window-to-buffer))))
          (when win (fit-window-to-buffer win))
          (let ((typed "") (result nil))
            (while (null result)
              (let* ((prompt (format "Select [a=all, 0-%d]%s: " (1- n)
                                     (if (string= typed "") ""
                                       (concat " " typed))))
                     (ch (read-char-exclusive prompt)))
                (cond
                 ((memq ch '(?q ?\e)) (keyboard-quit))
                 ((and (string= typed "") (memq ch '(?a ?A))) (setq result 'all))
                 ((and (>= ch ?0) (<= ch ?9))
                  (let* ((next (concat typed (char-to-string ch)))
                         (num (string-to-number next)))
                    (cond
                     ((>= num n) (setq typed ""))              ; out of range: reset
                     ((kizen-test--prefix-extendable-p next n) ; ambiguous: keep typing
                      (setq typed next))
                     (t (setq result num)))))                  ; unambiguous: run
                 ((memq ch '(?\r ?\n))                         ; commit typed digits
                  (when (and (not (string= typed ""))
                             (< (string-to-number typed) n))
                    (setq result (string-to-number typed))))
                 ((eq ch ?\d)                                  ; DEL: erase a digit
                  (when (> (length typed) 0)
                    (setq typed (substring typed 0 -1))))
                 (t nil))))                                    ; ignore anything else
            result))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun kizen-test--parameterized-select-unknown ()
  "Prompt when count is unknown; return `all' or a raw variant-suffix string."
  (let ((s (read-string
            "Parameterized (count unknown). 'a'=all, or type variant suffix: " "a")))
    (if (member s '("a" "all" "")) 'all s)))

(defun kizen-test--parameterized-k-pattern (method n choice)
  "Build the -k substring pattern for METHOD given N params and CHOICE.
CHOICE is `all', an integer index, or a raw suffix string.  Integer
indices are zero-padded to match parameterized's
`digits = len(str(N-1))' rule."
  (cond
   ((eq choice 'all) method)
   ((integerp choice)
    (let* ((digits (length (number-to-string (max 0 (1- n)))))
           (padded (format (format "%%0%dd" digits) choice)))
      (format "%s_%s" method padded)))
   ((stringp choice) (format "%s_%s" method choice))
   (t method)))

(defun kizen-test--run-parameterized (file func args pinfo)
  "Prompt for a parameterized variant and run it via a -k filter.
FUNC is `ClassName.method' or just `method' (module-level).  PINFO is a
list of element source strings, or the symbol `unknown'."
  (let* ((parts (s-split-up-to "\\." func 1))
         (has-class (= (length parts) 2))
         (class (when has-class (car parts)))
         (method (if has-class (cadr parts) func))
         (n (if (listp pinfo) (length pinfo) 0))
         (choice (if (eq pinfo 'unknown)
                     (kizen-test--parameterized-select-unknown)
                   (kizen-test--parameterized-read method pinfo)))
         (pattern (kizen-test--parameterized-k-pattern method n choice)))
    (kizen-test--run
     :args (-snoc args "-k" (kizen-test--shell-quote pattern))
     :file file
     :func class                      ; nil -> positional becomes <module>
     :edit current-prefix-arg)))

(defun kizen-test--make-test-name (func)
  "Turn function name FUNC into a name (hopefully) matching its test name.
Example: ‘MyABCThingy.__repr__’ becomes ‘test_my_abc_thingy_repr’."
  (-->
   func
   (s-replace "." "_" it)
   (s-snake-case it)
   (s-replace-regexp "_\+" "_" it)
   (s-chop-suffix "_" it)
   (s-chop-prefix "_" it)
   (format "test_%s" it)))


;; file/directory helpers

(defun kizen-test--project-name ()
  "Find the project name."
  (projectile-project-name))

(defun kizen-test--project-root ()
  "Find the project root directory."
  (let ((projectile-require-project-root nil))
    (projectile-project-root)))

(defcustom kizen-test-default-directory "~/projects/kizen/webapp"
  "Fallback directory in which to run kizen-test.
Used only when the Django root (the directory containing ‘manage.py’)
cannot be detected from the current buffer."
  :group 'kizen-test
  :type 'directory)

(defun kizen-test--webapp-root ()
  "Detect the Django root for the current buffer.
This is the directory containing ‘manage.py’, found by walking up from
the current ‘default-directory’ so that tests run in the correct git
worktree.  Falls back to the projectile project root and finally to
`kizen-test-default-directory'."
  (file-name-as-directory
   (expand-file-name
    (or (locate-dominating-file default-directory "manage.py")
        (ignore-errors (kizen-test--project-root))
        kizen-test-default-directory))))

(defun kizen-test--relative-file-name (file)
  "Make FILE relative to the project root."
  ;; Note: setting default-directory gives different results
  ;; than providing a second argument to file-relative-name.
  (let ((default-directory (kizen-test--project-root)))
    (file-relative-name file)))

(defun kizen-test--test-file-p (file)
  "Tell whether FILE is a test file."
  (projectile-test-file-p file))

(defun kizen-test--find-test-file (file)
  "Find a test file associated to FILE, if any."
  (let ((test-file (projectile-find-matching-test file)))
    (unless test-file
      (user-error "No test file found"))
    test-file))

(defun kizen-test--sensible-test-file (file)
  "Return a sensible test file name for FILE."
  (if (kizen-test--test-file-p file)
      (kizen-test--relative-file-name file)
    (kizen-test--find-test-file file)))

(cl-defun kizen-test--select-test-files (&key type)
  "Interactively choose test files."
  (cl-block nil
    (let* ((test-files
            (->> (projectile-project-files (kizen-test--project-root))
                 (-sort 'string<)
                 (projectile-sort-by-recentf-first)
                 (projectile-test-files)))
           (test-directories
            (->> test-files
                 (-map 'file-name-directory)
                 (-uniq)
                 (-sort 'string<)))
           (candidates (if (eq type 'file) test-files test-directories))
           (done-message (propertize "[finish test file selection]" 'face 'success))
           (choices)
           (choice)
           (selection-active t))
      (unless candidates
        (user-error "No test files found"))
      (while (and selection-active candidates)
        (setq choice (completing-read
                      "Choose test files: "
                      (if choices (cons done-message candidates) candidates)
                      nil t))
        (if (s-equals-p choice done-message)
            (setq selection-active nil)
          (setq
           choices (cons choice choices)
           candidates (-remove-item choice candidates))))
      (cl-return (reverse choices)))))

(defun kizen-test--maybe-save-buffers ()
  "Maybe save modified buffers."
  (cond
   ((memq kizen-test-unsaved-buffers-behavior '(ask-current save-current))
    ;; check only current buffer
    (when (and (buffer-modified-p)
               (or (eq kizen-test-unsaved-buffers-behavior 'save-current)
                   (y-or-n-p
                    (format "Save modified buffer (%s)? " (buffer-name)))))
      (save-buffer)))
   ((memq kizen-test-unsaved-buffers-behavior '(ask-all save-all))
    ;; check all project buffers
    (-when-let*
        ((buffers
          (projectile-buffers-with-file (projectile-project-buffers)))
         (modified-buffers
          (-filter 'buffer-modified-p buffers))
         (confirmed
          (or (eq kizen-test-unsaved-buffers-behavior 'save-all)
              (y-or-n-p
               (format "Save modified project buffers (%d)? "
                       (length modified-buffers))))))
      (--each modified-buffers
        (with-current-buffer it
          (save-buffer)))))
   (t nil)))


;; third party integration

(with-eval-after-load 'direnv
  (defvar direnv-non-file-modes)
  (add-to-list 'direnv-non-file-modes 'kizen-test-mode))


(provide 'kizen-test)
;;; kizen-test.el ends here
