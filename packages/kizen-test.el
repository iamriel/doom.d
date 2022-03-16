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

(defcustom kizen-test-executable "docker-compose exec web ./manage.py test --keepdb --no-migrations --exclude-tag slow"
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
  :man-page "docker-compose exec web ./manage.py test --keepdb --no-migrations --exclude-tag slow"
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
(defun kizen-test-function (file func args)
  "Run kizen-test on FILE with FUNC (or class).
Additional ARGS are passed along to kizen-test.
With a prefix argument, allow editing."
  (interactive
   (list
    (buffer-file-name)
    (kizen-test--current-defun)
    (transient-args 'kizen-test-dispatch)))
  (kizen-test--run
   :args args
   :file file
   :func func
   :edit current-prefix-arg))

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
(defun kizen-test-function-dwim (file func args)
  "Run kizen-test on FILE with FUNC (or class).
When run interactively, this tries to work sensibly using
the current file and function around point.
Additional ARGS are passed along to kizen-test.
With a prefix argument, allow editing."
  (interactive
   (list
    (buffer-file-name)
    (kizen-test--current-defun)
    (transient-args 'kizen-test-dispatch)))
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
   :edit current-prefix-arg))

;;;###autoload
(defun kizen-test-last-failed (&optional args)
  "Run kizen-test, only executing previous test failures.
Additional ARGS are passed along to kizen-test.
With a prefix argument, allow editing."
  (interactive (list (transient-args 'kizen-test-dispatch)))
  (kizen-test--run
   :args (-snoc args "--last-failed")
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
  (compilation-setup))

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
  (let* ((default-directory "~/projects/kizen/webapp"))
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
      (when kizen-test-pdb-track
        (add-hook
         'comint-output-filter-functions
         'python-pdbtrack-comint-output-filter-function
         nil t))
      (run-hooks 'kizen-test-setup-hook)
      (make-comint-in-buffer "kizen-test" buffer "sh" nil "-c" command)
      (run-hooks 'kizen-test-started-hook)
      (setq process (get-buffer-process buffer))
      (set-process-sentinel process #'kizen-test--process-sentinel)
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
