;;; agent-hq-dashboard-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-dashboard-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: shells are stand-in buffers, their status
;; stubbed, and the worktree checks use real git in a temp directory.

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-statusline.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-dashboard.el" dir) nil t))

(defvar agent-shell--state nil)
(defvar agent-shell-attention--pending (make-hash-table :test #'eq))

(define-derived-mode agent-hq-dashboard-test-shell-mode fundamental-mode "Test shell"
  "Stands in for `agent-shell-mode'.")
(put 'agent-hq-dashboard-test-shell-mode 'derived-mode-parent 'agent-shell-mode)

;;;; What the agent said

(ert-deftest agent-hq-dashboard-test-first-line ()
  (should (equal (agent-hq-dashboard--first-line "Done. Fixed it.\nMore below") "Done. Fixed it."))
  ;; Headings are passed over for prose; Markdown marks go.
  (should (equal (agent-hq-dashboard--first-line "## Summary\n\n- **Fixed** the `loader`\n")
                 "Fixed the loader"))
  (should (equal (agent-hq-dashboard--first-line "1. First step\n") "First step"))
  ;; Code blocks, tables and rules are not the answer.
  (should (equal (agent-hq-dashboard--first-line "```elisp\n(setq x 1)\n```\nAfter the code")
                 "After the code"))
  (should (equal (agent-hq-dashboard--first-line "| a | b |\n|---|---|\n---\nThe table above")
                 "The table above"))
  ;; A heading is the answer only when nothing else has words.
  (should (equal (agent-hq-dashboard--first-line "# Only a heading\n\n") "Only a heading"))
  (should-not (agent-hq-dashboard--first-line "\n  \n"))
  (should-not (agent-hq-dashboard--first-line nil)))

(ert-deftest agent-hq-dashboard-test-age ()
  (should (equal (agent-hq-dashboard--age nil) ""))
  (should (equal (agent-hq-dashboard--age -2) "0s"))
  (should (equal (agent-hq-dashboard--age 45.7) "45s"))
  (should (equal (agent-hq-dashboard--age 125) "2m"))
  (should (equal (agent-hq-dashboard--age 7300) "2h"))
  (should (equal (agent-hq-dashboard--age (* 3 86400)) "3d")))

(ert-deftest agent-hq-dashboard-test-stop-text ()
  (should-not (agent-hq-dashboard--stop-text nil))
  (should-not (agent-hq-dashboard--stop-text "end_turn"))
  (should (equal (agent-hq-dashboard--stop-text "cancelled") "Cancelled"))
  (should (equal (agent-hq-dashboard--stop-text "weird") "Stopped: weird")))

(ert-deftest agent-hq-dashboard-test-tool-label ()
  (with-temp-buffer
    (let ((shell (current-buffer)))
      ;; A command shows what it's for, else the command itself.
      (should (equal (agent-hq-dashboard--tool-label
                      shell '((:kind . "execute") (:title . "npm test") (:description . "Run the tests")))
                     "Run the tests"))
      (should (equal (agent-hq-dashboard--tool-label
                      shell '((:kind . "execute") (:title . "npm test\n--watch")))
                     "$ npm test"))
      (should (equal (agent-hq-dashboard--tool-label
                      shell '((:kind . "edit") (:title . "Edit `src/room.ts`")))
                     "Edit src/room.ts"))
      (should-not (agent-hq-dashboard--tool-label shell '((:kind . "read")))))))

(ert-deftest agent-hq-dashboard-test-follows-a-turn ()
  (with-temp-buffer
    (let ((shell (current-buffer))
          (event (lambda (name &rest data)
                   (agent-hq-dashboard--note-event
                    (current-buffer) (list (cons :event name) (cons :data data))))))
      (funcall event 'input-submitted)
      (should (numberp agent-hq-dashboard--turn-start))
      (funcall event 'agent-message-chunk (cons :text-chunk "Let me look at "))
      (funcall event 'agent-message-chunk (cons :text-chunk "the loader.\n"))
      (should (equal (agent-hq-dashboard--first-line (agent-hq-dashboard--text-of shell))
                     "Let me look at the loader."))
      ;; A tool call ends those words: what follows is the answer.
      (funcall event 'tool-call-update (cons :tool-call-id "t1"))
      (funcall event 'agent-message-chunk (cons :text-chunk nil))
      (funcall event 'agent-message-chunk (cons :text-chunk "## Done\n\nFixed the "))
      (funcall event 'agent-message-chunk (cons :text-chunk "loader; tests pass.\n"))
      (funcall event 'turn-complete (cons :stop-reason "end_turn"))
      (should (equal agent-hq-dashboard--answer "Fixed the loader; tests pass."))
      (should-not agent-hq-dashboard--outcome)
      (should (numberp agent-hq-dashboard--turn-end))
      ;; A cancelled turn says so; the next turn starts afresh.
      (funcall event 'input-submitted)
      (should-not agent-hq-dashboard--text)
      (funcall event 'turn-complete (cons :stop-reason "cancelled"))
      (should (equal agent-hq-dashboard--outcome "Cancelled"))
      (should-not agent-hq-dashboard--answer)
      (funcall event 'error (cons :message "Overloaded\ntry later"))
      (should (equal agent-hq-dashboard--outcome "Error: Overloaded")))))

;;;; The dashboard, with stand-in shells

(defvar agent-hq-dashboard-test--status nil
  "Alist of (SHELL . STATUS) for the stubbed `agent-hq--status'.")

(defvar agent-hq-dashboard-test--activity nil
  "Alist of (SHELL . FLOAT-TIME) for the stubbed `agent-hq--activity'.")

(defun agent-hq-dashboard-test--shell (base dir title status &rest state)
  "Return a stand-in shell working in BASE/DIR with session TITLE and STATUS.
STATE is more of its `agent-shell--state', as alternating keys and values."
  (let ((shell (generate-new-buffer (format "Claude @ %s" dir)))
        (root (file-name-as-directory (expand-file-name dir base))))
    (make-directory root t)
    (with-current-buffer shell
      (agent-hq-dashboard-test-shell-mode)
      (setq default-directory root)
      (setq-local agent-shell--state
                  (append (list (cons :session (list (cons :title title)))
                                (cons :agent-config (list (cons :buffer-name "Claude"))))
                          (cl-loop for (key value) on state by #'cddr
                                   collect (cons key value)))))
    (push (cons shell status) agent-hq-dashboard-test--status)
    shell))

(defmacro agent-hq-dashboard-test--with-shells (&rest body)
  "Run BODY with `base', a temp dir for shells, and agent-hq's view stubbed.
Shells made with `agent-hq-dashboard-test--shell' are the live ones;
they and the dashboard are killed afterwards."
  (declare (indent 0))
  `(let* ((base (file-name-as-directory (file-truename (make-temp-file "agent-hq-dashboard" t))))
          (agent-hq-dashboard-test--status nil)
          (agent-hq-dashboard-test--activity nil)
          (agent-shell-attention--pending (make-hash-table :test #'eq))
          (agent-hq-dashboard--timer nil)
          (agent-hq-dashboard--soon nil)
          ;; The shared clock, of this test's own.
          (agent-hq-clock--animations nil)
          (agent-hq-clock--timer nil))
     (cl-letf (((symbol-function 'agent-hq--shells)
                (lambda () (seq-filter #'buffer-live-p
                                       (reverse (mapcar #'car agent-hq-dashboard-test--status)))))
               ((symbol-function 'agent-hq--status)
                (lambda (shell) (alist-get shell agent-hq-dashboard-test--status)))
               ((symbol-function 'agent-hq--activity)
                (lambda (shell) (alist-get shell agent-hq-dashboard-test--activity 0)))
               ((symbol-function 'agent-hq-dashboard-setup) #'ignore))
       (unwind-protect
           (progn ,@body)
         (dolist (timer (list agent-hq-dashboard--timer agent-hq-clock--timer
                              agent-hq-dashboard--soon))
           (when (timerp timer) (cancel-timer timer)))
         (dolist (entry agent-hq-dashboard-test--status)
           (when (buffer-live-p (car entry))
             (with-current-buffer (car entry)
               (setq-local kill-buffer-query-functions nil))
             (kill-buffer (car entry))))
         (when-let* ((dashboard (get-buffer agent-hq-dashboard-buffer-name)))
           (kill-buffer dashboard))
         (delete-directory base t)))))

(defun agent-hq-dashboard-test--lines ()
  "Return the dashboard's lines, without trailing spaces."
  (with-current-buffer agent-hq-dashboard-buffer-name
    (mapcar #'string-trim-right
            (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t))))

(defun agent-hq-dashboard-test--row (shell)
  "Return SHELL's dashboard line, without trailing spaces."
  (with-current-buffer agent-hq-dashboard-buffer-name
    (when-let* ((start (agent-hq-dashboard--position shell)))
      (string-trim-right (buffer-substring-no-properties
                          start (save-excursion (goto-char start) (line-end-position)))))))

(ert-deftest agent-hq-dashboard-test-groups-and-rows ()
  (agent-hq-dashboard-test--with-shells
    (let* ((now (float-time))
           (idle-old (agent-hq-dashboard-test--shell base "doom.d" "Account fallback" 'ready))
           (idle-new (agent-hq-dashboard-test--shell base "clipshelf" "Saved links" 'ready))
           (unread (agent-hq-dashboard-test--shell base "bop-boulder" "Rescued friends" 'ready))
           (blocked (agent-hq-dashboard-test--shell
                     base "webapp" "Max length" 'blocked
                     :tool-calls '(("t2" . ((:kind . "execute") (:title . "rm -rf build")
                                            (:description . "Clean the build")
                                            (:status . "pending")
                                            (:permission-request-id . 7))))))
           (running (agent-hq-dashboard-test--shell
                     base "pebbles" "Golden pebble room" 'busy
                     :tool-calls '(("t9" . ((:kind . "edit") (:title . "Edit `room.ts`")
                                            (:status . "in_progress")))
                                   ("t8" . ((:kind . "read") (:title . "Read a.ts")
                                            (:status . "completed"))))
                     :pending-prompts '("and the tests")
                     :usage '((:context-used . 50000) (:context-size . 200000))))
           (starting (agent-hq-dashboard-test--shell base "newbie" nil 'starting)))
      (setq agent-hq-dashboard-test--activity
            (list (cons idle-old (- now 7200)) (cons idle-new (- now 300))
                  (cons running (- now 90))))
      (with-current-buffer unread
        (setq agent-hq-dashboard--answer "Pushed the fix; tests pass"
              agent-hq-dashboard--turn-end (- now 600)))
      (puthash unread (cons "Awaiting input" (- now 600)) agent-shell-attention--pending)
      (puthash blocked (list :label "Permission: rm" :timestamp (- now 120) :request-id 7)
               agent-shell-attention--pending)
      (agent-hq-dashboard)
      (let ((lines (agent-hq-dashboard-test--lines)))
        ;; Column names, then each group: who needs you, what runs, what's idle.
        (should (string-match-p "\\`  +Project +Branch +Session +Age +Latest +Model +Ctx\\'"
                                (car lines)))
        (should (equal (seq-filter (lambda (line) (string-match-p "\\`[A-Z]" line)) lines)
                       '("Needs you (2)" "Running (1)" "Idle (2)" "Starting (1)")))
        ;; Blocked before unread; idle most recent first.
        (should (equal (mapcar (lambda (shell) (cl-position (agent-hq-dashboard-test--row shell)
                                                            lines :test #'equal))
                               (list blocked unread running idle-new idle-old starting))
                       '(2 3 5 7 8 10))))
      (should (string-match-p "● webapp +Max length +2m +Permission: Clean the build +Claude"
                              (agent-hq-dashboard-test--row blocked)))
      (should (string-match-p "● bop-boulder +Rescued friends +10m +Pushed the fix; tests pass"
                              (agent-hq-dashboard-test--row unread)))
      ;; The newest call under way, the queue, the context used.
      (should (string-match-p "pebbles +Golden pebble room +1m +Edit room.ts · 1 queued +Claude +25%"
                              (agent-hq-dashboard-test--row running)))
      (should (string-match-p "✓ doom.d +Account fallback +2h"
                              (agent-hq-dashboard-test--row idle-old)))
      ;; A turn stopped with prompts still queued: they're held.
      (with-current-buffer idle-new
        (setq agent-hq-dashboard--outcome "Cancelled")
        (setf (alist-get :pending-prompts agent-shell--state) '("and then")))
      (with-current-buffer agent-hq-dashboard-buffer-name
        (agent-hq-dashboard-refresh))
      (should (string-match-p "Saved links +5m +Cancelled · 1 held"
                              (agent-hq-dashboard-test--row idle-new)))
      (should (string-match-p "○ newbie +New session +Starting…"
                              (agent-hq-dashboard-test--row starting)))
      (with-current-buffer agent-hq-dashboard-buffer-name
        ;; The header line sums up, % escaped for the mode line format.
        (should (string-match-p "Needs you 2 · Running 1 · Idle 2 · Starting 1"
                                (substring-no-properties header-line-format)))
        ;; The running row spins.
        (should (= (length agent-hq-dashboard--spinners) 1))
        (should (agent-hq-clock-running-p 'agent-hq-dashboard))
        (agent-hq-dashboard--spin 3)
        (should (equal (overlay-get (car agent-hq-dashboard--spinners) 'display)
                       (nth 3 agent-hq-spinner-frames)))))))

(ert-deftest agent-hq-dashboard-test-fetches-only-when-asked ()
  ;; Shell events redraw up to twice a second: those show git and plan
  ;; usage as last fetched.  Opening, the tick and gr may fetch.
  (agent-hq-dashboard-test--with-shells
    (let ((shell (agent-hq-dashboard-test--shell base "repo" "R" 'ready))
          asked)
      (agent-hq-dashboard-test--git (expand-file-name "repo" base) "init" "-q")
      (cl-letf (((symbol-function 'agent-hq-statusline--git)
                 (lambda (_ctx) (push (not agent-hq-statusline-no-fetch) asked) nil)))
        (ignore shell)
        ;; Opening lays it out before it shows and again at its width.
        (agent-hq-dashboard)
        (should (and asked (seq-every-p #'identity asked)))
        (with-current-buffer agent-hq-dashboard-buffer-name
          (setq asked nil)
          (agent-hq-dashboard-refresh)
          (should (equal asked '(nil)))
          (setq asked nil)
          (agent-hq-dashboard--tick)
          (should (equal asked '(t)))
          (setq asked nil)
          (call-interactively #'agent-hq-dashboard-refresh)
          (should (equal asked '(t))))))))

(ert-deftest agent-hq-dashboard-test-empty ()
  (agent-hq-dashboard-test--with-shells
    (agent-hq-dashboard)
    (should (member "No agent shells are running." (agent-hq-dashboard-test--lines)))
    (should (string-match-p "No shells" (with-current-buffer agent-hq-dashboard-buffer-name
                                          (substring-no-properties header-line-format))))))

(ert-deftest agent-hq-dashboard-test-widths ()
  (let* ((tabulated-list-padding 2)
         (row (vector "●" (make-string 30 ?p) (make-string 30 ?b) (make-string 50 ?s)
                      "12m" (make-string 80 ?l) "Opus 5.5 work" "42%")))
    ;; Wide: columns take what they need, up to their caps; Latest the rest.
    (should (equal (agent-hq-dashboard--widths (list row) 300)
                   [1 26 30 42 3 80 13 3]))
    ;; Narrower: Session gives way first, then Branch, then Project.
    (let ((widths (agent-hq-dashboard--widths (list row) 150)))
      (should (equal (seq-take widths 3) [1 26 30]))
      (should (< (aref widths 3) 42))
      (should (= (+ 2 7 (apply #'+ (append widths nil))) 150)))
    (let ((widths (agent-hq-dashboard--widths (list row) 90)))
      (should (equal widths [1 12 14 16 3 24 13 3])))
    ;; A column narrower than its floor (no branch outside git) stays so.
    (aset row 2 "")
    (should (equal (aref (agent-hq-dashboard--widths (list row) 90) 2) 6))))

(ert-deftest agent-hq-dashboard-test-moves-and-flags ()
  (agent-hq-dashboard-test--with-shells
    (let* ((a (agent-hq-dashboard-test--shell base "a" "A" 'ready))
           (b (agent-hq-dashboard-test--shell base "b" "B" 'busy))
           (c (agent-hq-dashboard-test--shell base "c" "C" 'ready))
           asked)
      (setq agent-hq-dashboard-test--activity
            (list (cons a (- (float-time) 10)) (cons c (- (float-time) 20))))
      (agent-hq-dashboard)
      (with-current-buffer agent-hq-dashboard-buffer-name
        ;; n and p step over headings and blank lines.
        (goto-char (point-min))
        (agent-hq-dashboard-next)
        (should (eq (tabulated-list-get-id) b))
        (agent-hq-dashboard-next)
        (should (eq (tabulated-list-get-id) a))
        (agent-hq-dashboard-next 5)
        (should (eq (tabulated-list-get-id) c))
        (agent-hq-dashboard-previous)
        (should (eq (tabulated-list-get-id) a))
        ;; d flags and moves on; the flag outlives a redraw.
        (agent-hq-dashboard-flag)
        (should (eq (tabulated-list-get-id) c))
        (agent-hq-dashboard-refresh)
        (should (string-prefix-p "D" (agent-hq-dashboard-test--row a)))
        (should (eq (tabulated-list-get-id) c))
        ;; ~ adds every idle shell; the running one stays out.
        (agent-hq-dashboard-flag-idle)
        (should (equal (sort (mapcar #'buffer-name agent-hq-dashboard--flagged) #'string<)
                       (list (buffer-name a) (buffer-name c))))
        (goto-char (agent-hq-dashboard--position c))
        (agent-hq-dashboard-unflag)
        (should (string-prefix-p " " (agent-hq-dashboard-test--row c)))
        ;; x kills the flagged after one question.
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) t)))
          (agent-hq-dashboard-kill-flagged))
        (should (equal asked '("Kill 1 shell? ")))
        (should-not (buffer-live-p a))
        (should (buffer-live-p c))
        (should-not (agent-hq-dashboard-test--row a))
        ;; The question says what killing would cut short.
        (with-current-buffer c
          (setf (alist-get :pending-prompts agent-shell--state) '("held back")))
        (setq agent-hq-dashboard--flagged (list b c) asked nil)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) nil)))
          (agent-hq-dashboard-kill-flagged))
        (should (equal asked '("Kill 2 shells (1 working or waiting on you, 1 with prompts queued)? ")))
        (should (buffer-live-p b))
        ;; Headings have no shell to act on.
        (goto-char (point-min))
        (should-error (agent-hq-dashboard-flag) :type 'user-error)))))

(ert-deftest agent-hq-dashboard-test-fits-and-closes ()
  (agent-hq-dashboard-test--with-shells
    (let* ((a (agent-hq-dashboard-test--shell base "a" "A" 'ready))
           ;; At the bottom, full width, as Doom's popup rule shows it.
           (display-buffer-alist `((,(regexp-quote agent-hq-dashboard-buffer-name)
                                    display-buffer-in-side-window
                                    (side . bottom) (window-height . 0.9))))
           (agent-hq-dashboard-max-height 0.8)
           visited)
      (delete-other-windows)
      (agent-hq-dashboard)
      (let ((window (get-buffer-window agent-hq-dashboard-buffer-name)))
        (should (eq (window-parameter window 'window-side) 'bottom))
        ;; Fit to its four lines: column names, heading, row, and the end.
        (let ((small (window-height window)))
          (should (< small (floor (* 0.9 (frame-height)))))
          ;; Another shell: a heading and a row more, so it grows.
          (agent-hq-dashboard-test--shell base "b" "B" 'busy)
          (with-current-buffer agent-hq-dashboard-buffer-name
            (agent-hq-dashboard-refresh))
          (should (> (window-height window) small))
          ;; Resized by hand, it keeps its size while the rows stay the same.
          (with-selected-window window
            (enlarge-window 1))
          (let ((by-hand (window-height window)))
            (with-current-buffer agent-hq-dashboard-buffer-name
              (agent-hq-dashboard-refresh))
            (should (= (window-height window) by-hand))))
        ;; RET visits the shell and closes the dashboard, as picking does.
        (cl-letf (((symbol-function 'agent-hq-visit-shell)
                   (lambda (shell) (push (list shell (selected-window)) visited))))
          (with-selected-window window
            (goto-char (agent-hq-dashboard--position a))
            (agent-hq-dashboard-visit)))
        (should (equal (mapcar #'car visited) (list a)))
        (should-not (window-live-p window))
        (should-not (eq (cadr (car visited)) window))))))

(ert-deftest agent-hq-dashboard-test-send ()
  (agent-hq-dashboard-test--with-shells
    (let* ((idle (agent-hq-dashboard-test--shell base "idle" "I" 'ready))
           (busy (agent-hq-dashboard-test--shell base "busy" "B" 'busy))
           sent queued)
      (cl-letf (((symbol-function 'shell-maker-busy)
                 (lambda () (eq (current-buffer) busy)))
                ((symbol-function 'agent-shell-insert)
                 (lambda (&rest args) (push (cons (current-buffer) args) sent)))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (push (cons (current-buffer) args) queued))))
        (agent-hq-dashboard-send idle "go on")
        (should (equal sent `((,idle :text "go on" :submit t :no-focus t :shell-buffer ,idle))))
        (agent-hq-dashboard-send busy "then the tests")
        (should (equal queued `((,busy :prompt "then the tests"))))
        (should-error (agent-hq-dashboard-send idle "  ") :type 'user-error)))))

;;;; Worktrees done with

(defun agent-hq-dashboard-test--git (dir &rest args)
  "Run git ARGS in DIR, failing the test on a non-zero exit."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil
                            "-c" "user.name=Test" "-c" "user.email=t@example.com"
                            "-c" "init.defaultBranch=develop" args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(ert-deftest agent-hq-dashboard-test-done-worktrees ()
  (let* ((base (file-name-as-directory (file-truename (make-temp-file "agent-hq-dashboard" t))))
         (origin (expand-file-name "origin.git" base))
         (main (expand-file-name "app/" base))
         (linked (expand-file-name "app_w1/" base))
         (process-environment (append (list "GIT_CONFIG_GLOBAL=/dev/null"
                                            (concat "XDG_CONFIG_HOME=" base))
                                      process-environment))
         (agent-hq-dashboard--beyond (make-hash-table :test #'equal))
         (agent-hq-base-branch-alist '(("/app/\\'" . "develop")))
         (clean '(:ahead 0 :behind 0 :staged 0 :modified 0 :untracked 0 :conflicts 0))
         (wait (lambda (root)
                 (let ((process (plist-get (gethash root agent-hq-dashboard--beyond) :process)))
                   (while (process-live-p process)
                     (accept-process-output process 0.05))
                   (accept-process-output nil 0.05)))))
    (unwind-protect
        (progn
          (agent-hq-dashboard-test--git base "init" "-q" "--bare" origin)
          (make-directory main)
          (agent-hq-dashboard-test--git main "init" "-q")
          (agent-hq-dashboard-test--git main "commit" "-q" "--allow-empty" "-m" "init")
          (agent-hq-dashboard-test--git main "remote" "add" "origin" origin)
          (agent-hq-dashboard-test--git main "worktree" "add" "-q" "-b" "feat/x"
                                        (directory-file-name linked))
          (let ((ctx (agent-hq-context linked)))
            ;; Nothing beyond develop yet: known once git answers.
            (should-not (agent-hq-dashboard--done ctx clean))
            (funcall wait (agent-hq-context-root ctx))
            (should (equal (agent-hq-dashboard--done ctx clean) "in develop"))
            ;; Not while anything is uncommitted, nor for the main checkout.
            (should-not (agent-hq-dashboard--done ctx (plist-put (copy-sequence clean) :untracked 1)))
            (should-not (agent-hq-dashboard--done (agent-hq-context main) clean))
            ;; A commit of its own: not done, until its pushed branch is deleted.
            (agent-hq-dashboard-test--git linked "commit" "-q" "--allow-empty" "-m" "work")
            (let ((agent-hq-dashboard-base-ttl 0))
              (agent-hq-dashboard--beyond-base ctx)
              (funcall wait (agent-hq-context-root ctx))
              (should (eql (agent-hq-dashboard--beyond-base ctx) 1)))
            (should-not (agent-hq-dashboard--done ctx clean))
            (agent-hq-dashboard-test--git linked "push" "-q" "-u" "origin" "feat/x")
            (agent-hq-dashboard-test--git main "push" "-q" "origin" "--delete" "feat/x")
            (agent-hq-dashboard-test--git main "fetch" "-q" "--prune" "origin")
            (let ((data (agent-hq-statusline-parse-status
                         (agent-hq-dashboard-test--git linked "status" "--porcelain=v2" "--branch"))))
              (should (plist-get data :gone))
              (should (equal (agent-hq-dashboard--done ctx data) "merged")))))
      (delete-directory base t))))

(ert-deftest agent-hq-dashboard-test-git-reads-skip-coding-detection ()
  ;; Each redraw reads git files for every shell.  Coding detection runs
  ;; `find-auto-coding', which editorconfig advises to start a process.
  (let ((file (make-temp-file "agent-hq-head" nil nil "ref: refs/heads/feat/ünï\n"))
        (set-auto-coding-function (lambda (&rest _) (error "Detected a coding system"))))
    (unwind-protect
        (should (equal (agent-hq--read-first-line file) "ref: refs/heads/feat/ünï"))
      (delete-file file))))

(ert-deftest agent-hq-dashboard-test-base-name ()
  (let* ((base (file-name-as-directory (file-truename (make-temp-file "agent-hq-dashboard" t))))
         (main (expand-file-name "lib/" base)))
    (unwind-protect
        (progn
          (make-directory main)
          (agent-hq-dashboard-test--git main "init" "-q")
          (agent-hq-dashboard-test--git main "commit" "-q" "--allow-empty" "-m" "init")
          (let ((ctx (agent-hq-context main))
                (agent-hq-base-branch-alist nil))
            ;; origin/HEAD names the branch it points at.
            (make-directory (expand-file-name ".git/refs/remotes/origin" main) t)
            (with-temp-file (expand-file-name ".git/refs/remotes/origin/HEAD" main)
              (insert "ref: refs/remotes/origin/trunk\n"))
            (should (equal (agent-hq-dashboard--base-name ctx) "trunk"))
            (let ((agent-hq-base-branch-alist '(("/lib/\\'" . "origin/develop"))))
              (should (equal (agent-hq-dashboard--base-name ctx) "develop")))))
      (delete-directory base t))))

;;; agent-hq-dashboard-test.el ends here
