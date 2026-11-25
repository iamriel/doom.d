;;; agent-hq-status-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-status-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: its few functions used here are stubbed.

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-statusline.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-status.el" dir) nil t))

(defvar agent-shell--state nil)
(defvar shell-maker--config nil)

(defconst agent-hq-status-test--options
  '(((:id . "model") (:current-value . "default")
     (:options ((:value . "default") (:name . "Default (recommended)"))))
    ((:id . "effort") (:current-value . "xhigh")
     (:options ((:value . "default") (:name . "Default"))
               ((:value . "xhigh") (:name . "Xhigh"))))
    ((:id . "fast") (:current-value . "on")
     (:options ((:value . "off") (:name . "Off")) ((:value . "on") (:name . "On"))))
    ((:id . "mode") (:current-value . "auto")
     (:options ((:value . "default") (:name . "Manual"))
               ((:value . "auto") (:name . "Auto"))))))

(defun agent-hq-status-test--cli (dir logged-in)
  "Write a fake Claude CLI into DIR and return it.
It prints a version, or a login that is LOGGED-IN or not, and echoes
CLAUDE_CONFIG_DIR as the org so the test can see it arrived."
  (let ((cli (expand-file-name "claude" dir)))
    (with-temp-file cli
      (insert "#!/bin/sh\n"
              "echo 'noise' >&2\n"
              "case \"$1\" in\n"
              "  --version) echo '2.1.330 (Claude Code)' ;;\n"
              (if logged-in
                  "  auth) echo '{\"loggedIn\": true, \"authMethod\": \"claude.ai\", \"subscriptionType\": \"team\", \"email\": \"me@example.com\", \"orgName\": \"'\"${CLAUDE_CONFIG_DIR-unset}\"'\"}' ;;\n"
                "  auth) echo '{\"loggedIn\": false, \"authMethod\": \"none\"}'; exit 1 ;;\n")
              "esac\n"))
    (set-file-modes cli #o755)
    cli))

(defmacro agent-hq-status-test--in-shell (dir started-with &rest body)
  "Run BODY in a stub Claude shell in DIR, started with STARTED-WITH."
  (declare (indent 2))
  `(with-temp-buffer
     (setq agent-hq--claude-started-with ,started-with
           agent-shell--state
           `((:session . ((:id . "sess-1")))
             (:client . ((:environment-variables
                          . ("ANTHROPIC_BASE_URL=http://127.0.0.1:8787"
                             ,@(unless (eq ,started-with 'default)
                                 (list (concat "CLAUDE_CONFIG_DIR=" ,started-with)))))))
             (:usage . ((:context-used . 88000) (:context-size . 1000000)))))
     (cl-letf (((symbol-function 'agent-shell--config-options)
                (lambda (_) agent-hq-status-test--options))
               ((symbol-function 'agent-shell--current-model-id) (lambda (_) "default"))
               ((symbol-function 'agent-shell--get-available-models) (lambda (_) nil))
               ((symbol-function 'agent-shell--format-number-compact)
                (lambda (n) (format "%dk" (/ n 1000))))
               ((symbol-function 'agent-shell-cwd) (lambda () ,dir))
               ((symbol-function 'agent-hq-statusline-plan-text)
                (lambda (account) (format "plan of %S" account))))
       (setq agent-hq-statusline--answered-with '("default" . "claude-opus-5-5"))
       ,@body)))

(defun agent-hq-status-test--rows (text)
  "Return TEXT's status rows as (LABEL . VALUE), properties dropped."
  (mapcar (lambda (line)
            (cons (string-trim (substring line 0 13)) (substring line 13)))
          (split-string (substring-no-properties text) "\n")))

(ert-deftest agent-hq-status-test-command-p ()
  (should (agent-hq-status--command-p "/status"))
  (should (agent-hq-status--command-p "  /status \n"))
  (should-not (agent-hq-status--command-p "/status please"))
  (should-not (agent-hq-status--command-p "/statusline"))
  (should-not (agent-hq-status--command-p "what about /status?"))
  (should-not (agent-hq-status--command-p nil)))

(ert-deftest agent-hq-status-test-login-method ()
  (should (equal (agent-hq-status--login-method
                  '((loggedIn . t) (authMethod . "claude.ai") (subscriptionType . "max")))
                 "Claude Max account"))
  (should (equal (agent-hq-status--login-method '((loggedIn . t) (authMethod . "claude.ai")))
                 "Claude account"))
  (should (equal (agent-hq-status--login-method '((loggedIn . t) (authMethod . "console")))
                 "console"))
  (should (equal (agent-hq-status--login-method '((loggedIn . :false) (authMethod . "none")))
                 "not logged in")))

(ert-deftest agent-hq-status-test-text ()
  (let* ((home (make-temp-file "agent-hq-status" t))
         (work (expand-file-name "work-config" home))
         (repo (expand-file-name "repo" home))
         (cli (agent-hq-status-test--cli home t)))
    (unwind-protect
        (progn
          (make-directory work)
          (make-directory (expand-file-name ".claude" repo) t)
          (with-temp-file (expand-file-name "CLAUDE.md" work))
          (with-temp-file (expand-file-name "CLAUDE.md" repo))
          (with-temp-file (expand-file-name ".claude/settings.local.json" repo)
            (insert "{\"autoMemoryDirectory\": \"/shared/memory\"}"))
          (let ((process-environment (cons (concat "CLAUDE_CODE_EXECUTABLE=" cli)
                                           process-environment)))
            (agent-hq-status-test--in-shell (file-name-as-directory repo) work
              (cl-letf (((symbol-function 'agent-hq-status--acp-version) (lambda () "0.110.0")))
                (let ((rows (agent-hq-status-test--rows (agent-hq-status-text))))
                  (should (equal (map-elt rows "Version")
                                 "Claude Code 2.1.330 · claude-agent-acp 0.110.0"))
                  (should (equal (map-elt rows "Session ID") "`sess-1`"))
                  (should (equal (map-elt rows "Login method") "Claude Team account"))
                  ;; The shell's account reached the CLI.
                  (should (equal (map-elt rows "Organization") work))
                  (should (equal (map-elt rows "Email") "me@example.com"))
                  (should (equal (map-elt rows "Model") "Opus 5.5 · Xhigh effort · fast mode"))
                  (should (equal (map-elt rows "Mode") "Auto"))
                  (should (equal (map-elt rows "Context") "88k/1000k (8.8%)"))
                  (should (equal (map-elt rows "Plan usage") (format "plan of %S" work)))
                  (should (equal (map-elt rows "Base URL") "`http://127.0.0.1:8787`"))
                  (should (equal (map-elt rows "Memory")
                                 (format "`%s` · `%s`"
                                         (abbreviate-file-name (expand-file-name "CLAUDE.md" work))
                                         (abbreviate-file-name (expand-file-name "CLAUDE.md" repo)))))
                  (should (equal (map-elt rows "Auto memory") "`/shared/memory`"))))))
          ;; Logged out, nothing reported yet, default account: rows say so
          ;; or are left out.
          (let ((process-environment
                 (cons (concat "CLAUDE_CODE_EXECUTABLE="
                               (agent-hq-status-test--cli
                                (make-temp-file "agent-hq-status-cli" t) nil))
                       process-environment)))
            (agent-hq-status-test--in-shell (file-name-as-directory repo) 'default
              (setf (alist-get :usage agent-shell--state) nil)
              (let ((rows (agent-hq-status-test--rows (agent-hq-status-text))))
                (should (equal (map-elt rows "Login method") "not logged in"))
                (should-not (map-elt rows "Email"))
                (should (equal (map-elt rows "Context") "nothing reported until Claude answers"))
                (should (equal (map-elt rows "Config dir")
                               (format "`%s`" (abbreviate-file-name
                                               (expand-file-name "~/.claude")))))
                (should (equal (map-elt rows "Plan usage") "plan of \"\""))))))
      (delete-directory home t))))

(ert-deftest agent-hq-status-test-output-timeout ()
  (let ((agent-hq-status-timeout 0.2))
    (should-not (agent-hq-status--output '("sleep" "5") nil))
    (should-not (agent-hq-status--output '("sh" "-c" "echo out; exit 1") nil))
    (should (equal (agent-hq-status--output '("sh" "-c" "echo out; exit 1") nil t) "out\n"))
    (should (equal (agent-hq-status--output '("sh" "-c" "echo \"$X\"") '("X=1")) "1\n"))))

(ert-deftest agent-hq-status-test-advice ()
  (let (sent shown finished)
    (cl-letf (((symbol-function 'shell-maker--current-request-id) (lambda () 7))
              ((symbol-function 'agent-hq-status-text) (lambda () "STATUS"))
              ((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args) (setq shown args)))
              ((symbol-function 'agent-shell--finish-output)
               (lambda (&rest args) (setq finished args))))
      (with-temp-buffer
        (let ((fn (lambda (&rest args) (setq sent (plist-get args :command)))))
          (setq agent-shell--state (list (cons :session '((:id . "sess-1")))
                                         (cons :request-count 0))
                agent-hq--claude-started-with 'default)
          ;; /status in a Claude shell: shown here, the turn ended, nothing sent.
          (agent-hq-status-command-a fn :command "/status" :shell-buffer (current-buffer))
          (should-not sent)
          (should (equal (plist-get shown :body) "STATUS"))
          (should (plist-get finished :success))
          (should (= (map-elt agent-shell--state :request-count) 7))
          ;; Other prompts go to the agent.
          (agent-hq-status-command-a fn :command "/status now" :shell-buffer (current-buffer))
          (should (equal sent "/status now"))
          ;; A failing status still ends the turn.
          (setq shown nil finished nil)
          (cl-letf (((symbol-function 'agent-hq-status-text) (lambda () (error "Boom"))))
            (agent-hq-status-command-a fn :command "/status" :shell-buffer (current-buffer)))
          (should (string-match-p "Boom" (plist-get shown :body)))
          (should finished)
          ;; Codex shells, and shells whose session isn't up, send it.
          (setq sent nil agent-hq--claude-started-with nil)
          (agent-hq-status-command-a fn :command "/status" :shell-buffer (current-buffer))
          (should (equal sent "/status"))
          (setq sent nil agent-hq--claude-started-with 'default
                agent-shell--state nil)
          (agent-hq-status-command-a fn :command "/status" :shell-buffer (current-buffer))
          (should (equal sent "/status")))))))

;;; agent-hq-status-test.el ends here
