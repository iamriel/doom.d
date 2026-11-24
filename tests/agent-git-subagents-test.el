;;; agent-git-subagents-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-git -l tests/agent-git-subagents-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: its few functions used here are stubbed, and
;; transcripts are written to a temporary config dir.

(require 'ert)
(require 'cl-lib)
(require 'json)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-git/agent-git.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-git/agent-git-subagents.el" dir) nil t))

(defvar comint-last-prompt nil)

;;;; Transcript lines

(defun agent-git-subagents-test--json (object)
  "Return OBJECT, an alist or plist tree, as one JSON line."
  (concat (json-encode object) "\n"))

(defun agent-git-subagents-test--user (time content &rest extra)
  "A user line at TIME with message CONTENT and EXTRA top-level fields."
  (agent-git-subagents-test--json
   `((type . "user") ,@extra
     (message . ((role . "user") (content . ,content)))
     (timestamp . ,time))))

(defun agent-git-subagents-test--assistant (time stop content &optional usage)
  "An assistant line at TIME ending with STOP, with CONTENT blocks and USAGE."
  (agent-git-subagents-test--json
   `((type . "assistant")
     (message . ((model . "claude-opus-5-5") (role . "assistant")
                 (content . ,content) (stop_reason . ,stop)
                 (usage . ,(or usage '((input_tokens . 1) (output_tokens . 1))))))
     (timestamp . ,time))))

(defun agent-git-subagents-test--parse (line)
  (json-parse-string line :object-type 'alist :null-object nil :false-object nil))

;;;; Formatting

(ert-deftest agent-git-subagents-test-format ()
  (should (equal (agent-git-subagents-format-tokens 950) "950"))
  (should (equal (agent-git-subagents-format-tokens 581800) "581.8k"))
  (should (equal (agent-git-subagents-format-tokens 1250000) "1.2m"))
  (should (equal (agent-git-subagents-format-elapsed 45.7) "45s"))
  (should (equal (agent-git-subagents-format-elapsed (+ (* 22 60) 53)) "22m 53s"))
  (should (equal (agent-git-subagents-format-elapsed (+ 3600 (* 5 60) 9)) "1h 05m")))

;;;; What a line says

(ert-deftest agent-git-subagents-test-prompt-text ()
  (let ((prompt (lambda (line)
                  (agent-git-subagents--prompt-text (agent-git-subagents-test--parse line)))))
    (should (equal (funcall prompt (agent-git-subagents-test--user "t" "Find the bug"))
                   "Find the bug"))
    ;; A teammate's message keeps who sent it, not the wrapper.
    (should (equal (funcall prompt (agent-git-subagents-test--user
                                    "t" "<teammate-message teammate_id=\"team-lead\" summary=\"Go\">\nDraft it\n</teammate-message>"))
                   "[team-lead] Draft it"))
    ;; Reminders Claude Code adds are not prompts, alone or around one.
    (should-not (funcall prompt (agent-git-subagents-test--user
                                 "t" "<system-reminder>\nOther agents\n</system-reminder>")))
    (should (equal (funcall prompt (agent-git-subagents-test--user
                                    "t" "<system-reminder>x</system-reminder>\nReal ask"))
                   "Real ask"))
    (should-not (funcall prompt (agent-git-subagents-test--user "t" "Hidden" '(isMeta . t))))
    ;; Text blocks are a prompt; tool results are not.
    (should (equal (funcall prompt (agent-git-subagents-test--user
                                    "t" [((type . "text") (text . "One")) ((type . "text") (text . "Two"))]))
                   "One\nTwo"))
    (should-not (funcall prompt (agent-git-subagents-test--user
                                 "t" [((type . "tool_result") (tool_use_id . "x") (content . "ok"))])))))

(ert-deftest agent-git-subagents-test-entry-state ()
  (let ((state (lambda (line)
                 (agent-git-subagents--entry-state (agent-git-subagents-test--parse line)))))
    (should (eq (funcall state (agent-git-subagents-test--assistant
                                "t" "end_turn" [((type . "text") (text . "Done"))]))
                'idle))
    (should (eq (funcall state (agent-git-subagents-test--assistant
                                "t" "tool_use" [((type . "tool_use") (name . "Bash") (input . ((command . "ls"))))]))
                'working))
    ;; A thinking block written ahead of its message's tool call.
    (should (eq (funcall state (agent-git-subagents-test--assistant
                                "t" "tool_use" [((type . "thinking") (thinking . ""))]))
                'working))
    (should (eq (funcall state (agent-git-subagents-test--user "t" "Go")) 'working))))

;;;; Reading what was appended

(defmacro agent-git-subagents-test--with-file (var &rest body)
  "Run BODY with VAR bound to a fresh temporary file, deleted afterwards."
  (declare (indent 1))
  `(let ((,var (make-temp-file "agent-git-subagents-test")))
     (unwind-protect (progn ,@body)
       (delete-file ,var))))

(defun agent-git-subagents-test--append (file text)
  "Append TEXT to FILE; a unibyte TEXT goes as the bytes it holds."
  (let ((coding-system-for-write (if (multibyte-string-p text) 'utf-8-unix 'binary)))
    (write-region text nil file t 'silent)))

(defun agent-git-subagents-test--read (file state)
  "Read FILE from STATE, (OFFSET . PARTIAL); return (LINES . NEXT-STATE)."
  (let (lines)
    (let ((next (agent-git-subagents--read-appended
                 file (car state) (cdr state) nil
                 (lambda () (setq lines (split-string (buffer-string) "\n" t))))))
      (cons lines (or next state)))))

(ert-deftest agent-git-subagents-test-read-appended ()
  (agent-git-subagents-test--with-file file
    ;; Written up to half of the é in "thrée".
    (agent-git-subagents-test--append file "one\ntwo café\nthr")
    (agent-git-subagents-test--append file (unibyte-string #xc3))
    (pcase-let ((`(,lines . ,state) (agent-git-subagents-test--read file '(0 . ""))))
      ;; Only complete lines, decoded; the unfinished one waits.
      (should (equal lines '("one" "two café")))
      (agent-git-subagents-test--append file (unibyte-string #xa9 ?e ?\n))
      (pcase-let ((`(,lines . ,state) (agent-git-subagents-test--read file state)))
        (should (equal lines '("thrée")))
        ;; Nothing appended: nothing read.
        (should-not (agent-git-subagents--read-appended file (car state) (cdr state) nil #'ignore))))))

(ert-deftest agent-git-subagents-test-read-tail ()
  (agent-git-subagents-test--with-file file
    (agent-git-subagents-test--append file "aaaaaaaaaa\nbbbbbbbbbb\ncccc\n")
    (let (lines)
      (agent-git-subagents--read-appended
       file 0 "" 8 (lambda () (setq lines (split-string (buffer-string) "\n" t))))
      ;; Starts at the first whole line within the last 8 bytes.
      (should (equal lines '("cccc"))))))

;;;; A session's agents

(defvar agent-git-subagents-test--session "11111111-2222-3333-4444-555555555555")

(defmacro agent-git-subagents-test--with-session (&rest body)
  "Run BODY in a shell-like buffer whose session lives in a temporary config.
`subagents' is bound to the session's subagents/ dir."
  (declare (indent 0))
  `(let* ((config (file-name-as-directory (make-temp-file "agent-git-config" t)))
          (work (file-name-as-directory (make-temp-file "agent-git-work" t)))
          (project (expand-file-name (agent-git--claude-project-key work)
                                     (expand-file-name "projects" config)))
          (subagents (expand-file-name (concat agent-git-subagents-test--session "/subagents/")
                                       project)))
     (make-directory subagents t)
     (write-region "" nil (expand-file-name (concat agent-git-subagents-test--session ".jsonl")
                                            project))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-session-id)
                    (lambda (&rest _) agent-git-subagents-test--session))
                   ((symbol-function 'agent-shell--live-input-prompt-p)
                    (lambda (prompt) (and prompt (= (cdr prompt) (point-max))))))
           (with-temp-buffer
             (setq default-directory work
                   agent-git--claude-started-with config)
             ,@body))
       (delete-directory config t)
       (delete-directory work t))))

(defun agent-git-subagents-test--agent (dir id meta &rest lines)
  "Write agent ID's META (an alist) and transcript LINES into DIR."
  (write-region (json-encode meta) nil (expand-file-name (format "agent-%s.meta.json" id) dir)
                nil 'silent)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (apply #'concat lines) nil (expand-file-name (format "agent-%s.jsonl" id) dir)
                  nil 'silent)))

(defvar agent-git-subagents-test--now nil
  "Float time stamps count back from; nil for the current time.
A whole second, so they mean exactly what they say: stamps keep no
fraction, and one taken from the clock could end up a second early.")

(defun agent-git-subagents-test--stamp (seconds-ago)
  (format-time-string "%FT%T.000Z"
                      (time-subtract agent-git-subagents-test--now seconds-ago)
                      t))

(ert-deftest agent-git-subagents-test-scan ()
  (agent-git-subagents-test--with-session
    (let* ((now (float (truncate (float-time))))
           (agent-git-subagents-test--now now))
      ;; A teammate at work on its second task, with a tool call out.
      (agent-git-subagents-test--agent
       subagents "adrafts-1"
       '((name . "drafts") (agentType . "drafts") (description . "Draft the top floors"))
       (agent-git-subagents-test--user (agent-git-subagents-test--stamp 3000) "First brief")
       (agent-git-subagents-test--assistant (agent-git-subagents-test--stamp 2900) "end_turn"
                                            [((type . "text") (text . "Done"))]
                                            '((input_tokens . 10) (output_tokens . 5)))
       (agent-git-subagents-test--user (agent-git-subagents-test--stamp 120)
                                       "<teammate-message teammate_id=\"team-lead\">\nNext\n</teammate-message>")
       (agent-git-subagents-test--assistant (agent-git-subagents-test--stamp 60) "tool_use"
                                            [((type . "tool_use") (name . "Bash") (input . ((command . "make"))))]
                                            '((input_tokens . 2) (cache_read_input_tokens . 580000)
                                              (cache_creation_input_tokens . 1000) (output_tokens . 800))))
      ;; A subagent that finished a minute ago.
      (agent-git-subagents-test--agent
       subagents "a0f1"
       '((agentType . "Explore") (description . "Trace the upload") (finished . t))
       (agent-git-subagents-test--user (agent-git-subagents-test--stamp 400) "Trace it")
       (agent-git-subagents-test--assistant (agent-git-subagents-test--stamp 60) "end_turn"
                                            [((type . "text") (text . "Report"))]
                                            '((input_tokens . 265000) (output_tokens . 389))))
      (let* ((agents (agent-git-subagents--scan))
             (drafts (gethash "adrafts-1" agent-git-subagents--agents))
             (explore (gethash "a0f1" agent-git-subagents--agents)))
        (should (equal (mapcar #'agent-git-subagents-agent-id agents) '("adrafts-1" "a0f1")))
        (should (equal (agent-git-subagents-agent-name drafts) "drafts"))
        ;; No name: the agent type.
        (should (equal (agent-git-subagents-agent-name explore) "Explore"))
        (should (eq (agent-git-subagents--status drafts now) 'working))
        (should (eq (agent-git-subagents--status explore now) 'done))
        ;; Time counts from the latest task; tokens are the latest context.
        (should (equal (agent-git-subagents--stats drafts (+ now 0.5)) "2m 00s · ↓ 581.8k tokens"))
        (should (equal (agent-git-subagents--stats explore now) "done in 5m 40s · ↓ 265.4k tokens"))
        ;; It answers: idle, and its time stops at the answer.
        (agent-git-subagents-test--append
         (agent-git-subagents-agent-file drafts)
         (agent-git-subagents-test--assistant (agent-git-subagents-test--stamp 30) "end_turn"
                                              [((type . "text") (text . "Drafted"))]
                                              '((input_tokens . 600000) (output_tokens . 100))))
        (agent-git-subagents--scan)
        (should (eq (agent-git-subagents--status drafts now) 'idle))
        (should (equal (agent-git-subagents--stats drafts now) "idle · ↓ 600.1k tokens"))
        ;; Both stepped lately, so both are listed; neither once that's long ago.
        (should (= (length (agent-git-subagents--listed agents now)) 2))
        (should-not (agent-git-subagents--listed agents (+ now agent-git-subagents-linger 100)))))))

(ert-deftest agent-git-subagents-test-new-session-starts-over ()
  (agent-git-subagents-test--with-session
    (agent-git-subagents-test--agent subagents "a1" '((agentType . "Explore"))
                                     (agent-git-subagents-test--user (agent-git-subagents-test--stamp 5) "Go"))
    (should (= (length (agent-git-subagents--scan)) 1))
    (let ((agent-git-subagents-test--session "99999999-0000-0000-0000-000000000000"))
      (should-not (agent-git-subagents--scan)))))

;;;; The list above the prompt

(ert-deftest agent-git-subagents-test-panel ()
  (let* ((now (float-time))
         (working (agent-git-subagents--make-agent
                   :id "a" :name "room29-drafts" :description "Draft rich Room 29 top floors"
                   :first (- now 3000) :started (- now 1373) :last (- now 5) :tokens 581800))
         (done (agent-git-subagents--make-agent
                :id "b" :name "Explore" :description "Trace it" :finished t
                :first (- now 100) :started (- now 100) :last (- now 40) :tokens 9000))
         (old (agent-git-subagents--make-agent
               :id "c" :name "old" :finished t :first (- now 9000) :last (- now 8000)))
         (panel (substring-no-properties
                 (agent-git-subagents--panel (list old working done) 100 now))))
    (should (equal (split-string panel "\n" t)
                   '("● main"
                     "○ room29-drafts  Draft rich Room 29 top floors 22m 53s · ↓ 581.8k tokens"
                     "○ Explore        Trace it done in 1m 00s · ↓ 9.0k tokens")))
    (should-not (agent-git-subagents--panel (list old) 100 now))
    ;; Past the row limit, the rest are counted, working ones kept.
    (let ((agent-git-subagents-max-rows 1))
      (should (equal (split-string (substring-no-properties
                                    (agent-git-subagents--panel (list old working done) 100 now))
                                   "\n" t)
                     '("● main"
                       "○ room29-drafts  Draft rich Room 29 top floors 22m 53s · ↓ 581.8k tokens"
                       "  … 1 more"))))))

(ert-deftest agent-git-subagents-test-draw-above-prompt ()
  (cl-letf (((symbol-function 'agent-shell--live-input-prompt-p)
             (lambda (prompt) (and prompt (= (cdr prompt) (point-max))))))
    (with-temp-buffer
      (insert "answer\n\n")
      (let ((start (point-marker)))
        (insert "Claude> ")
        (setq-local comint-last-prompt (cons start (point-marker)))
        (agent-git-subagents--draw "● main\n○ x")
        (should (= (overlay-start agent-git-subagents--overlay) (marker-position start)))
        (should (equal (overlay-get agent-git-subagents--overlay 'before-string) "● main\n○ x\n"))
        ;; Output written above the prompt goes above the list too.
        (save-excursion
          (goto-char start)
          (insert "more\n"))
        (should (equal (buffer-substring (point-min) (overlay-start agent-git-subagents--overlay))
                       "answer\n\nmore\n"))
        (should (equal (buffer-substring (overlay-start agent-git-subagents--overlay) (point-max))
                       "Claude> "))
        ;; A new prompt: the list moves to it.
        (insert "hi\n")
        (let ((next (point-marker)))
          (insert "Claude> ")
          (setq-local comint-last-prompt (cons next (point-marker)))
          (agent-git-subagents--follow-prompt)
          (should (= (overlay-start agent-git-subagents--overlay) (marker-position next))))
        (agent-git-subagents--draw nil)
        (should-not agent-git-subagents--overlay)))))

;;;; Transcripts

(ert-deftest agent-git-subagents-test-render ()
  (with-temp-buffer
    (dolist (line (list (agent-git-subagents-test--user "t" "Fix /w/src/a.el")
                        (agent-git-subagents-test--assistant
                         "t" "tool_use"
                         [((type . "thinking") (thinking . "hmm"))
                          ((type . "text") (text . "Reading it."))
                          ((type . "tool_use") (name . "Read") (input . ((file_path . "/w/src/a.el"))))])
                        (agent-git-subagents-test--user
                         "t" [((type . "tool_result") (tool_use_id . "x")
                               (content . "1\n2\n3\n4\n5"))])
                        (agent-git-subagents-test--assistant
                         "t" "tool_use" [((type . "tool_use") (name . "Bash") (input . ((command . "make test"))))])
                        (agent-git-subagents-test--user
                         "t" [((type . "tool_result") (tool_use_id . "y") (is_error . t)
                               (content . [((type . "text") (text . "boom"))]))])))
      (agent-git-subagents--insert-entry (agent-git-subagents-test--parse line) "/w/"))
    (should (equal (buffer-string)
                   (concat "❯ Fix /w/src/a.el\n\n"
                           "Reading it.\n\n"
                           "● Read(src/a.el)\n"
                           "  ⎿  1\n     2\n     3\n     … +2 lines\n\n"
                           "● Bash(make test)\n"
                           "  ⎿  boom\n\n")))))

(ert-deftest agent-git-subagents-test-ring ()
  (agent-git-subagents-test--with-session
    (agent-git-subagents-test--agent subagents "a1" '((agentType . "One"))
                                     (agent-git-subagents-test--user (agent-git-subagents-test--stamp 20) "Go"))
    (agent-git-subagents-test--agent subagents "a2" '((agentType . "Two"))
                                     (agent-git-subagents-test--user (agent-git-subagents-test--stamp 10) "Go"))
    (agent-git-subagents-test--agent subagents "a3" '((agentType . "Old") (finished . t))
                                     (agent-git-subagents-test--user (agent-git-subagents-test--stamp 9000) "Go"))
    (set-file-times (expand-file-name "agent-a3.jsonl" subagents) (time-subtract nil 9000))
    (let ((shell (current-buffer)))
      (should (equal (agent-git-subagents--ring shell nil) '(nil "a1" "a2")))
      ;; The agent shown stays in the ring after it leaves the list.
      (should (equal (agent-git-subagents--ring shell "a3") '(nil "a3" "a1" "a2"))))))

;;; agent-git-subagents-test.el ends here
