;;; agent-hq-subagents-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-subagents-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: its few functions used here are stubbed, and
;; transcripts are written to a temporary config dir.

(require 'ert)
(require 'cl-lib)
(require 'json)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq.el" dir) nil t)
  (load (expand-file-name "../lisp/agent-hq/agent-hq-subagents.el" dir) nil t))

(defvar comint-last-prompt nil)

;;;; Transcript lines

(defun agent-hq-subagents-test--json (object)
  "Return OBJECT, an alist or plist tree, as one JSON line."
  (concat (json-encode object) "\n"))

(defun agent-hq-subagents-test--user (time content &rest extra)
  "A user line at TIME with message CONTENT and EXTRA top-level fields."
  (agent-hq-subagents-test--json
   `((type . "user") ,@extra
     (message . ((role . "user") (content . ,content)))
     (timestamp . ,time))))

(defun agent-hq-subagents-test--assistant (time stop content &optional usage)
  "An assistant line at TIME ending with STOP, with CONTENT blocks and USAGE."
  (agent-hq-subagents-test--json
   `((type . "assistant")
     (message . ((model . "claude-opus-5-5") (role . "assistant")
                 (content . ,content) (stop_reason . ,stop)
                 (usage . ,(or usage '((input_tokens . 1) (output_tokens . 1))))))
     (timestamp . ,time))))

(defun agent-hq-subagents-test--parse (line)
  (json-parse-string line :object-type 'alist :null-object nil :false-object nil))

;;;; Formatting

(ert-deftest agent-hq-subagents-test-format ()
  (should (equal (agent-hq-subagents-format-tokens 950) "950"))
  (should (equal (agent-hq-subagents-format-tokens 581800) "581.8k"))
  (should (equal (agent-hq-subagents-format-tokens 1250000) "1.2m"))
  (should (equal (agent-hq-subagents-format-elapsed 45.7) "45s"))
  (should (equal (agent-hq-subagents-format-elapsed (+ (* 22 60) 53)) "22m 53s"))
  (should (equal (agent-hq-subagents-format-elapsed (+ 3600 (* 5 60) 9)) "1h 05m")))

;;;; What a line says

(ert-deftest agent-hq-subagents-test-prompt-text ()
  (let ((prompt (lambda (line)
                  (agent-hq-subagents--prompt-text (agent-hq-subagents-test--parse line)))))
    (should (equal (funcall prompt (agent-hq-subagents-test--user "t" "Find the bug"))
                   "Find the bug"))
    ;; A teammate's message keeps who sent it, not the wrapper.
    (should (equal (funcall prompt (agent-hq-subagents-test--user
                                    "t" "<teammate-message teammate_id=\"team-lead\" summary=\"Go\">\nDraft it\n</teammate-message>"))
                   "[team-lead] Draft it"))
    ;; Reminders Claude Code adds are not prompts, alone or around one.
    (should-not (funcall prompt (agent-hq-subagents-test--user
                                 "t" "<system-reminder>\nOther agents\n</system-reminder>")))
    (should (equal (funcall prompt (agent-hq-subagents-test--user
                                    "t" "<system-reminder>x</system-reminder>\nReal ask"))
                   "Real ask"))
    (should-not (funcall prompt (agent-hq-subagents-test--user "t" "Hidden" '(isMeta . t))))
    ;; Text blocks are a prompt; tool results are not.
    (should (equal (funcall prompt (agent-hq-subagents-test--user
                                    "t" [((type . "text") (text . "One")) ((type . "text") (text . "Two"))]))
                   "One\nTwo"))
    (should-not (funcall prompt (agent-hq-subagents-test--user
                                 "t" [((type . "tool_result") (tool_use_id . "x") (content . "ok"))])))))

(ert-deftest agent-hq-subagents-test-entry-state ()
  (let ((state (lambda (line)
                 (agent-hq-subagents--entry-state (agent-hq-subagents-test--parse line)))))
    (should (eq (funcall state (agent-hq-subagents-test--assistant
                                "t" "end_turn" [((type . "text") (text . "Done"))]))
                'idle))
    (should (eq (funcall state (agent-hq-subagents-test--assistant
                                "t" "tool_use" [((type . "tool_use") (name . "Bash") (input . ((command . "ls"))))]))
                'working))
    ;; A thinking block written ahead of its message's tool call.
    (should (eq (funcall state (agent-hq-subagents-test--assistant
                                "t" "tool_use" [((type . "thinking") (thinking . ""))]))
                'working))
    (should (eq (funcall state (agent-hq-subagents-test--user "t" "Go")) 'working))))

;;;; Reading what was appended

(defmacro agent-hq-subagents-test--with-file (var &rest body)
  "Run BODY with VAR bound to a fresh temporary file, deleted afterwards."
  (declare (indent 1))
  `(let ((,var (make-temp-file "agent-hq-subagents-test")))
     (unwind-protect (progn ,@body)
       (delete-file ,var))))

(defun agent-hq-subagents-test--append (file text)
  "Append TEXT to FILE; a unibyte TEXT goes as the bytes it holds."
  (let ((coding-system-for-write (if (multibyte-string-p text) 'utf-8-unix 'binary)))
    (write-region text nil file t 'silent)))

(ert-deftest agent-hq-subagents-test-read-raw ()
  "Raw, the lines come as bytes, for a search that needn't decode them."
  (agent-hq-subagents-test--with-file file
    (agent-hq-subagents-test--append file "café\nhalf")
    (let (text multibyte)
      (should (equal (agent-hq-subagents--read-appended
                      file 0 "" nil
                      (lambda () (setq text (buffer-string)
                                       multibyte enable-multibyte-characters))
                      'raw)
                     '(10 . "half")))
      (should-not multibyte)
      (should (equal (decode-coding-string text 'utf-8) "café\n")))))

(defun agent-hq-subagents-test--read (file state)
  "Read FILE from STATE, (OFFSET . PARTIAL); return (LINES . NEXT-STATE)."
  (let (lines)
    (let ((next (agent-hq-subagents--read-appended
                 file (car state) (cdr state) nil
                 (lambda () (setq lines (split-string (buffer-string) "\n" t))))))
      (cons lines (or next state)))))

(ert-deftest agent-hq-subagents-test-read-appended ()
  (agent-hq-subagents-test--with-file file
    ;; Written up to half of the é in "thrée".
    (agent-hq-subagents-test--append file "one\ntwo café\nthr")
    (agent-hq-subagents-test--append file (unibyte-string #xc3))
    (pcase-let ((`(,lines . ,state) (agent-hq-subagents-test--read file '(0 . ""))))
      ;; Only complete lines, decoded; the unfinished one waits.
      (should (equal lines '("one" "two café")))
      (agent-hq-subagents-test--append file (unibyte-string #xa9 ?e ?\n))
      (pcase-let ((`(,lines . ,state) (agent-hq-subagents-test--read file state)))
        (should (equal lines '("thrée")))
        ;; Nothing appended: nothing read.
        (should-not (agent-hq-subagents--read-appended file (car state) (cdr state) nil #'ignore))))))

(ert-deftest agent-hq-subagents-test-read-tail ()
  (agent-hq-subagents-test--with-file file
    (agent-hq-subagents-test--append file "aaaaaaaaaa\nbbbbbbbbbb\ncccc\n")
    (let (lines)
      (agent-hq-subagents--read-appended
       file 0 "" 8 (lambda () (setq lines (split-string (buffer-string) "\n" t))))
      ;; Starts at the first whole line within the last 8 bytes.
      (should (equal lines '("cccc"))))))

;;;; A session's agents

(defvar agent-hq-subagents-test--session "11111111-2222-3333-4444-555555555555")

(defmacro agent-hq-subagents-test--with-session (&rest body)
  "Run BODY in a shell-like buffer whose session lives in a temporary config.
`subagents' is bound to the session's subagents/ dir, `transcript' to its
transcript, empty at first."
  (declare (indent 0))
  `(let* ((config (file-name-as-directory (make-temp-file "agent-hq-config" t)))
          (work (file-name-as-directory (make-temp-file "agent-hq-work" t)))
          (project (expand-file-name (agent-hq--claude-project-key work)
                                     (expand-file-name "projects" config)))
          (subagents (expand-file-name (concat agent-hq-subagents-test--session "/subagents/")
                                       project))
          (transcript (expand-file-name (concat agent-hq-subagents-test--session ".jsonl")
                                        project)))
     (ignore transcript)
     (make-directory subagents t)
     (write-region "" nil transcript)
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-session-id)
                    (lambda (&rest _) agent-hq-subagents-test--session))
                   ((symbol-function 'agent-shell--live-input-prompt-p)
                    (lambda (prompt) (and prompt (= (cdr prompt) (point-max))))))
           (with-temp-buffer
             (setq default-directory work
                   agent-hq--claude-started-with config)
             ,@body))
       (delete-directory config t)
       (delete-directory work t))))

(defun agent-hq-subagents-test--agent (dir id meta &rest lines)
  "Write agent ID's META (an alist) and transcript LINES into DIR."
  (write-region (json-encode meta) nil (expand-file-name (format "agent-%s.meta.json" id) dir)
                nil 'silent)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (apply #'concat lines) nil (expand-file-name (format "agent-%s.jsonl" id) dir)
                  nil 'silent)))

(defvar agent-hq-subagents-test--now nil
  "Float time stamps count back from; nil for the current time.
A whole second, so they mean exactly what they say: stamps keep no
fraction, and one taken from the clock could end up a second early.")

(defun agent-hq-subagents-test--stamp (seconds-ago)
  (format-time-string "%FT%T.000Z"
                      (time-subtract agent-hq-subagents-test--now seconds-ago)
                      t))

(ert-deftest agent-hq-subagents-test-scan ()
  (agent-hq-subagents-test--with-session
    (let* ((now (float (truncate (float-time))))
           (agent-hq-subagents-test--now now))
      ;; A teammate at work on its second task, with a tool call out.
      (agent-hq-subagents-test--agent
       subagents "adrafts-1"
       '((name . "drafts") (agentType . "drafts") (description . "Draft the top floors"))
       (agent-hq-subagents-test--user (agent-hq-subagents-test--stamp 3000) "First brief")
       (agent-hq-subagents-test--assistant (agent-hq-subagents-test--stamp 2900) "end_turn"
                                            [((type . "text") (text . "Done"))]
                                            '((input_tokens . 10) (output_tokens . 5)))
       (agent-hq-subagents-test--user (agent-hq-subagents-test--stamp 120)
                                       "<teammate-message teammate_id=\"team-lead\">\nNext\n</teammate-message>")
       (agent-hq-subagents-test--assistant (agent-hq-subagents-test--stamp 60) "tool_use"
                                            [((type . "tool_use") (name . "Bash") (input . ((command . "make"))))]
                                            '((input_tokens . 2) (cache_read_input_tokens . 580000)
                                              (cache_creation_input_tokens . 1000) (output_tokens . 800))))
      ;; A subagent that finished a minute ago.
      (agent-hq-subagents-test--agent
       subagents "a0f1"
       '((agentType . "Explore") (description . "Trace the upload") (finished . t))
       (agent-hq-subagents-test--user (agent-hq-subagents-test--stamp 400) "Trace it")
       (agent-hq-subagents-test--assistant (agent-hq-subagents-test--stamp 60) "end_turn"
                                            [((type . "text") (text . "Report"))]
                                            '((input_tokens . 265000) (output_tokens . 389))))
      (let* ((agents (agent-hq-subagents--scan))
             (drafts (gethash "adrafts-1" agent-hq-subagents--agents))
             (explore (gethash "a0f1" agent-hq-subagents--agents)))
        (should (equal (mapcar #'agent-hq-subagents-agent-id agents) '("adrafts-1" "a0f1")))
        (should (equal (agent-hq-subagents-agent-name drafts) "drafts"))
        ;; No name: the agent type.
        (should (equal (agent-hq-subagents-agent-name explore) "Explore"))
        (should (eq (agent-hq-subagents--status drafts now) 'working))
        (should (eq (agent-hq-subagents--status explore now) 'done))
        ;; Time counts from the latest task; tokens are the latest context.
        (should (equal (agent-hq-subagents--stats drafts (+ now 0.5)) "2m 00s · ↓ 581.8k tokens"))
        (should (equal (agent-hq-subagents--stats explore now) "done in 5m 40s · ↓ 265.4k tokens"))
        ;; It answers: idle, and its time stops at the answer.
        (agent-hq-subagents-test--append
         (agent-hq-subagents-agent-file drafts)
         (agent-hq-subagents-test--assistant (agent-hq-subagents-test--stamp 30) "end_turn"
                                              [((type . "text") (text . "Drafted"))]
                                              '((input_tokens . 600000) (output_tokens . 100))))
        (agent-hq-subagents--scan)
        (should (eq (agent-hq-subagents--status drafts now) 'idle))
        (should (equal (agent-hq-subagents--stats drafts now) "idle · ↓ 600.1k tokens"))
        ;; Both stepped lately, so both are listed; neither once that's long ago.
        (should (= (length (agent-hq-subagents--listed agents now)) 2))
        (should-not (agent-hq-subagents--listed agents (+ now agent-hq-subagents-linger 100)))))))

(ert-deftest agent-hq-subagents-test-new-session-starts-over ()
  (agent-hq-subagents-test--with-session
    (agent-hq-subagents-test--agent subagents "a1" '((agentType . "Explore"))
                                     (agent-hq-subagents-test--user (agent-hq-subagents-test--stamp 5) "Go"))
    (should (= (length (agent-hq-subagents--scan)) 1))
    (let ((agent-hq-subagents-test--session "99999999-0000-0000-0000-000000000000"))
      (should-not (agent-hq-subagents--scan)))))

(ert-deftest agent-hq-subagents-test-session-moves ()
  "A session that enters a worktree is followed to its new folder."
  (agent-hq-subagents-test--with-session
    (should (equal (agent-hq-subagents--dir)
                   (file-name-as-directory
                    (expand-file-name agent-hq-subagents-test--session project))))
    (let ((elsewhere (expand-file-name "-elsewhere" (file-name-directory project))))
      (make-directory elsewhere)
      (rename-file transcript (expand-file-name (file-name-nondirectory transcript) elsewhere))
      (should (equal (agent-hq-subagents--dir)
                     (file-name-as-directory
                      (expand-file-name agent-hq-subagents-test--session elsewhere)))))))

;;;; A session's background tasks

(defun agent-hq-subagents-test--call (time id name input)
  "An assistant line at TIME that calls tool NAME with INPUT, as call ID."
  (agent-hq-subagents-test--assistant
   time "tool_use" `[((type . "tool_use") (id . ,id) (name . ,name) (input . ,input))]))

(defun agent-hq-subagents-test--result (time id text result)
  "A user line at TIME that answers call ID with TEXT; RESULT is its toolUseResult."
  (agent-hq-subagents-test--user
   time `[((tool_use_id . ,id) (type . "tool_result") (content . ,text))]
   `(toolUseResult . ,result)))

(defun agent-hq-subagents-test--queued (time text)
  "A line at TIME of the queue Claude Code keeps, holding TEXT."
  (agent-hq-subagents-test--json
   `((type . "queue-operation") (operation . "enqueue") (timestamp . ,time) (content . ,text))))

(defun agent-hq-subagents-test--notification (id status summary &optional event)
  "Task ID's notification: its end with STATUS, or with none an EVENT."
  (concat "<task-notification>\n<task-id>" id "</task-id>\n"
          (when status
            (format "<output-file>/tmp/x/tasks/%s.output</output-file>\n<status>%s</status>\n"
                    id status))
          "<summary>" summary "</summary>\n"
          (when event (concat "<event>" event "</event>\n"))
          "</task-notification>"))

(defun agent-hq-subagents-test--write-tasks (transcript)
  "Write into TRANSCRIPT the start of four background tasks, and some ends."
  (let ((stamp #'agent-hq-subagents-test--stamp))
    (agent-hq-subagents-test--append
     transcript
     (concat
      ;; The gate, run in the background ten minutes ago.
      (agent-hq-subagents-test--call (funcall stamp 600) "toolu_gate" "Bash"
                                     '((command . "make gate > gate.log")
                                       (description . "Run the gate") (run_in_background . t)))
      (agent-hq-subagents-test--result
       (funcall stamp 600) "toolu_gate"
       "Command running in background with ID: bgate. Output is being written to: /tmp/x/tasks/bgate.output. You will be notified when it completes."
       '((stdout . "") (backgroundTaskId . "bgate")))
      ;; Tests too slow for the foreground, moved to the background minutes
      ;; after another call.
      (agent-hq-subagents-test--call (funcall stamp 500) "toolu_slow" "Bash" '((command . "npm test")))
      (agent-hq-subagents-test--call (funcall stamp 450) "toolu_ls" "Bash" '((command . "ls")))
      (agent-hq-subagents-test--result (funcall stamp 380) "toolu_slow" "Moved to the background"
                                       '((backgroundTaskId . "bslow") (timedOutAfterMs . 120000)))
      ;; A watch for ten minutes, and one for one minute.
      (agent-hq-subagents-test--call (funcall stamp 300) "toolu_mon" "Monitor"
                                     '((description . "Watch the log") (command . "tail -F log")))
      (agent-hq-subagents-test--result (funcall stamp 300) "toolu_mon" "Monitor started (task bmon)"
                                       '((taskId . "bmon") (timeoutMs . 600000)
                                         (persistent . :json-false)))
      (agent-hq-subagents-test--call (funcall stamp 250) "toolu_short" "Monitor"
                                     '((description . "Wait for the phone") (command . "wait-phone")))
      (agent-hq-subagents-test--result (funcall stamp 250) "toolu_short" "Monitor started (task bshort)"
                                       '((taskId . "bshort") (timeoutMs . 60000)
                                         (persistent . :json-false)))
      ;; A to-do item is no background task.
      (agent-hq-subagents-test--result (funcall stamp 240) "toolu_todo" "Task #1 created"
                                       '((taskId . "1")))
      ;; The tests fail; the watch sees an event and goes on.
      (agent-hq-subagents-test--queued
       (funcall stamp 200)
       (agent-hq-subagents-test--notification
        "bslow" "failed" "Background command \"npm test\" failed with exit code 1"))
      (agent-hq-subagents-test--queued
       (funcall stamp 150)
       (agent-hq-subagents-test--notification
        "bmon" nil "Monitor event: \"Watch the log\"" "ERROR boom"))))))

(ert-deftest agent-hq-subagents-test-tasks ()
  (agent-hq-subagents-test--with-session
    (let* ((now (float (truncate (float-time))))
           (agent-hq-subagents-test--now now))
      (agent-hq-subagents-test--write-tasks transcript)
      ;; The agent process started an hour ago, before all of them.
      (cl-letf (((symbol-function 'agent-hq-subagents--alive-since) (lambda () (- now 3600))))
        (let* ((all (agent-hq-subagents--scan))
               (task (lambda (id) (gethash id agent-hq-subagents--tasks)))
               (stats (lambda (id) (agent-hq-subagents--stats (funcall task id) now))))
          (should (equal (mapcar #'agent-hq-subagents-agent-id all)
                         '("bgate" "bslow" "bmon" "bshort")))
          ;; Kept apart from the subagents, which other modules count.
          (should (zerop (hash-table-count agent-hq-subagents--agents)))
          (let ((gate (funcall task "bgate")))
            (should (equal (agent-hq-subagents-agent-name gate) "bash"))
            (should (equal (agent-hq-subagents-agent-description gate) "Run the gate"))
            (should (equal (agent-hq-subagents-agent-command gate) "make gate > gate.log"))
            (should (equal (agent-hq-subagents-agent-file gate) "/tmp/x/tasks/bgate.output"))
            (should (eq (agent-hq-subagents--status gate now) 'working)))
          (should (equal (funcall stats "bgate") "10m 00s"))
          ;; No description: its command.  Its time runs from the call.
          (should (equal (agent-hq-subagents-agent-description (funcall task "bslow")) "npm test"))
          (should (equal (funcall stats "bslow") "failed in 5m 00s · exit 1"))
          (should (equal (agent-hq-subagents-agent-file (funcall task "bslow"))
                         "/tmp/x/tasks/bslow.output"))
          ;; An event doesn't end a watch; its timeout does.
          (should (equal (agent-hq-subagents-agent-name (funcall task "bmon")) "monitor"))
          (should (equal (funcall stats "bmon") "5m 00s"))
          (should (equal (funcall stats "bshort") "expired after 1m 00s"))
          (should (equal (split-string (substring-no-properties
                                        (agent-hq-subagents--panel all 100 now))
                                       "\n" t)
                         '("● main"
                           "○ bash     Run the gate 10m 00s"
                           "○ bash     npm test failed in 5m 00s · exit 1"
                           "○ monitor  Watch the log 5m 00s"
                           "○ monitor  Wait for the phone expired after 1m 00s")))
          ;; The gate passes, and Claude stops the watch.
          (agent-hq-subagents-test--append
           transcript
           (concat (agent-hq-subagents-test--queued
                    (agent-hq-subagents-test--stamp 10)
                    (agent-hq-subagents-test--notification
                     "bgate" "completed" "Background command \"Run the gate\" completed (exit code 0)"))
                   (agent-hq-subagents-test--call (agent-hq-subagents-test--stamp 5) "toolu_stop"
                                                  "TaskStop" '((task_id . "bmon")))
                   (agent-hq-subagents-test--result
                    (agent-hq-subagents-test--stamp 5) "toolu_stop" "Stopped"
                    '((message . "Successfully stopped task: bmon (tail -F log)")
                      (task_id . "bmon") (task_type . "local_bash")))))
          (agent-hq-subagents--scan)
          (should (equal (funcall stats "bgate") "done in 9m 50s"))
          (should (equal (funcall stats "bmon") "stopped after 4m 55s"))
          ;; Ended ones stay listed a while, as agents do.
          (should (= (length (agent-hq-subagents--listed (agent-hq-subagents--scan) now)) 4))
          (should-not (agent-hq-subagents--listed (agent-hq-subagents--scan)
                                                  (+ now agent-hq-subagents-linger 100))))))))

(ert-deftest agent-hq-subagents-test-tasks-need-their-process ()
  "A task runs only while the agent process that started it does."
  (agent-hq-subagents-test--with-session
    (let* ((now (float (truncate (float-time))))
           (agent-hq-subagents-test--now now)
           (since nil))
      (agent-hq-subagents-test--write-tasks transcript)
      (cl-letf (((symbol-function 'agent-hq-subagents--alive-since) (lambda () since)))
        (let ((gate (lambda ()
                      (agent-hq-subagents--scan)
                      (gethash "bgate" agent-hq-subagents--tasks))))
          ;; No process: the shell's agent is gone, and its tasks with it.
          (should (eq (agent-hq-subagents--status (funcall gate) now) 'done))
          (should (equal (agent-hq-subagents--stats (funcall gate) now) "ended"))
          (should-not (agent-hq-subagents--listed-p (funcall gate) now))
          ;; A process started after it: a resumed session's task, long gone.
          (setq since (- now 60))
          (should (eq (agent-hq-subagents--status (funcall gate) now) 'done))
          ;; The process that started it.
          (setq since (- now 3600))
          (should (eq (agent-hq-subagents--status (funcall gate) now) 'working)))))))

(defvar agent-shell--state)

(ert-deftest agent-hq-subagents-test-alive-since ()
  "When the shell's agent process started, while it runs."
  (with-temp-buffer
    (let ((process (make-process :name "agent-hq-test-agent" :command '("sleep" "30")
                                 :noquery t)))
      (unwind-protect
          (progn
            (setq-local agent-shell--state `((:client . ((:process . ,process)))))
            (should (< (abs (- (agent-hq-subagents--alive-since) (float-time))) 5))
            (delete-process process)
            (should-not (agent-hq-subagents--alive-since)))
        (delete-process process)))
    (setq-local agent-shell--state nil)
    (should-not (agent-hq-subagents--alive-since))))

(ert-deftest agent-hq-subagents-test-task-view ()
  "A task opens on its command, then its output as it grows."
  (agent-hq-subagents-test--with-file output
    (let* ((now (float-time))
           (task (agent-hq-subagents--make-agent
                  :id "bgate" :kind 'bash :name "bash" :description "Run the gate"
                  :command "make gate" :file output :first (- now 60) :started (- now 60)))
           (text (lambda (view)
                   (with-current-buffer view
                     (buffer-substring-no-properties (point-min) (point-max)))))
           view)
      (agent-hq-subagents-test--append output "line one\n\e[32mgreen\e[0m\n50%\r100%\r\nhalf")
      (with-temp-buffer
        (setq agent-hq-subagents--tasks (make-hash-table :test #'equal))
        (puthash "bgate" task agent-hq-subagents--tasks)
        (unwind-protect
            (progn
              (setq view (agent-hq-subagents--view-buffer (current-buffer) task))
              (agent-hq-subagents--view-update view)
              ;; No colors, each line as last drawn; an unfinished one waits.
              (should (equal (funcall text view) "$ make gate\n\nline one\ngreen\n100%\n"))
              (agent-hq-subagents-test--append output " done\n")
              (agent-hq-subagents--view-update view)
              (should (equal (funcall text view) "$ make gate\n\nline one\ngreen\n100%\nhalf done\n"))
              (should (string-match-p "● bash  Run the gate  1m 0[0-9]s"
                                      (with-current-buffer view
                                        (agent-hq-subagents--view-header)))))
          (when (buffer-live-p view)
            (kill-buffer view)))))))

;;;; The list above the prompt

(ert-deftest agent-hq-subagents-test-panel ()
  (let* ((now (float-time))
         (working (agent-hq-subagents--make-agent
                   :id "a" :name "room29-drafts" :description "Draft rich Room 29 top floors"
                   :first (- now 3000) :started (- now 1373) :last (- now 5) :tokens 581800))
         (done (agent-hq-subagents--make-agent
                :id "b" :name "Explore" :description "Trace it" :finished t
                :first (- now 100) :started (- now 100) :last (- now 40) :tokens 9000))
         (old (agent-hq-subagents--make-agent
               :id "c" :name "old" :finished t :first (- now 9000) :last (- now 8000)))
         (panel (substring-no-properties
                 (agent-hq-subagents--panel (list old working done) 100 now))))
    (should (equal (split-string panel "\n" t)
                   '("● main"
                     "○ room29-drafts  Draft rich Room 29 top floors 22m 53s · ↓ 581.8k tokens"
                     "○ Explore        Trace it done in 1m 00s · ↓ 9.0k tokens")))
    ;; Only the working agent's ○ is marked to spin.
    (let ((panel (agent-hq-subagents--panel (list old working done) 100 now)))
      (should (equal (mapcar (lambda (row)
                               (get-text-property 0 'agent-hq-subagents-spin row))
                             (cdr (split-string panel "\n" t)))
                     '(t nil))))
    (should-not (agent-hq-subagents--panel (list old) 100 now))
    ;; Past the row limit, the rest are counted, working ones kept.
    (let ((agent-hq-subagents-max-rows 1))
      (should (equal (split-string (substring-no-properties
                                    (agent-hq-subagents--panel (list old working done) 100 now))
                                   "\n" t)
                     '("● main"
                       "○ room29-drafts  Draft rich Room 29 top floors 22m 53s · ↓ 581.8k tokens"
                       "  … 1 more"))))))

(ert-deftest agent-hq-subagents-test-draw-above-prompt ()
  (cl-letf (((symbol-function 'agent-shell--live-input-prompt-p)
             (lambda (prompt) (and prompt (= (cdr prompt) (point-max))))))
    (with-temp-buffer
      (insert "answer\n\n")
      (let ((start (point-marker)))
        (insert "Claude> ")
        (setq-local comint-last-prompt (cons start (point-marker)))
        (agent-hq-subagents--draw "● main\n○ x")
        (should (= (overlay-start agent-hq-subagents--overlay) (marker-position start)))
        (should (equal (overlay-get agent-hq-subagents--overlay 'before-string) "● main\n○ x\n"))
        ;; Output written above the prompt goes above the list too.
        (save-excursion
          (goto-char start)
          (insert "more\n"))
        (should (equal (buffer-substring (point-min) (overlay-start agent-hq-subagents--overlay))
                       "answer\n\nmore\n"))
        (should (equal (buffer-substring (overlay-start agent-hq-subagents--overlay) (point-max))
                       "Claude> "))
        ;; A new prompt: the list moves to it.
        (insert "hi\n")
        (let ((next (point-marker)))
          (insert "Claude> ")
          (setq-local comint-last-prompt (cons next (point-marker)))
          (agent-hq-subagents--follow-prompt)
          (should (= (overlay-start agent-hq-subagents--overlay) (marker-position next))))
        (agent-hq-subagents--draw nil)
        (should-not agent-hq-subagents--overlay)))))

(defun agent-hq-subagents-test--icons ()
  "Return how this shell's list shows each agent's icon, top to bottom."
  (let ((string (overlay-get agent-hq-subagents--overlay 'before-string))
        (position 0)
        icons)
    (while (setq position (string-search "○" string position))
      (push (or (get-text-property position 'display string) "○") icons)
      (setq position (1+ position)))
    (nreverse icons)))

(ert-deftest agent-hq-subagents-test-spin ()
  (let* ((now (float-time))
         (working (agent-hq-subagents--make-agent
                   :id "a" :name "drafts" :first (- now 60) :started (- now 60) :last (- now 1)))
         (done (agent-hq-subagents--make-agent
                :id "b" :name "explore" :finished t :first (- now 60) :last (- now 30)))
         (panel (lambda () (agent-hq-subagents--panel (list working done) 80 now)))
         (agent-hq-spinner-frames '("1" "2" "3"))
         (agent-hq-subagents--spinning nil)
         (agent-hq-subagents--beat 0)
         ;; The shared clock, of this test's own.
         (agent-hq-clock--animations nil)
         (agent-hq-clock--timer nil)
         (beat 0)
         (running (lambda () (agent-hq-clock-running-p 'agent-hq-subagents)))
         (window (selected-window))
         (elsewhere (window-buffer window)))
    (cl-letf (((symbol-function 'agent-shell--live-input-prompt-p)
               (lambda (prompt) (and prompt (= (cdr prompt) (point-max))))))
      (cl-flet ((tick () (agent-hq-subagents--spin-tick (cl-incf beat))))
        (unwind-protect
            (with-temp-buffer
              (set-window-buffer window (current-buffer))
              (insert "Claude> ")
              (setq-local comint-last-prompt (cons (copy-marker 1) (point-marker)))
              (agent-hq-subagents--draw (funcall panel))
              ;; The agent at work spins; the one done stays a still ○.
              (should (equal (agent-hq-subagents-test--icons) '("1" "○")))
              (should (equal agent-hq-subagents--spinning (list (current-buffer))))
              (should (funcall running))
              (tick)
              (should (equal (agent-hq-subagents-test--icons) '("2" "○")))
              ;; Frames are made once: a turn later, the same strings show.
              (let ((shown (overlay-get agent-hq-subagents--overlay 'before-string)))
                (dotimes (_ 3) (tick))
                (should (eq (overlay-get agent-hq-subagents--overlay 'before-string) shown)))
              ;; Drawing the same list again keeps them, and the frame shown.
              (let ((frames agent-hq-subagents--frames))
                (agent-hq-subagents--draw (funcall panel))
                (should (eq agent-hq-subagents--frames frames))
                (should (equal (agent-hq-subagents-test--icons) '("2" "○"))))
              ;; Mid-redisplay, the frame stays.
              (let ((inhibit-redisplay t))
                (tick))
              (should (equal (agent-hq-subagents-test--icons) '("2" "○")))
              ;; Out of view, the clock stops; drawn in view again, it restarts.
              (set-window-buffer window elsewhere)
              (tick)
              (should-not (funcall running))
              (agent-hq-subagents--draw (funcall panel))
              (should-not (funcall running))
              (set-window-buffer window (current-buffer))
              (agent-hq-subagents--draw (funcall panel))
              (should (funcall running))
              ;; A shell killed while spinning is let go.
              (let ((gone (generate-new-buffer " gone")))
                (kill-buffer gone)
                (push gone agent-hq-subagents--spinning)
                (tick)
                (should (equal agent-hq-subagents--spinning (list (current-buffer)))))
              ;; Once the agent is done, nothing spins and the clock stops.
              (setf (agent-hq-subagents-agent-finished working) t)
              (agent-hq-subagents--draw (funcall panel))
              (should (equal (agent-hq-subagents-test--icons) '("○" "○")))
              (should-not agent-hq-subagents--spinning)
              (tick)
              (should-not (funcall running)))
          (set-window-buffer window elsewhere)
          (when (timerp agent-hq-clock--timer)
            (cancel-timer agent-hq-clock--timer)))))))

(ert-deftest agent-hq-subagents-test-task-spins-while-busy ()
  "A background task's ○ spins only while its shell's turn runs."
  (let* ((now (float-time))
         (task (agent-hq-subagents--make-agent
                :id "t" :kind 'bash :name "bash" :description "Dev server"
                :first (- now 600) :started (- now 600) :state 'working))
         (agent (agent-hq-subagents--make-agent
                 :id "a" :name "drafts" :first (- now 60) :started (- now 60) :last (- now 1)))
         (status 'ready)
         (spins (lambda ()
                  (mapcar (lambda (row) (and (get-text-property 0 'agent-hq-subagents-spin row) t))
                          (cdr (split-string (agent-hq-subagents--panel (list task agent) 100 now)
                                             "\n" t))))))
    (cl-letf (((symbol-function 'agent-hq--status) (lambda (_) status)))
      ;; Idle: the agent at work spins, the server running for hours doesn't.
      (should (equal (funcall spins) '(nil t)))
      (setq status 'busy)
      (should (equal (funcall spins) '(t t))))))

;;;; Transcripts

(ert-deftest agent-hq-subagents-test-render ()
  (with-temp-buffer
    (dolist (line (list (agent-hq-subagents-test--user "t" "Fix /w/src/a.el")
                        (agent-hq-subagents-test--assistant
                         "t" "tool_use"
                         [((type . "thinking") (thinking . "hmm"))
                          ((type . "text") (text . "Reading it."))
                          ((type . "tool_use") (name . "Read") (input . ((file_path . "/w/src/a.el"))))])
                        (agent-hq-subagents-test--user
                         "t" [((type . "tool_result") (tool_use_id . "x")
                               (content . "1\n2\n3\n4\n5"))])
                        (agent-hq-subagents-test--assistant
                         "t" "tool_use" [((type . "tool_use") (name . "Bash") (input . ((command . "make test"))))])
                        (agent-hq-subagents-test--user
                         "t" [((type . "tool_result") (tool_use_id . "y") (is_error . t)
                               (content . [((type . "text") (text . "boom"))]))])))
      (agent-hq-subagents--insert-entry (agent-hq-subagents-test--parse line) "/w/"))
    (should (equal (buffer-string)
                   (concat "❯ Fix /w/src/a.el\n\n"
                           "Reading it.\n\n"
                           "● Read(src/a.el)\n"
                           "  ⎿  1\n     2\n     3\n     … +2 lines\n\n"
                           "● Bash(make test)\n"
                           "  ⎿  boom\n\n")))))

(ert-deftest agent-hq-subagents-test-ring ()
  (agent-hq-subagents-test--with-session
    (agent-hq-subagents-test--agent subagents "a1" '((agentType . "One"))
                                     (agent-hq-subagents-test--user (agent-hq-subagents-test--stamp 20) "Go"))
    (agent-hq-subagents-test--agent subagents "a2" '((agentType . "Two"))
                                     (agent-hq-subagents-test--user (agent-hq-subagents-test--stamp 10) "Go"))
    (agent-hq-subagents-test--agent subagents "a3" '((agentType . "Old") (finished . t))
                                     (agent-hq-subagents-test--user (agent-hq-subagents-test--stamp 9000) "Go"))
    (set-file-times (expand-file-name "agent-a3.jsonl" subagents) (time-subtract nil 9000))
    (let ((shell (current-buffer)))
      (should (equal (agent-hq-subagents--ring shell nil) '(nil "a1" "a2")))
      ;; The agent shown stays in the ring after it leaves the list.
      (should (equal (agent-hq-subagents--ring shell "a3") '(nil "a3" "a1" "a2"))))))

;;;; Shown in a window

(define-derived-mode agent-hq-subagents-test-shell-mode fundamental-mode "Shell"
  "Stands in for `agent-shell-mode'.")
(put 'agent-hq-subagents-test-shell-mode 'derived-mode-parent 'agent-shell-mode)

(ert-deftest agent-hq-subagents-test-shown-h ()
  "Only a window now showing a shell or transcript refreshes a shell.
Since Emacs 31 a buffer's local `window-buffer-change-functions' also
runs once it leaves a window, which by then shows some other buffer."
  (let ((shell (generate-new-buffer " shell"))
        (view (generate-new-buffer " transcript"))
        (other (generate-new-buffer " other"))
        (window (selected-window))
        scheduled)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-hq-subagents--schedule)
                   (lambda (buffer) (push buffer scheduled))))
          (with-current-buffer shell (agent-hq-subagents-test-shell-mode))
          (with-current-buffer view
            (agent-hq-subagents-view-mode)
            (setq agent-hq-subagents--shell shell))
          ;; The shell left WINDOW for a buffer that is neither.
          (set-window-buffer window other)
          (agent-hq-subagents--shown-h window)
          (should-not scheduled)
          (set-window-buffer window view)
          (agent-hq-subagents--shown-h window)
          (set-window-buffer window shell)
          (agent-hq-subagents--shown-h window)
          (should (equal scheduled (list shell shell))))
      (mapc #'kill-buffer (list shell view other)))))

;;;; Stepping onto the list with TAB

(defun agent-hq-subagents-test--selected-row ()
  "Return the id of the row this shell's list shows selected, or nil.
Checks the cursor is drawn on it, and that it is highlighted."
  (let* ((string (overlay-get agent-hq-subagents--overlay 'before-string))
         (cursor (text-property-not-all 0 (length string) 'cursor nil string))
         (id (and cursor (get-text-property cursor 'agent-hq-subagents-id string))))
    (when cursor
      (should (memq 'agent-hq-subagents-selected
                    (ensure-list (get-text-property cursor 'face string))))
      (should (= (seq-count (lambda (position) (get-text-property position 'cursor string))
                            (number-sequence 0 (1- (length string))))
                 1)))
    id))

(ert-deftest agent-hq-subagents-test-tab-steps-onto-the-list ()
  "TAB and S-TAB stop on each row between the last item and the prompt.
RET shows the agent of the row; any other command leaves the list."
  (let* ((now (float-time))
         (agents (seq-map-indexed
                  (lambda (id age)
                    (agent-hq-subagents--make-agent
                     :id id :name id :finished t :first (- now 90 (- age)) :last (- now 30)))
                  '("a" "b" "c")))
         (agent-hq-subagents-max-rows 2)
         (agent-hq-subagents--unselect nil)
         (overriding-terminal-local-map nil)
         (pre-command-hook nil)
         (items nil)
         ;; agent-shell's own, stepping through ITEMS: output items and the
         ;; prompt's input.
         (next (lambda () (when-let* ((p (seq-find (lambda (p) (> p (point))) items)))
                            (goto-char p))))
         (previous (lambda () (when-let* ((p (seq-find (lambda (p) (< p (point))) (reverse items))))
                                (goto-char p))))
         shown)
    (cl-letf (((symbol-function 'agent-shell--live-input-prompt-p)
               (lambda (prompt) (and prompt (= (cdr prompt) (point-max)))))
              ((symbol-function 'agent-hq-subagents--show)
               (lambda (shell id) (push (cons shell id) shown)))
              ((symbol-function 'agent-hq-subagents-view)
               (lambda () (interactive) (push 'picked shown))))
      (with-temp-buffer
        (agent-hq-subagents-test-shell-mode)
        (insert "one\ntwo\n\n")
        (let ((foot (point-marker))
              (shell (current-buffer)))
          (insert "Claude> ")
          (setq-local comint-last-prompt (cons foot (point-marker)))
          (setq items (list 1 5 (point)))
          (agent-hq-subagents--draw (agent-hq-subagents--panel agents 80 now))
          (cl-flet ((tab () (agent-hq-subagents--step-item next t nil))
                    (backtab () (agent-hq-subagents--step-item previous nil nil)))
            (unwind-protect
                (progn
                  ;; From the last item above the list, down its rows: not
                  ;; main, the shell itself.
                  (goto-char 5)
                  (tab)
                  (should (equal (agent-hq-subagents-test--selected-row) "a"))
                  (should (= (point) foot))
                  (should agent-hq-subagents--unselect)
                  (should (eq (cadr overriding-terminal-local-map) agent-hq-subagents-selected-map))
                  (tab)
                  (should (equal (agent-hq-subagents-test--selected-row) "b"))
                  (tab)
                  (should (eq (agent-hq-subagents-test--selected-row) 'more))
                  ;; Past the last row, to the prompt, and the list's keys end.
                  (tab)
                  (should (= (point) (car (last items))))
                  (should-not (agent-hq-subagents-test--selected-row))
                  (should-not agent-hq-subagents--selected)
                  (should-not agent-hq-subagents--unselect)
                  (should-not (memq agent-hq-subagents-selected-map overriding-terminal-local-map))
                  ;; Up from the prompt: the rows, then the items above.
                  (backtab)
                  (should (eq (agent-hq-subagents-test--selected-row) 'more))
                  (backtab)
                  (backtab)
                  (should (equal (agent-hq-subagents-test--selected-row) "a"))
                  (backtab)
                  (should (= (point) 5))
                  (should-not agent-hq-subagents--selected)
                  (backtab)
                  (should (= (point) 1))
                  (tab)
                  (should (= (point) 5))
                  ;; Typing TAB at the prompt types it.
                  (goto-char (point-max))
                  (agent-hq-subagents--step-item (lambda () (insert "\t")) nil nil)
                  (should-not agent-hq-subagents--selected)
                  (delete-char -1)
                  ;; The list's keys last while it is stepped through.
                  (goto-char 5)
                  (tab)
                  (let ((this-command 'agent-shell-next-item))
                    (should (agent-hq-subagents--keep-selected-p shell)))
                  (let ((this-command 'agent-hq-subagents-open-selected))
                    (should (agent-hq-subagents--keep-selected-p shell)))
                  (let ((this-command 'evil-next-line))
                    (should-not (agent-hq-subagents--keep-selected-p shell)))
                  ;; RET shows the row's agent in this shell's window.
                  (agent-hq-subagents-open-selected)
                  (should (equal shown (list (cons shell "a"))))
                  (should-not agent-hq-subagents--selected)
                  (should-not agent-hq-subagents--unselect)
                  ;; And on the count of the others, picks from them all.
                  (goto-char (point-max))
                  (backtab)
                  (agent-hq-subagents-open-selected)
                  (should (eq (car shown) 'picked))
                  ;; Another command ends the list's keys: the row is let go.
                  (backtab)
                  (funcall agent-hq-subagents--unselect)
                  (should-not agent-hq-subagents--selected)
                  (should-not (agent-hq-subagents-test--selected-row))
                  ;; No list: TAB is agent-shell's own.
                  (agent-hq-subagents--draw nil)
                  (goto-char 5)
                  (tab)
                  (should (= (point) (car (last items))))
                  (backtab)
                  (should (= (point) 5)))
              (when agent-hq-subagents--unselect
                (funcall agent-hq-subagents--unselect)))))))))

;;; agent-hq-subagents-test.el ends here
