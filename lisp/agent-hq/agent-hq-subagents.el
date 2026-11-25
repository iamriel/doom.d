;;; agent-hq-subagents.el --- Subagents of Claude shells, listed and opened as in Claude Code -*- lexical-binding: t; -*-

;;; Commentary:

;; Claude Code's terminal UI lists, under its prompt, the subagents and
;; agent-team teammates a session has started, and opens any one's
;; transcript.  This does the same for agent-shell's Claude shells.  Just
;; above the prompt:
;;
;;   ● main
;;   ○ room29-drafts  Draft rich Room 29 top floors     22m 53s · ↓ 581.8k tokens
;;
;; Time counts from the agent's latest task (its prompt, or the latest
;; message sent to it).  Tokens are its context after its latest reply, the
;; figure Claude Code reports when an agent finishes.  Working agents stay
;; listed; finished and idle ones for `agent-hq-subagents-linger' seconds.
;;
;; Commands:
;;   `agent-hq-subagents-view'      pick any agent of the session, or main,
;;                                   and show it in this window; a transcript
;;                                   follows its agent while it works
;;   `agent-hq-subagents-next', `agent-hq-subagents-previous'
;;                                   step through main and the listed agents
;;   `agent-hq-subagents-main'      back to the shell
;;
;; Claude Code writes each subagent's transcript beside its session's:
;; <config>/projects/<project>/<session>/subagents/agent-<id>.jsonl, and
;; agent-<id>.meta.json with its name, description and, once it's done,
;; "finished".  Those files are the source here, so resumed sessions and
;; teammates show too.  Each read picks up only what was appended since the
;; last.  Tool calls in the shell trigger a read; while an agent works, a
;; one-second clock keeps the visible lists and transcripts current.
;;
;; Turn it on with `agent-hq-subagents-setup'.

;;; Code:

(require 'agent-hq)
(require 'iso8601)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell-session-id "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell--live-input-prompt-p "agent-shell-prompt")
(defvar agent-shell-display-action)
(defvar comint-last-prompt)
(defvar shell-maker-finish-output-hook)

(defgroup agent-hq-subagents nil
  "Subagents of Claude shells, as Claude Code's terminal UI lists them."
  :group 'agent-hq
  :prefix "agent-hq-subagents-")

(defcustom agent-hq-subagents-linger 300
  "Seconds a finished or idle agent stays listed after its last step."
  :type 'integer)

(defcustom agent-hq-subagents-max-rows 6
  "Most agents listed above a shell's prompt; the rest are counted."
  :type 'integer)

(defcustom agent-hq-subagents-stalled-after 1800
  "Seconds without a step after which an unfinished agent counts as idle.
Its session may have ended without Claude Code marking it finished."
  :type 'integer)

(defcustom agent-hq-subagents-result-lines 3
  "Lines of each tool result a transcript shows."
  :type 'integer)

(defcustom agent-hq-subagents-key-hints "q main  n/p next/previous  a pick"
  "Keys a transcript's header line names; set them to match your bindings."
  :type 'string)

(defcustom agent-hq-subagents-tail-bytes (* 256 1024)
  "How much of a quiet agent's transcript is read for its latest state.
Transcripts that changed in the last `agent-hq-subagents-linger'
seconds are read whole, for when the agent's task began."
  :type 'integer)

(defface agent-hq-subagents-current '((t :inherit bold))
  "The agent shown: main in the shell's list, the agent in a transcript.")

(defface agent-hq-subagents-quiet '((t :inherit shadow))
  "Agents that are done or idle, and tool results.")

(defface agent-hq-subagents-tool '((t :inherit font-lock-function-name-face))
  "Tool names in transcripts.")

;;;; Agents, from their files

(cl-defstruct (agent-hq-subagents-agent
               (:constructor agent-hq-subagents--make-agent)
               (:copier nil))
  "A subagent or teammate, as its files under the session's subagents/ say."
  id file name named description finished meta-mtime
  (offset 0) (partial "")              ; transcript bytes read so far
  first started last tokens            ; float times; context tokens
  (state 'working))                    ; from its latest line: working or idle

(defun agent-hq-subagents--time (timestamp)
  "Return ISO 8601 TIMESTAMP as a float time, or nil."
  (when (stringp timestamp)
    (ignore-errors (float-time (encode-time (iso8601-parse timestamp))))))

(defun agent-hq-subagents--strip-tag (text tag)
  "Return TEXT without its <TAG>…</TAG> spans."
  (let ((open (concat "<" tag ">"))
        (close (concat "</" tag ">"))
        (start 0)
        parts)
    (while-let ((from (string-search open text start)))
      (push (substring text start from) parts)
      (setq start (if-let* ((to (string-search close text from)))
                      (+ to (length close))
                    (length text))))
    (push (substring text start) parts)
    (apply #'concat (nreverse parts))))

(defun agent-hq-subagents--prompt-text (entry)
  "Return what ENTRY, a user line of a transcript, asks of the agent, or nil.
Nil for tool results and for the reminders Claude Code adds on its own.
A teammate's message keeps its sender: \"[team-lead] …\"."
  (let ((content (map-nested-elt entry '(message content))))
    (unless (or (eq (alist-get 'isMeta entry) t)
                (and (vectorp content)
                     (seq-some (lambda (block) (equal (alist-get 'type block) "tool_result"))
                               content)))
      (let* ((text (if (stringp content)
                       content
                     (mapconcat (lambda (block)
                                  (or (and (equal (alist-get 'type block) "text")
                                           (alist-get 'text block))
                                      ""))
                                content "\n")))
             (text (agent-hq-subagents--strip-tag text "system-reminder"))
             (text (replace-regexp-in-string
                    "<teammate-message[^>]*?teammate_id=\"\\([^\"]*\\)\"[^>]*>\n?" "[\\1] "
                    text t))
             (text (string-trim (replace-regexp-in-string "\n?</teammate-message>" "" text t t))))
        (unless (string-empty-p text)
          text)))))

(defun agent-hq-subagents--entry-state (entry)
  "Return `idle' if ENTRY, a transcript line, ends the agent's turn.
Else `working'."
  (let* ((message (alist-get 'message entry))
         (content (alist-get 'content message)))
    (if (and (equal (alist-get 'type entry) "assistant")
             (member (alist-get 'stop_reason message)
                     '("end_turn" "stop_sequence" "max_tokens" "refusal"))
             (not (and (vectorp content)
                       (seq-some (lambda (block) (equal (alist-get 'type block) "tool_use"))
                                 content))))
        'idle
      'working)))

(defun agent-hq-subagents--context-tokens (entry)
  "Return the context after ENTRY, an assistant line: its input plus output tokens."
  (when-let* ((usage (map-nested-elt entry '(message usage))))
    (seq-reduce (lambda (sum key)
                  (let ((n (alist-get key usage)))
                    (if (numberp n) (+ sum n) sum)))
                '(input_tokens cache_creation_input_tokens cache_read_input_tokens
                  output_tokens)
                0)))

(defun agent-hq-subagents--read-appended (file offset partial tail fn)
  "Call FN in a buffer holding the lines appended to FILE since byte OFFSET.
PARTIAL is the unfinished last line of the previous read, as bytes.  With
TAIL, a first read (OFFSET 0) of a larger file starts at the first line
within TAIL bytes of its end.  The buffer holds complete lines, decoded,
with point at its start.  Returns (OFFSET . PARTIAL) for the next read,
or nil when nothing was appended."
  (let ((size (file-attribute-size (file-attributes file))))
    (when (and size (> size offset))
      (let ((start (if (and tail (zerop offset) (> size tail)) (- size tail) offset)))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (if (> start offset)
              (progn (insert-file-contents-literally file nil start size)
                     ;; Started mid-line: drop that line.
                     (delete-region (point-min) (progn (forward-line 1) (point))))
            (insert partial)
            (insert-file-contents-literally file nil start size))
          (goto-char (point-max))
          (let* ((cut (if (search-backward "\n" nil t) (1+ (point)) (point-min)))
                 (rest (buffer-substring-no-properties cut (point-max))))
            (delete-region cut (point-max))
            (set-buffer-multibyte t)
            (decode-coding-region (point-min) (point-max) 'utf-8)
            (goto-char (point-min))
            (funcall fn)
            (cons size rest)))))))

(defun agent-hq-subagents--line-entry ()
  "Return the transcript line at point, parsed, or nil."
  (save-excursion
    (goto-char (line-beginning-position))
    (ignore-errors (json-parse-buffer :object-type 'alist :null-object nil :false-object nil))))

(defun agent-hq-subagents--find-last (regexp predicate)
  "Return the last line matching REGEXP whose parse PREDICATE accepts, parsed.
Searches back from the end of the buffer; matching lines are the only
ones parsed."
  (goto-char (point-max))
  (let (found)
    (while (and (not found) (re-search-backward regexp nil t))
      (let ((entry (agent-hq-subagents--line-entry)))
        (if (and entry (funcall predicate entry))
            (setq found entry)
          (goto-char (line-beginning-position)))))
    found))

(defun agent-hq-subagents--note-lines (agent)
  "Update AGENT from this buffer's transcript lines, its latest appended."
  (let ((stamp "\"timestamp\":\"\\([^\"]+\\)\""))
    (when (and (null (agent-hq-subagents-agent-first agent))
               (re-search-forward stamp nil t))
      (setf (agent-hq-subagents-agent-first agent)
            (agent-hq-subagents--time (match-string 1))))
    ;; A line's own timestamp follows its message, so it's the line's last.
    (goto-char (point-max))
    (when (re-search-backward stamp nil t)
      (setf (agent-hq-subagents-agent-last agent)
            (agent-hq-subagents--time (match-string 1)))))
  (when-let* ((entry (agent-hq-subagents--find-last
                      "\"type\":\"\\(?:user\\|assistant\\)\""
                      (lambda (entry) (member (alist-get 'type entry) '("user" "assistant"))))))
    (setf (agent-hq-subagents-agent-state agent) (agent-hq-subagents--entry-state entry)))
  (when-let* ((entry (agent-hq-subagents--find-last
                      "\"type\":\"assistant\""
                      (lambda (entry)
                        (and (equal (alist-get 'type entry) "assistant")
                             (not (equal (map-nested-elt entry '(message model)) "<synthetic>"))
                             (map-nested-elt entry '(message usage)))))))
    (setf (agent-hq-subagents-agent-tokens agent) (agent-hq-subagents--context-tokens entry)))
  (when-let* ((entry (agent-hq-subagents--find-last
                      "\"role\":\"user\",\"content\":\\(?:\"\\|\\[{\"type\":\"text\"\\)"
                      (lambda (entry)
                        (and (equal (alist-get 'type entry) "user")
                             (agent-hq-subagents--prompt-text entry))))))
    (setf (agent-hq-subagents-agent-started agent)
          (agent-hq-subagents--time (alist-get 'timestamp entry)))))

(defun agent-hq-subagents--read-meta (agent file)
  "Set AGENT's name, description and whether it finished from FILE, its meta.json."
  (when-let* ((meta (ignore-errors (agent-hq--read-json file))))
    (let ((text (lambda (key)
                  (let ((value (alist-get key meta)))
                    (and (stringp value) (not (string-empty-p value)) value)))))
      (setf (agent-hq-subagents-agent-name agent)
            (or (funcall text 'name) (funcall text 'agentType) "agent")
            (agent-hq-subagents-agent-named agent) (and (funcall text 'name) t)
            (agent-hq-subagents-agent-description agent) (funcall text 'description)
            (agent-hq-subagents-agent-finished agent) (eq (alist-get 'finished meta) t)))))

(defun agent-hq-subagents--update (agent all)
  "Read what AGENT's transcript gained since it was last read.
A transcript not read yet is read when it may be listed, or with ALL: its
agent isn't marked finished or changed it lately.  One unchanged for
`agent-hq-subagents-linger' seconds is read from its tail."
  (when-let* ((file (agent-hq-subagents-agent-file agent))
              (attributes (file-attributes file)))
    (let* ((mtime (float-time (file-attribute-modification-time attributes)))
           (quiet (> (- (float-time) mtime) agent-hq-subagents-linger))
           (offset (agent-hq-subagents-agent-offset agent)))
      (when (or all (> offset 0) (not quiet)
                (not (agent-hq-subagents-agent-finished agent)))
        (when-let* ((next (agent-hq-subagents--read-appended
                           file offset (agent-hq-subagents-agent-partial agent)
                           (and quiet agent-hq-subagents-tail-bytes)
                           (lambda () (agent-hq-subagents--note-lines agent)))))
          (setf (agent-hq-subagents-agent-offset agent) (car next)
                (agent-hq-subagents-agent-partial agent) (cdr next))))
      (unless (agent-hq-subagents-agent-last agent)
        (setf (agent-hq-subagents-agent-last agent) mtime)))))

(defun agent-hq-subagents--status (agent now)
  "Return AGENT's status at float time NOW: `working', `idle' or `done'."
  (cond ((agent-hq-subagents-agent-finished agent) 'done)
        ((and (eq (agent-hq-subagents-agent-state agent) 'working)
              (< (- now (or (agent-hq-subagents-agent-last agent) 0))
                 agent-hq-subagents-stalled-after))
         'working)
        (t 'idle)))

(defun agent-hq-subagents--listed-p (agent now)
  "Return non-nil if AGENT belongs in its shell's list at float time NOW."
  (or (eq (agent-hq-subagents--status agent now) 'working)
      (< (- now (or (agent-hq-subagents-agent-last agent) 0)) agent-hq-subagents-linger)))

(defun agent-hq-subagents--older-p (a b)
  "Return non-nil if agent A started before agent B."
  (< (or (agent-hq-subagents-agent-first a) (agent-hq-subagents-agent-last a) 0)
     (or (agent-hq-subagents-agent-first b) (agent-hq-subagents-agent-last b) 0)))

;;;; Each shell's session

(defvar-local agent-hq-subagents--agents nil
  "This shell's agents by id, a hash table.")

(defvar-local agent-hq-subagents--where nil
  "(SESSION . DIR): this shell's session and its subagents/ folder.
DIR is nil until the session's files are found.")

(defvar-local agent-hq-subagents--looked nil
  "When the session's files were last looked for, as a float time.")

(defun agent-hq-subagents--config-dir ()
  "Return the config dir this Claude shell runs on, or nil if it isn't one."
  (pcase agent-hq--claude-started-with
    ('nil nil)
    ('default (agent-hq--claude-default-config))
    (dir (file-name-as-directory (expand-file-name dir)))))

(defun agent-hq-subagents--session-dir (config session dir)
  "Return the folder CONFIG keeps SESSION's files in, or nil while it has none.
DIR is the session's working directory.  Claude Code names the folder's
parent after DIR, except for long paths; those are found by SESSION."
  (let* ((projects (expand-file-name "projects" config))
         (key (agent-hq--claude-project-key dir))
         (guess (and key (expand-file-name key projects))))
    (if (and guess (file-exists-p (expand-file-name (concat session ".jsonl") guess)))
        (expand-file-name session guess)
      (when-let* ((found (car (file-expand-wildcards
                               (expand-file-name (concat "*/" session ".jsonl") projects)))))
        (expand-file-name session (file-name-directory found))))))

(defun agent-hq-subagents--dir ()
  "Return this shell's subagents/ folder, or nil while it can't be told.
A new session (/resume, fork) starts over with no agents."
  (when-let* ((config (agent-hq-subagents--config-dir))
              (session (ignore-errors (agent-shell-session-id))))
    (unless (equal (car agent-hq-subagents--where) session)
      (setq agent-hq-subagents--where (list session)
            agent-hq-subagents--looked nil
            agent-hq-subagents--agents (make-hash-table :test #'equal)))
    (or (cdr agent-hq-subagents--where)
        (when (or (null agent-hq-subagents--looked)
                  (> (- (float-time) agent-hq-subagents--looked) 10))
          (setq agent-hq-subagents--looked (float-time))
          (when-let* ((dir (agent-hq-subagents--session-dir config session default-directory)))
            (setcdr agent-hq-subagents--where
                    (file-name-as-directory (expand-file-name "subagents" dir))))))))

(defun agent-hq-subagents--scan (&optional all)
  "Re-read this shell's agents and return them, oldest first.
With ALL, read every transcript, not just those that may be listed."
  (unless agent-hq-subagents--agents
    (setq agent-hq-subagents--agents (make-hash-table :test #'equal)))
  (let ((dir (agent-hq-subagents--dir)))
    (when (and dir (file-directory-p dir))
      (dolist (meta (directory-files dir t "\\`agent-.+\\.meta\\.json\\'" t))
        (let* ((id (string-remove-suffix
                    ".meta.json" (string-remove-prefix "agent-" (file-name-nondirectory meta))))
               (agent (or (gethash id agent-hq-subagents--agents)
                          (puthash id (agent-hq-subagents--make-agent
                                       :id id
                                       :file (expand-file-name (concat "agent-" id ".jsonl") dir))
                                   agent-hq-subagents--agents)))
               (mtime (file-attribute-modification-time (file-attributes meta))))
          (unless (equal mtime (agent-hq-subagents-agent-meta-mtime agent))
            (setf (agent-hq-subagents-agent-meta-mtime agent) mtime)
            (agent-hq-subagents--read-meta agent meta))
          (agent-hq-subagents--update agent all)))))
  (sort (hash-table-values agent-hq-subagents--agents) #'agent-hq-subagents--older-p))

;;;; The list above the prompt

(defun agent-hq-subagents-format-tokens (tokens)
  "Return TOKENS as Claude Code shows them: 950, 12.3k, 1.2m."
  (cond ((< tokens 1000) (number-to-string tokens))
        ((< tokens 1000000) (format "%.1fk" (/ tokens 1000.0)))
        (t (format "%.1fm" (/ tokens 1000000.0)))))

(defun agent-hq-subagents-format-elapsed (seconds)
  "Return SECONDS as \"45s\", \"22m 53s\" or \"1h 05m\"."
  (let ((seconds (max 0 (floor seconds))))
    (cond ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm %02ds" (/ seconds 60) (% seconds 60)))
          (t (format "%dh %02dm" (/ seconds 3600) (/ (% seconds 3600) 60))))))

(defun agent-hq-subagents--stats (agent now)
  "Return \"22m 53s · ↓ 581.8k tokens\" for AGENT at float time NOW.
Time is that of its latest task, so far while it works; an idle agent
says \"idle\" instead."
  (let* ((status (agent-hq-subagents--status agent now))
         (since (or (agent-hq-subagents-agent-started agent)
                    (agent-hq-subagents-agent-first agent)))
         (elapsed (and since (agent-hq-subagents-format-elapsed
                              (- (if (eq status 'working)
                                     now
                                   (or (agent-hq-subagents-agent-last agent) now))
                                 since))))
         (tokens (agent-hq-subagents-agent-tokens agent)))
    (string-join (delq nil (list (pcase status
                                   ('working elapsed)
                                   ('idle "idle")
                                   ('done (if elapsed (concat "done in " elapsed) "done")))
                                 (and tokens (concat "↓ " (agent-hq-subagents-format-tokens tokens)
                                                     " tokens"))))
                 " · ")))

(defun agent-hq-subagents--row (agent name-width width now)
  "Return AGENT's line in a list WIDTH columns wide, at float time NOW.
Names are padded to NAME-WIDTH; the stats end at the window's right edge."
  (let* ((stats (agent-hq-subagents--stats agent now))
         (head (concat "○ " (string-pad (agent-hq-subagents-agent-name agent) name-width) "  "))
         (room (- width (string-width head) (string-width stats) 3))
         (description (if (> room 8)
                          (agent-hq--one-line (agent-hq-subagents-agent-description agent) room)
                        "")))
    (propertize (concat head description
                        (propertize " " 'display `(space :align-to (- right ,(1+ (string-width stats)))))
                        stats)
                'face (unless (eq (agent-hq-subagents--status agent now) 'working)
                        'agent-hq-subagents-quiet)
                'agent-hq-subagents-id (agent-hq-subagents-agent-id agent))))

(defun agent-hq-subagents--listed (agents now)
  "Return the AGENTS a shell lists at float time NOW, oldest first.
At most `agent-hq-subagents-max-rows' of them, working ones before others."
  (let ((listed (seq-filter (lambda (agent) (agent-hq-subagents--listed-p agent now)) agents)))
    (sort (seq-take (sort (copy-sequence listed)
                          (lambda (a b)
                            (and (eq (agent-hq-subagents--status a now) 'working)
                                 (not (eq (agent-hq-subagents--status b now) 'working)))))
                    agent-hq-subagents-max-rows)
          #'agent-hq-subagents--older-p)))

(defun agent-hq-subagents--panel (agents width now)
  "Return the list shown above a prompt for AGENTS, or nil if it lists none.
WIDTH is the window's width in columns; NOW a float time."
  (when-let* ((shown (agent-hq-subagents--listed agents now)))
    (let ((more (- (seq-count (lambda (agent) (agent-hq-subagents--listed-p agent now)) agents)
                   (length shown)))
          (name-width (apply #'max 4 (mapcar (lambda (agent)
                                               (string-width (agent-hq-subagents-agent-name agent)))
                                             shown))))
      (concat (propertize "● main" 'face 'agent-hq-subagents-current) "\n"
              (mapconcat (lambda (agent)
                           (concat (agent-hq-subagents--row agent name-width width now) "\n"))
                         shown "")
              (when (> more 0)
                (propertize (format "  … %d more\n" more) 'face 'agent-hq-subagents-quiet))))))

(defvar-local agent-hq-subagents--overlay nil
  "Overlay drawing this shell's list just above its prompt.")

(defvar-local agent-hq-subagents--text nil
  "The list this shell's overlay draws, or nil when it lists none.")

(defun agent-hq-subagents--anchor ()
  "Return where this shell's list goes: its live prompt's start, else the end.
Mid-turn without a persistent prompt, output streams at the end and
pushes the list along."
  (or (and comint-last-prompt
           (ignore-errors (agent-shell--live-input-prompt-p comint-last-prompt))
           (marker-position (car comint-last-prompt)))
      (point-max)))

(defun agent-hq-subagents--draw (text)
  "Show TEXT just above this shell's prompt, or no list when TEXT is nil."
  (setq agent-hq-subagents--text text)
  (if (null text)
      (when agent-hq-subagents--overlay
        (delete-overlay agent-hq-subagents--overlay)
        (setq agent-hq-subagents--overlay nil))
    (let* ((anchor (agent-hq-subagents--anchor))
           ;; Advancing both ends keeps the overlay after whatever is
           ;; written where it sits: above the prompt, below the output.
           (overlay (or agent-hq-subagents--overlay
                        (setq agent-hq-subagents--overlay
                              (make-overlay anchor anchor nil t t))))
           (string (concat (unless (memq (char-before anchor) '(nil ?\n)) "\n")
                           text "\n")))
      (unless (eql (overlay-start overlay) anchor)
        (move-overlay overlay anchor anchor))
      (unless (equal-including-properties string (overlay-get overlay 'before-string))
        (overlay-put overlay 'before-string string)))))

(defun agent-hq-subagents--follow-prompt ()
  "Move this shell's list above its newest prompt."
  (when (and agent-hq-subagents--overlay
             (not (eql (overlay-start agent-hq-subagents--overlay)
                       (agent-hq-subagents--anchor))))
    (agent-hq-subagents--draw agent-hq-subagents--text)))

(defun agent-hq-subagents--follow-prompt-a (&rest _)
  "After advice for `agent-shell--print-prompt': keep the list above the prompt.
Submitting leaves the old prompt in the transcript and prints a new one."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-hq-subagents--follow-prompt)))

;;;; Transcripts

(defvar-local agent-hq-subagents--shell nil
  "In a transcript, the shell whose agent it shows.")

(defvar-local agent-hq-subagents--id nil
  "In a transcript, the id of the agent it shows.")

(defvar-local agent-hq-subagents--read '(0 . "")
  "In a transcript, (OFFSET . PARTIAL): how far its file has been read.")


(defun agent-hq-subagents--result-text (content)
  "Return the text of CONTENT, a tool result's content."
  (cond ((stringp content) content)
        ((vectorp content)
         (mapconcat (lambda (block)
                      (pcase (alist-get 'type block)
                        ("text" (or (alist-get 'text block) ""))
                        ("image" "[image]")
                        (_ "")))
                    content "\n"))
        (t "")))

(defun agent-hq-subagents--tool-summary (input cwd)
  "Return the input that says what a tool call with INPUT does, on one line.
Paths under CWD are shown relative to it."
  (when-let* (((consp input))
              (value (seq-some (lambda (key)
                                 (let ((value (alist-get key input)))
                                   (and (stringp value) (not (string-blank-p value)) value)))
                               '(command file_path notebook_path pattern path url query
                                 description skill prompt))))
    (when (and cwd (file-name-absolute-p value)
               (string-prefix-p (file-name-as-directory (expand-file-name cwd)) value))
      (setq value (file-relative-name value cwd)))
    (agent-hq--one-line value 120)))

(defun agent-hq-subagents--insert-result (block)
  "Insert the first lines of BLOCK, a tool result, as Claude Code shows them."
  (let* ((text (string-trim-right
                (agent-hq-subagents--result-text (alist-get 'content block))))
         (lines (if (string-empty-p text) (list "(no output)") (split-string text "\n")))
         (shown (seq-take lines agent-hq-subagents-result-lines))
         (more (- (length lines) (length shown))))
    (insert (propertize (concat "  ⎿  "
                                (mapconcat (lambda (line)
                                             (truncate-string-to-width line 200 nil nil "…"))
                                           shown "\n     ")
                                (when (> more 0) (format "\n     … +%d lines" more))
                                "\n\n")
                        'face (if (eq (alist-get 'is_error block) t)
                                  'error
                                'agent-hq-subagents-quiet)))))

(defun agent-hq-subagents--insert-entry (entry cwd)
  "Insert ENTRY, a parsed transcript line, at point.
Prompts, replies, tool calls and the start of their results show;
thinking and Claude Code's own bookkeeping don't.  Paths under CWD
are shown relative to it."
  (let ((content (map-nested-elt entry '(message content))))
    (pcase (alist-get 'type entry)
      ("user"
       (if-let* ((prompt (agent-hq-subagents--prompt-text entry)))
           (insert (propertize "❯ " 'face 'agent-hq-subagents-current) prompt "\n\n")
         (when (vectorp content)
           (seq-doseq (block content)
             (when (equal (alist-get 'type block) "tool_result")
               (agent-hq-subagents--insert-result block))))))
      ("assistant"
       (when (vectorp content)
         (seq-doseq (block content)
           (pcase (alist-get 'type block)
             ("text"
              (when-let* ((text (alist-get 'text block))
                          ((not (string-blank-p text))))
                (insert (string-trim text) "\n\n")))
             ("tool_use"
              (let ((summary (agent-hq-subagents--tool-summary (alist-get 'input block) cwd)))
                (insert (propertize "● " 'face 'success)
                        (propertize (or (alist-get 'name block) "Tool")
                                    'face 'agent-hq-subagents-tool)
                        (if summary (concat "(" summary ")") "")
                        "\n"))))))))))

(defun agent-hq-subagents--render-lines (view cwd)
  "Insert into VIEW each transcript line from point to the end of this buffer."
  (while (not (eobp))
    (when-let* ((entry (ignore-errors
                         (json-parse-buffer :object-type 'alist :null-object nil
                                            :false-object nil))))
      (with-current-buffer view
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (agent-hq-subagents--insert-entry entry cwd)))))
    (forward-line 1)))

(defun agent-hq-subagents--views (shell)
  "Return the live transcripts of SHELL's agents."
  (seq-filter (lambda (buffer) (eq (buffer-local-value 'agent-hq-subagents--shell buffer) shell))
              (buffer-list)))

(defun agent-hq-subagents--view-agent ()
  "Return the agent this transcript shows, or nil once its shell is gone."
  (when-let* ((shell agent-hq-subagents--shell)
              ((buffer-live-p shell))
              (agents (buffer-local-value 'agent-hq-subagents--agents shell)))
    (gethash agent-hq-subagents--id agents)))

(defun agent-hq-subagents--view-update (view)
  "Append to VIEW what its agent did since VIEW was last updated.
Windows at the end of VIEW stay there, following the agent."
  (with-current-buffer view
    (when-let* ((agent (agent-hq-subagents--view-agent)))
      (let ((followers (seq-filter (lambda (window) (>= (window-point window) (point-max)))
                                   (get-buffer-window-list view nil t)))
            (at-end (eobp))
            (cwd default-directory))
        (when-let* ((next (agent-hq-subagents--read-appended
                           (agent-hq-subagents-agent-file agent)
                           (car agent-hq-subagents--read) (cdr agent-hq-subagents--read) nil
                           (lambda () (agent-hq-subagents--render-lines view cwd)))))
          (setq agent-hq-subagents--read next)
          (dolist (window followers)
            (set-window-point window (point-max)))
          (when at-end
            (goto-char (point-max))))))
    (force-mode-line-update)))

(defun agent-hq-subagents--view-header ()
  "Return a transcript's header line: its agent, what it's doing, and keys."
  (string-replace
   "%" "%%"
   (if-let* ((agent (agent-hq-subagents--view-agent)))
       (concat " "
               (propertize (concat "● " (agent-hq-subagents-agent-name agent))
                           'face 'agent-hq-subagents-current)
               (when-let* ((description (agent-hq-subagents-agent-description agent)))
                 (concat "  " description))
               "  " (propertize (agent-hq-subagents--stats agent (float-time))
                                'face 'agent-hq-subagents-quiet)
               "    " (propertize agent-hq-subagents-key-hints
                                  'face 'agent-hq-subagents-quiet))
     " This agent's shell is gone")))

(defvar-keymap agent-hq-subagents-view-mode-map
  :doc "Keys of a subagent's transcript."
  :parent special-mode-map
  "q" #'agent-hq-subagents-main
  "n" #'agent-hq-subagents-next
  "p" #'agent-hq-subagents-previous
  "a" #'agent-hq-subagents-view
  "g" #'agent-hq-subagents-refresh)

(define-derived-mode agent-hq-subagents-view-mode special-mode "Subagent"
  "Transcript of an agent a Claude shell's session started.
Follows the agent while it works.

\\{agent-hq-subagents-view-mode-map}"
  (setq-local header-line-format '(:eval (agent-hq-subagents--view-header)))
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (add-hook 'window-buffer-change-functions #'agent-hq-subagents--shown-h nil t))

(defun agent-hq-subagents--view-buffer (shell agent)
  "Return the transcript of SHELL's AGENT, creating it if needed."
  (or (seq-find (lambda (view)
                  (equal (buffer-local-value 'agent-hq-subagents--id view)
                         (agent-hq-subagents-agent-id agent)))
                (agent-hq-subagents--views shell))
      (let ((view (generate-new-buffer
                   (format "%s ▸ %s" (buffer-name shell)
                           ;; Agents without a name of their own go by type,
                           ;; often the same one ("Explore"); their task tells
                           ;; them apart.
                           (if (agent-hq-subagents-agent-named agent)
                               (agent-hq-subagents-agent-name agent)
                             (agent-hq--one-line
                              (or (agent-hq-subagents-agent-description agent)
                                  (agent-hq-subagents-agent-name agent))
                              40)))))
            (dir (buffer-local-value 'default-directory shell)))
        (with-current-buffer view
          (agent-hq-subagents-view-mode)
          (setq default-directory dir
                agent-hq-subagents--shell shell
                agent-hq-subagents--id (agent-hq-subagents-agent-id agent)))
        view)))

;;;; Keeping lists and transcripts current

(defvar agent-hq-subagents--timer nil
  "Ticks every second while a shown list or transcript has an agent working.")

(defvar-local agent-hq-subagents--pending nil
  "Timer of this shell's refresh, when one is due.")

(defun agent-hq-subagents--watched-p (shell)
  "Return non-nil if SHELL, or a transcript of one of its agents, shows."
  (or (get-buffer-window shell 'visible)
      (seq-some (lambda (view) (get-buffer-window view 'visible))
                (agent-hq-subagents--views shell))))

;;;###autoload
(defun agent-hq-subagents-refresh (&optional shell all)
  "Re-read SHELL's agents, redraw its list and its shown transcripts.
SHELL defaults to this buffer's, a shell or a transcript.  With ALL,
read every agent's transcript.  Returns non-nil while an agent works."
  (interactive)
  (let ((shell (or shell (car (agent-hq-subagents--context)))))
    (when (buffer-live-p shell)
      (with-current-buffer shell
        (let* ((now (float-time))
               (agents (agent-hq-subagents--scan all))
               (window (get-buffer-window shell t)))
          (agent-hq-subagents--draw
           (agent-hq-subagents--panel agents (if window (window-max-chars-per-line window) 80)
                                       now))
          (dolist (view (agent-hq-subagents--views shell))
            (when (get-buffer-window view 'visible)
              (agent-hq-subagents--view-update view)))
          (seq-some (lambda (agent) (eq (agent-hq-subagents--status agent now) 'working))
                    agents))))))

(defun agent-hq-subagents--tick ()
  "Refresh every shown list and transcript; stop once no agent there works."
  (let (working)
    (dolist (shell (agent-hq--shells))
      (when (and (buffer-local-value 'agent-hq-subagents--agents shell)
                 (agent-hq-subagents--watched-p shell)
                 (with-demoted-errors "agent-hq-subagents: %S"
                   (agent-hq-subagents-refresh shell)))
        (setq working t)))
    (unless working
      (cancel-timer agent-hq-subagents--timer)
      (setq agent-hq-subagents--timer nil))))

(defun agent-hq-subagents--keep-time (shell working)
  "Start the clock if WORKING, SHELL's agents being at work, shows."
  (when (and working
             (not (timerp agent-hq-subagents--timer))
             (agent-hq-subagents--watched-p shell))
    (setq agent-hq-subagents--timer (run-at-time 1 1 #'agent-hq-subagents--tick))))

(defun agent-hq-subagents--run-pending (shell)
  "Refresh SHELL now that its pending refresh is due."
  (when (buffer-live-p shell)
    (with-current-buffer shell
      (setq agent-hq-subagents--pending nil))
    (with-demoted-errors "agent-hq-subagents: %S"
      (agent-hq-subagents--keep-time shell (agent-hq-subagents-refresh shell)))))

(defun agent-hq-subagents--schedule (shell)
  "Refresh SHELL shortly: once, however many updates come meanwhile."
  (when (buffer-live-p shell)
    (with-current-buffer shell
      (unless (timerp agent-hq-subagents--pending)
        (setq agent-hq-subagents--pending
              (run-at-time 0.5 nil #'agent-hq-subagents--run-pending shell))))))

(defun agent-hq-subagents--shown-h (window)
  "Refresh the shell of WINDOW's buffer, a shell or transcript just shown.
For a buffer-local `window-buffer-change-functions'.  Since Emacs 31 the
hook also runs for the buffer leaving WINDOW, which by then shows another
buffer: that one is left alone unless it is a shell or transcript too."
  (when-let* ((context (agent-hq-subagents--context-of (window-buffer window))))
    (agent-hq-subagents--schedule (car context))))

(defun agent-hq-subagents--kill-views-h ()
  "Kill this shell's transcripts.  For a buffer-local `kill-buffer-hook'."
  (mapc #'kill-buffer (agent-hq-subagents--views (current-buffer))))

;;;###autoload
(defun agent-hq-subagents-setup-shell-h ()
  "Keep this shell's list of agents current.  For `agent-shell-mode-hook'.
Subagents' tool calls come through the shell too, so any tool call
triggers a read, as does the end of a turn."
  (let ((shell (current-buffer)))
    (dolist (event '(tool-call-update turn-complete))
      (agent-shell-subscribe-to
       :shell-buffer shell
       :event event
       :on-event (lambda (_event) (agent-hq-subagents--schedule shell)))))
  (add-hook 'window-buffer-change-functions #'agent-hq-subagents--shown-h nil t)
  (add-hook 'shell-maker-finish-output-hook #'agent-hq-subagents--follow-prompt nil t)
  (add-hook 'kill-buffer-hook #'agent-hq-subagents--kill-views-h nil t))

;;;###autoload
(defun agent-hq-subagents-setup ()
  "List each Claude shell's subagents above its prompt; see the Commentary."
  (add-hook 'agent-shell-mode-hook #'agent-hq-subagents-setup-shell-h)
  (advice-add 'agent-shell--print-prompt :after #'agent-hq-subagents--follow-prompt-a))

;;;; Moving between the shell and its agents

(defun agent-hq-subagents--context-of (buffer)
  "Return (SHELL . ID) if BUFFER is a shell or transcript, else nil.
ID is the agent BUFFER shows, nil in the shell itself."
  (with-current-buffer buffer
    (cond ((derived-mode-p 'agent-hq-subagents-view-mode)
           (cons agent-hq-subagents--shell agent-hq-subagents--id))
          ((derived-mode-p 'agent-shell-mode)
           (cons buffer nil)))))

(defun agent-hq-subagents--context ()
  "Return (SHELL . ID): the shell commands act on, and the agent shown there.
ID is nil in the shell itself.  Outside both, the first in this frame."
  (or (agent-hq-subagents--context-of (current-buffer))
      (seq-some (lambda (window) (agent-hq-subagents--context-of (window-buffer window)))
                (window-list nil 'nomini))
      (user-error "No agent shell in this frame")))

(defun agent-hq-subagents--show (shell id)
  "Show SHELL's agent ID, or SHELL itself for a nil ID.
In the window showing SHELL or one of its transcripts, if any."
  (let* ((agent (and id (gethash id (buffer-local-value 'agent-hq-subagents--agents shell))))
         (target (if agent (agent-hq-subagents--view-buffer shell agent) shell))
         (ours (cons shell (agent-hq-subagents--views shell)))
         (window (seq-find (lambda (window) (memq (window-buffer window) ours))
                           (cons (selected-window) (window-list nil 'nomini)))))
    (when agent
      (agent-hq-subagents--view-update target))
    (if window
        (progn (select-window window)
               (switch-to-buffer target nil t))
      (pop-to-buffer target (bound-and-true-p agent-shell-display-action)))
    (when (and agent (eq (current-buffer) target))
      (goto-char (point-max)))))

(defun agent-hq-subagents--ring (shell current)
  "Return what SHELL's next and previous step through: nil for main, then ids.
The ids are those of the listed agents, plus CURRENT, the one shown."
  (let ((now (float-time)))
    (cons nil (mapcar #'agent-hq-subagents-agent-id
                      (seq-filter (lambda (agent)
                                    (or (agent-hq-subagents--listed-p agent now)
                                        (equal (agent-hq-subagents-agent-id agent) current)))
                                  (with-current-buffer shell (agent-hq-subagents--scan)))))))

(defun agent-hq-subagents--step (n)
  "Show the agent N steps on from the one shown, main counting as one."
  (pcase-let* ((`(,shell . ,current) (agent-hq-subagents--context))
               (ring (agent-hq-subagents--ring shell current)))
    (unless (cdr ring)
      (user-error "No agents working lately; %s lists all of this session's"
                  (substitute-command-keys "\\[agent-hq-subagents-view]")))
    (agent-hq-subagents--show
     shell (nth (mod (+ (or (seq-position ring current) 0) n) (length ring)) ring))))

;;;###autoload
(defun agent-hq-subagents-next ()
  "Show the next of main and the agents listed above its prompt."
  (interactive)
  (agent-hq-subagents--step 1))

;;;###autoload
(defun agent-hq-subagents-previous ()
  "Show the previous of main and the agents listed above its prompt."
  (interactive)
  (agent-hq-subagents--step -1))

;;;###autoload
(defun agent-hq-subagents-main ()
  "Go back from an agent's transcript to its shell."
  (interactive)
  (agent-hq-subagents--show (car (agent-hq-subagents--context)) nil))

(defun agent-hq-subagents--candidates (agents)
  "Return (LABEL . AGENT) for each of AGENTS, working ones first, then latest.
LABEL is the agent's name and description, made unique."
  (let ((now (float-time))
        (seen (make-hash-table :test #'equal)))
    (mapcar (lambda (agent)
              (let* ((label (string-trim (concat (agent-hq-subagents-agent-name agent) "  "
                                                 (agent-hq--one-line
                                                  (agent-hq-subagents-agent-description agent)
                                                  60))))
                     (count (puthash label (1+ (gethash label seen 0)) seen)))
                (cons (if (> count 1) (format "%s <%d>" label count) label) agent)))
            (sort (copy-sequence agents)
                  (lambda (a b)
                    (let ((working-a (eq (agent-hq-subagents--status a now) 'working))
                          (working-b (eq (agent-hq-subagents--status b now) 'working)))
                      (if (eq working-a working-b)
                          (> (or (agent-hq-subagents-agent-last a) 0)
                             (or (agent-hq-subagents-agent-last b) 0))
                        working-a)))))))

;;;###autoload
(defun agent-hq-subagents-view ()
  "Show the transcript of an agent this shell's session started, or main.
Lists every agent of the session, working ones first."
  (interactive)
  (pcase-let* ((`(,shell . ,current) (agent-hq-subagents--context))
               (agents (or (with-current-buffer shell (agent-hq-subagents--scan t))
                           (user-error "This shell's session has started no agents")))
               (now (float-time))
               (candidates (cons (cons "main" nil) (agent-hq-subagents--candidates agents)))
               (annotate (lambda (label)
                           (when-let* ((agent (cdr (assoc label candidates))))
                             (concat "  " (propertize (agent-hq-subagents--stats agent now)
                                                      'face 'completions-annotations)))))
               (table (lambda (string predicate action)
                        (if (eq action 'metadata)
                            `(metadata (category . agent-hq-subagent)
                                       (display-sort-function . identity)
                                       (cycle-sort-function . identity)
                                       (annotation-function . ,annotate))
                          (complete-with-action action candidates string predicate))))
               (default (if current
                            "main"
                          (car (seq-find (lambda (candidate)
                                           (and (cdr candidate)
                                                (eq (agent-hq-subagents--status (cdr candidate) now)
                                                    'working)))
                                         candidates))))
               (choice (completing-read "Agent: " table nil t nil nil default)))
    (agent-hq-subagents--show
     shell (when-let* ((agent (cdr (assoc choice candidates))))
             (agent-hq-subagents-agent-id agent)))))

(provide 'agent-hq-subagents)
;;; agent-hq-subagents.el ends here
