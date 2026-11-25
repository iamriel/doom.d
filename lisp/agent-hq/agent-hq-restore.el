;;; agent-hq-restore.el --- Bring agent shells back after Emacs quits -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs forgets its agent shells when it quits, on purpose or not.  This
;; keeps a short list of them in `agent-hq-restore-file': per shell its
;; agent, session id, directory, Claude account, workspaces and name.  The
;; list is written a couple of seconds after a shell starts, gets a
;; prompt, ends a turn, changes title or is killed, and once more as Emacs
;; quits, so it is current after a crash too.  Nothing runs while shells
;; sit idle, and a write that would change nothing is skipped.
;;
;; At the next start `agent-hq-restore-startup-h' offers those shells back
;; (see `agent-hq-restore-on-startup'): each resumes its session in a new
;; shell, on the account that stores it, in its old workspace (created if
;; need be), and the one you looked at last is shown.  A shell that never
;; got a prompt comes back as a new shell in its directory.  Declined or
;; not, \\[agent-hq-restore-shells] restores them later.
;;
;; persp-mode, which Doom's workspaces and its `doom/quickload-session'
;; (SPC q l) save and load, kept shells as plain buffers and brought them
;; back as dead `agent-shell-mode' buffers with no agent.  With
;; `agent-hq-restore-persp-setup' a saved workspace keeps its shells'
;; sessions, and loading it resumes them in the windows they had.  A
;; session already open in a shell is never started twice: both ways
;; adopt the shell that has it.
;;
;; A daemon with its own `server-name' keeps a list of its own, so a test
;; daemon never touches the main Emacs's.  While another Emacs that wrote
;; the list still runs, this one neither offers its shells nor writes.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

;; Neither agent-shell nor agent-hq is required here: Doom loads this file
;; at startup to offer the last shells back, before either is needed.

(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--resolve-config-designator "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function shell-maker-set-buffer-name "shell-maker")
(declare-function agent-hq--shells "agent-hq")
(declare-function agent-hq--status "agent-hq")
(declare-function agent-hq--session-title "agent-hq")
(declare-function agent-hq--claude-config-for "agent-hq")
(declare-function agent-hq--strip-uniquifier "agent-hq")
(declare-function agent-hq--count "agent-hq")
(declare-function agent-hq--claude-project-key "agent-hq")
(declare-function agent-hq--claude-default-config "agent-hq")
(declare-function agent-hq-context "agent-hq")
(declare-function agent-hq-context-label "agent-hq")
(declare-function agent-hq-context-root "agent-hq")
(declare-function agent-hq-visit-shell "agent-hq")
(declare-function persp--buffer-in-persps "persp-mode")
(declare-function persp-get-by-name "persp-mode")
(declare-function persp-add-new "persp-mode")
(declare-function persp-add-buffer "persp-mode")
(declare-function persp-p "persp-mode")
(declare-function safe-persp-name "persp-mode")
(declare-function set-persp-parameter "persp-mode")
(declare-function +workspace-new "workspaces")
(defvar agent-shell--state)
(defvar agent-hq--claude-started-with)
(defvar agent-hq--claude-config-override)
(defvar agent-hq--shell-number)
(defvar persp-save-buffer-functions)
(defvar persp-load-buffer-functions)
(defvar *persp-hash*)
(defvar server-name)

(defcustom agent-hq-restore-file (locate-user-emacs-file "agent-hq-shells.eld")
  "File listing the agent shells open in this Emacs, kept up to date.
A daemon whose `server-name' isn't \"server\" uses a file of its own
beside it, named after the server."
  :type 'file
  :group 'agent-hq)

(defcustom agent-hq-restore-on-startup 'ask
  "What a new Emacs does with the shells open when the last one quit.
`ask' offers them back once the startup is over, t restores them
without asking, nil leaves them to \\[agent-hq-restore-shells]."
  :type '(choice (const :tag "Offer them back" ask)
                 (const :tag "Restore them" t)
                 (const :tag "Only on request" nil))
  :group 'agent-hq)

(defcustom agent-hq-restore-save-delay 2
  "Seconds between a shell changing and the list being written.
Changes in that time make a single write."
  :type 'number
  :group 'agent-hq)

(defconst agent-hq-restore--transient-modes
  '(agent-shell-viewport-view-mode agent-shell-viewport-edit-mode
    agent-hq-subagents-view-mode)
  "Modes of buffers that stand in for a shell and go with it.
Workspaces don't save them: on loading they would come back empty.")

(defvar agent-hq-restore--owner nil
  "Whether this Emacs writes the list: t, `other' (another Emacs does) or nil.
Nil until the list was first read.")

(defvar agent-hq-restore--other-pid nil
  "The pid of the other Emacs that wrote the list, while `--owner' is `other'.")

(defvar agent-hq-restore--pending nil
  "Shells of the last Emacs not yet restored or declined, as records.
They stay in the list, so a crash before they are restored keeps them.")

(defvar agent-hq-restore--declined nil
  "Shells of the last Emacs whose offer was declined.
\\[agent-hq-restore-shells] still restores them; the list no longer has them.")

(defvar agent-hq-restore--written nil
  "The text last written to the list, so writing it again is skipped.")

(defvar agent-hq-restore--timer nil
  "Timer of the write that a change scheduled, or nil.")

(defvar agent-hq-restore--frozen nil
  "Non-nil once Emacs is quitting: the list written then is the last.")

(defvar-local agent-hq-restore--prompted nil
  "Non-nil once this shell's session has had a prompt.
Before that the agent may not have stored the session at all, so a
restore starts a new one instead of loading it.")

;;;; Records

(defun agent-hq-restore--string (string)
  "Return STRING without text properties, or nil if it isn't a string."
  (and (stringp string) (substring-no-properties string)))

(defun agent-hq-restore--workspaces (buffer)
  "Return the names of the persp-mode workspaces holding BUFFER."
  (when (and (bound-and-true-p persp-mode) (fboundp 'persp--buffer-in-persps))
    (delq nil (mapcar (lambda (persp)
                        (and persp (agent-hq-restore--string (safe-persp-name persp))))
                      (persp--buffer-in-persps buffer)))))

(defun agent-hq-restore--record (buffer)
  "Return what restoring agent shell BUFFER takes, as a plist, or nil.
Nil while the shell has no session yet, and for remote shells.
:agent is the agent's identifier, :session-id its session, :resume
non-nil once it had a prompt or was resumed (for agents whose stored
sessions this can't look up), :cwd the shell's directory,
:account the Claude config dir it runs on (`default' for ~/.claude),
:name the buffer's name, :title the session's, :workspaces the names
of the workspaces holding it, :busy non-nil while a turn was running
or waiting on you, and :shown when a window last showed it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((state agent-shell--state)
             (resumed (map-elt state :resume-session-id))
             (id (or (map-nested-elt state '(:session :id)) resumed))
             (agent (map-nested-elt state '(:agent-config :identifier))))
        (when (and (stringp id) agent (symbolp agent)
                   (not (file-remote-p default-directory)))
          (list :agent agent
                :session-id (agent-hq-restore--string id)
                :resume (and (or resumed agent-hq-restore--prompted
                                 ;; Shells started before tracking was on.
                                 (> (or (map-nested-elt state '(:usage :total-tokens)) 0) 0))
                             t)
                :cwd (agent-hq-restore--string (expand-file-name default-directory))
                :account (let ((config (bound-and-true-p agent-hq--claude-started-with)))
                           (if (stringp config) (agent-hq-restore--string config) config))
                :name (agent-hq-restore--string (buffer-name))
                :title (agent-hq-restore--string (agent-hq--session-title buffer))
                :workspaces (agent-hq-restore--workspaces buffer)
                :busy (and (memq (agent-hq--status buffer) '(busy blocked)) t)
                :shown (and buffer-display-time (floor (float-time buffer-display-time)))))))))

(defun agent-hq-restore--live-shells ()
  "Return the live agent shells, in the order they started."
  (when (and (featurep 'agent-shell) (require 'agent-hq nil t))
    (sort (copy-sequence (agent-hq--shells))
          (lambda (a b)
            (< (or (buffer-local-value 'agent-hq--shell-number a) 0)
               (or (buffer-local-value 'agent-hq--shell-number b) 0))))))

(defun agent-hq-restore--shell-session-ids (buffer)
  "Return the session ids shell BUFFER has or is loading."
  (let ((state (buffer-local-value 'agent-shell--state buffer)))
    (delq nil (list (map-nested-elt state '(:session :id))
                    (map-elt state :resume-session-id)))))

(defun agent-hq-restore--live-shell (record)
  "Return the live shell that has or is loading RECORD's session, or nil."
  (when-let* ((id (plist-get record :session-id)))
    (seq-find (lambda (shell)
                (member id (ignore-errors (agent-hq-restore--shell-session-ids shell))))
              (agent-hq-restore--live-shells))))

(defun agent-hq-restore--claude-session-file (record)
  "Return the file Claude Code keeps RECORD's session in, or nil.
Nil for other agents, and for directories whose key Claude Code hashes."
  (when-let* (((eq (plist-get record :agent) 'claude-code))
              (key (agent-hq--claude-project-key (plist-get record :cwd))))
    (let ((account (plist-get record :account)))
      (expand-file-name (concat "projects/" key "/" (plist-get record :session-id) ".jsonl")
                        (if (stringp account) account (agent-hq--claude-default-config))))))

(defun agent-hq-restore--resumable-p (record)
  "Return non-nil if RECORD's session can be loaded, not just started anew.
For Claude, whether Claude Code stored it, which it does from the first
prompt on; otherwise whether the shell had a prompt or was resumed."
  (if-let* ((file (agent-hq-restore--claude-session-file record)))
      (file-exists-p file)
    (plist-get record :resume)))

(defun agent-hq-restore--records-to-keep ()
  "Return what the list should hold now: the open shells, then the pending.
A pending shell whose session is open again appears once, as the open one."
  (let* ((live (delq nil (mapcar #'agent-hq-restore--record
                                 (agent-hq-restore--live-shells))))
         (ids (mapcar (lambda (record) (plist-get record :session-id)) live)))
    (append live
            (seq-remove (lambda (record) (member (plist-get record :session-id) ids))
                        agent-hq-restore--pending))))

;;;; The list on disk

(defun agent-hq-restore--file ()
  "Return the file this Emacs keeps its list in.
`agent-hq-restore-file', or for a daemon with another `server-name' a
file beside it named after the server."
  (let ((name (bound-and-true-p server-name)))
    (if (or (not (stringp name)) (equal name "server"))
        agent-hq-restore-file
      (concat (file-name-sans-extension agent-hq-restore-file)
              "." (replace-regexp-in-string "[^[:alnum:]_.-]" "_" name)
              (if-let* ((extension (file-name-extension agent-hq-restore-file)))
                  (concat "." extension)
                "")))))

(defun agent-hq-restore--format (records)
  "Return the text of a list holding RECORDS, written by this Emacs."
  (let ((print-length nil)
        (print-level nil)
        (print-escape-newlines t)
        (print-circle nil))
    (concat ";;; -*- mode: lisp-data; coding: utf-8-unix -*-\n"
            ";; The agent shells open in Emacs, which agent-hq-restore offers back.\n"
            (format "(:version 1 :pid %d\n :shells\n (" (emacs-pid))
            (mapconcat #'prin1-to-string records "\n  ")
            "))\n")))

(defun agent-hq-restore--read ()
  "Return the list on disk as a plist (:pid PID :shells RECORDS), or nil.
A file that is missing, unreadable or from another version gives nil."
  (let ((file (agent-hq-restore--file)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8-unix))
              (insert-file-contents file))
            (let ((data (read (current-buffer))))
              (when (and (plistp data) (eql (plist-get data :version) 1))
                (list :pid (plist-get data :pid)
                      :shells (seq-filter (lambda (record)
                                            (and (plistp record)
                                                 (symbolp (plist-get record :agent))
                                                 (stringp (plist-get record :session-id))
                                                 (stringp (plist-get record :cwd))))
                                          (plist-get data :shells))))))
        (error (message "agent-hq: ignoring %s, not a list of shells (%s)"
                        (abbreviate-file-name file) (car err))
               nil)))))

(defun agent-hq-restore--write (records)
  "Write RECORDS as the list, unless that is what it already holds.
The text goes to a temporary file first, then over the list, so a crash
while writing never leaves half a list."
  (let ((text (agent-hq-restore--format records)))
    (unless (equal text agent-hq-restore--written)
      (let* ((file (agent-hq-restore--file))
             (dir (file-name-directory file)))
        (make-directory dir t)
        (let ((temp (make-temp-file (expand-file-name ".agent-hq-shells-" dir)))
              (coding-system-for-write 'utf-8-unix))
          (condition-case err
              (progn (write-region text nil temp nil 'silent)
                     (rename-file temp file t))
            (error (ignore-errors (delete-file temp))
                   (signal (car err) (cdr err))))))
      (setq agent-hq-restore--written text))))

(defun agent-hq-restore--other-emacs-p (pid)
  "Return non-nil if PID is an Emacs that runs now, other than this one."
  (and (natnump pid)
       (/= pid (emacs-pid))
       (when-let* ((comm (alist-get 'comm (ignore-errors (process-attributes pid)))))
         (string-match-p "emacs" (downcase comm)))))

(defun agent-hq-restore--load ()
  "Read the last Emacs's list once, making its shells pending.
While the Emacs that wrote it still runs, its shells are its own:
nothing is pending and this Emacs doesn't write the list until it quits."
  (unless (eq agent-hq-restore--owner t)
    (let* ((data (agent-hq-restore--read))
           (pid (plist-get data :pid)))
      (if (agent-hq-restore--other-emacs-p pid)
          (setq agent-hq-restore--owner 'other
                agent-hq-restore--other-pid pid)
        (setq agent-hq-restore--owner t
              agent-hq-restore--other-pid nil
              agent-hq-restore--pending
              (if (eql pid (emacs-pid))
                  ;; Written by this Emacs: keep only what it still lists.
                  agent-hq-restore--pending
                (plist-get data :shells)))))))

(defun agent-hq-restore-save ()
  "Write the list of agent shells now, if this Emacs keeps it."
  (interactive)
  (when (timerp agent-hq-restore--timer)
    (cancel-timer agent-hq-restore--timer))
  (setq agent-hq-restore--timer nil)
  (unless agent-hq-restore--frozen
    (with-demoted-errors "agent-hq: couldn't save the shell list: %S"
      (agent-hq-restore--load)
      (when (eq agent-hq-restore--owner t)
        (agent-hq-restore--write (agent-hq-restore--records-to-keep))))))

(defun agent-hq-restore--schedule (&rest _)
  "Write the list in `agent-hq-restore-save-delay' seconds, if not already due."
  (unless (or agent-hq-restore--timer agent-hq-restore--frozen)
    (setq agent-hq-restore--timer
          (run-at-time agent-hq-restore-save-delay nil #'agent-hq-restore-save))))

(defun agent-hq-restore-kill-emacs-h ()
  "Write the list one last time as Emacs quits.  For `kill-emacs-hook'."
  (agent-hq-restore-save)
  (setq agent-hq-restore--frozen t))

(defun agent-hq-restore-track-h ()
  "Keep the list current with this shell.  For `agent-shell-mode-hook'."
  (let ((buffer (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'input-submitted
     :on-event (lambda (_event)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq agent-hq-restore--prompted t)))))
    (dolist (event '(init-finished session-restored session-title-changed
                     input-submitted turn-complete clean-up))
      (agent-shell-subscribe-to :shell-buffer buffer :event event
                                :on-event #'agent-hq-restore--schedule))))

;;;; Restoring

(defun agent-hq-restore--label (record)
  "Return the name RECORD's shell had, without a <N> suffix."
  (let ((name (or (plist-get record :name) (abbreviate-file-name (plist-get record :cwd)))))
    (replace-regexp-in-string "<[0-9]+>\\'" "" name)))

(defun agent-hq-restore--place-label (record)
  "Return where RECORD's shell worked: repo:worktree, or its directory's name."
  (let ((cwd (plist-get record :cwd)))
    (if-let* ((ctx (agent-hq-context cwd)))
        (agent-hq-context-label ctx)
      (file-name-nondirectory (directory-file-name cwd)))))

(defun agent-hq-restore--summary (records)
  "Return RECORDS' places, a count after any with more than one shell."
  (let (places)
    (dolist (record records)
      (let ((place (agent-hq-restore--place-label record)))
        (if-let* ((entry (assoc place places)))
            (cl-incf (cdr entry))
          (push (cons place 1) places))))
    (mapconcat (lambda (entry)
                 (if (> (cdr entry) 1) (format "%s ×%d" (car entry) (cdr entry)) (car entry)))
               (nreverse places) ", ")))

(defun agent-hq-restore--workspace-p (persp)
  "Return non-nil if PERSP is a workspace, not persp-mode's nil (none) one.
`persp-p' is true of nil too."
  (and persp (persp-p persp)))

(defun agent-hq-restore--new-workspace (name record)
  "Create workspace NAME for RECORD's shell without switching to it.
Doom's own workspaces are tagged with the shell's project root, so
SPC p p finds them.  Return the workspace, or nil."
  (condition-case err
      (if (fboundp '+workspace-new)
          ;; It refuses Doom's protected names, such as persp-mode's "none".
          (let ((persp (+workspace-new name))
                (ctx (agent-hq-context (plist-get record :cwd))))
            (when (agent-hq-restore--workspace-p persp)
              (set-persp-parameter '+workspace-project
                                   (file-truename (if ctx (agent-hq-context-root ctx)
                                                    (plist-get record :cwd)))
                                   persp)
              persp))
        (persp-add-new name))
    (error (message "agent-hq: no workspace %s for a restored shell: %s"
                    name (error-message-string err))
           nil)))

(defun agent-hq-restore--place (buffer record)
  "Add BUFFER to the workspaces RECORD's shell was in.
When none of them exists any more, the first is created."
  (when (and (bound-and-true-p persp-mode) (fboundp 'persp-add-buffer))
    (let* ((names (plist-get record :workspaces))
           (persps (seq-filter #'agent-hq-restore--workspace-p
                               (mapcar (lambda (name) (persp-get-by-name name *persp-hash* nil))
                                       names))))
      (unless persps
        (when-let* ((name (car names))
                    (persp (agent-hq-restore--new-workspace name record)))
          (setq persps (list persp))))
      (dolist (persp persps)
        (persp-add-buffer buffer persp nil nil)))))

(defun agent-hq-restore--config (record)
  "Return the agent config to restore RECORD's shell with.
A Claude shell keeps the account it ran on, which can differ from the
one its repo maps to (an old session, or one moved at a plan limit)."
  (let* ((agent (plist-get record :agent))
         (config (or (agent-shell--resolve-config-designator agent)
                     (error "No agent `%s' in agent-shell-agent-configs" agent)))
         (account (plist-get record :account)))
    (if (and (eq agent 'claude-code) account)
        (agent-hq--claude-config-for account config)
      config)))

(defun agent-hq-restore--start (record &optional name no-place)
  "Return a shell for RECORD: the one that has its session, or a new one.
A new shell resumes RECORD's session (or starts a new one if it never
got a prompt) in its directory without being shown, is named NAME or
as before, and joins its old workspaces unless NO-PLACE."
  (require 'agent-shell)
  (require 'agent-hq)
  (or (agent-hq-restore--live-shell record)
      (let* ((cwd (if (file-directory-p (plist-get record :cwd))
                      (file-name-as-directory (plist-get record :cwd))
                    (error "Its directory is gone: %s" (plist-get record :cwd))))
             (account (and (eq (plist-get record :agent) 'claude-code)
                           (plist-get record :account)))
             ;; For the shell's name, which is made before its client.
             (agent-hq--claude-config-override (or account agent-hq--claude-config-override))
             (default-directory cwd)
             (buffer (agent-shell--start :config (agent-hq-restore--config record)
                                         :session-id (and (agent-hq-restore--resumable-p record)
                                                          (plist-get record :session-id))
                                         :session-strategy 'new
                                         :new-session t
                                         :no-focus t
                                         :cwd cwd)))
        (when (buffer-live-p buffer)
          (when-let* ((name (or name (plist-get record :name)))
                      ((not (string-empty-p name))))
            (shell-maker-set-buffer-name buffer name))
          (unless no-place
            (agent-hq-restore--place buffer record))
          buffer))))

(defun agent-hq-restore--restorable (records)
  "Return those of RECORDS whose shell isn't open and whose directory exists."
  (seq-filter (lambda (record)
                (and (file-directory-p (plist-get record :cwd))
                     (not (agent-hq-restore--live-shell record))))
              records))

(defun agent-hq-restore--forget (records)
  "Take RECORDS off the pending and declined shells, and write the list."
  (setq agent-hq-restore--pending (seq-difference agent-hq-restore--pending records)
        agent-hq-restore--declined (seq-difference agent-hq-restore--declined records))
  (agent-hq-restore-save))

(defun agent-hq-restore--decline (records)
  "Keep RECORDS for \\[agent-hq-restore-shells] only, out of the list."
  (setq agent-hq-restore--declined
        (append (seq-difference agent-hq-restore--declined records) records))
  (setq agent-hq-restore--pending (seq-difference agent-hq-restore--pending records))
  (agent-hq-restore-save))

(defun agent-hq-restore--restore (records)
  "Restore the shells of RECORDS and show the one looked at last.
Return the shells."
  (let (shells failed)
    (dolist (record records)
      (condition-case err
          (if-let* ((shell (agent-hq-restore--start record)))
              (push (cons record shell) shells)
            (push record failed))
        (error (push record failed)
               (message "agent-hq: couldn't restore %s: %s"
                        (agent-hq-restore--label record) (error-message-string err)))))
    (setq shells (nreverse shells))
    ;; The ones that failed are kept too: \\[agent-hq-restore-shells] can
    ;; try again, and the list still has them for the next Emacs.
    (agent-hq-restore--forget (mapcar #'car shells))
    (when-let* ((last (car (sort (copy-sequence shells)
                                 (lambda (a b) (> (or (plist-get (car a) :shown) 0)
                                                  (or (plist-get (car b) :shown) 0)))))))
      (agent-hq-visit-shell (cdr last)))
    (let ((busy (seq-filter (lambda (entry) (plist-get (car entry) :busy)) shells)))
      (message "%s%s%s"
               (if shells
                   (format "Restored %s (%s)"
                           (agent-hq--count (length shells) "agent shell")
                           (agent-hq-restore--summary (mapcar #'car shells)))
                 "Restored no agent shells")
               (if failed
                   (format "; %d failed, %s tries again"
                           (length failed) (substitute-command-keys "\\[agent-hq-restore-shells]"))
                 "")
               (if busy
                   (format "; %s mid-turn when Emacs quit: %s"
                           (if (cdr busy) "these were" "this one was")
                           (mapconcat (lambda (entry) (buffer-name (cdr entry))) busy ", "))
                 "")))
    (mapcar #'cdr shells)))

(defun agent-hq-restore--pick (records)
  "Ask about each of RECORDS, restore the ones chosen and decline the rest."
  (let (chosen)
    (map-y-or-n-p (lambda (record)
                    (format "Restore %s%s? " (agent-hq-restore--label record)
                            (if (plist-get record :busy) " (was working)" "")))
                  (lambda (record) (push record chosen))
                  records
                  '("agent shell" "agent shells" "restore"))
    (setq chosen (nreverse chosen))
    (agent-hq-restore--decline (seq-difference records chosen))
    (if chosen
        (agent-hq-restore--restore chosen)
      (message "Restored no agent shells; %s still can"
               (substitute-command-keys "\\[agent-hq-restore-shells]")))))

(defun agent-hq-restore--gone (records)
  "Forget those of RECORDS whose directory is gone and say so; return the rest."
  (let ((gone (seq-remove (lambda (record) (file-directory-p (plist-get record :cwd)))
                          records)))
    (when gone
      (setq agent-hq-restore--pending (seq-difference agent-hq-restore--pending gone)
            agent-hq-restore--declined (seq-difference agent-hq-restore--declined gone))
      (message "agent-hq: not restoring %s, its directory is gone: %s"
               (agent-hq--count (length gone) "shell")
               (mapconcat (lambda (record) (abbreviate-file-name (plist-get record :cwd)))
                          gone ", ")))
    (seq-difference records gone)))

(defun agent-hq-restore--ask (records)
  "Ask what to do with RECORDS: return `all', `pick' or `no'."
  (pcase (car (read-multiple-choice
               (format "Restore the %s open when Emacs quit (%s)? "
                       (agent-hq--count (length records) "agent shell")
                       (agent-hq-restore--summary records))
               `((?y "yes" "Restore them all")
                 (?n "no" ,(substitute-command-keys
                            "Not now: \\[agent-hq-restore-shells] restores them later"))
                 (?p "pick" "Ask about each one"))))
    (?y 'all)
    (?p 'pick)
    (_ 'no)))

(defun agent-hq-restore--offer (&optional ask)
  "Offer back the shells open when the last Emacs quit.
With ASK nil they are restored without asking."
  (require 'agent-hq)
  (agent-hq-restore--load)
  (when (eq agent-hq-restore--owner t)
    (when-let* ((records (agent-hq-restore--restorable
                          (agent-hq-restore--gone agent-hq-restore--pending))))
      (if (not ask)
          (agent-hq-restore--restore records)
        (pcase (condition-case nil
                   (agent-hq-restore--ask records)
                 ;; C-g: neither yes nor no, so they stay pending.
                 (quit nil))
          ('all (agent-hq-restore--restore records))
          ('pick (agent-hq-restore--pick records))
          ('no (agent-hq-restore--decline records)
               (message "Not restoring them; %s still can"
                        (substitute-command-keys "\\[agent-hq-restore-shells]"))))))))

(defun agent-hq-restore--startup-offer ()
  "Run the offer `agent-hq-restore-on-startup' asks for, reporting errors."
  (with-demoted-errors "agent-hq: couldn't restore the shells: %S"
    (agent-hq-restore--offer (eq agent-hq-restore-on-startup 'ask))))

(defun agent-hq-restore--client-frame-p ()
  "Return non-nil if a frame of this Emacs is one a person sees."
  (seq-some (lambda (frame)
              (or (frame-parameter frame 'client)
                  (and (not (daemonp)) (frame-visible-p frame))))
            (frame-list)))

(defun agent-hq-restore--first-frame-h ()
  "Offer the shells back in the daemon's first frame, then stop waiting."
  (remove-hook 'server-after-make-frame-hook #'agent-hq-restore--first-frame-h)
  (run-with-idle-timer 0.5 nil #'agent-hq-restore--startup-offer))

;;;###autoload
(defun agent-hq-restore-startup-h ()
  "Offer back the shells open when Emacs last quit, once it is up.
For `emacs-startup-hook' or Doom's `doom-after-init-hook'.
Waits until Emacs is idle; a daemon that should ask waits for its first
frame.  See `agent-hq-restore-on-startup'."
  (when (and agent-hq-restore-on-startup (not noninteractive))
    (if (and (eq agent-hq-restore-on-startup 'ask) (daemonp)
             (not (agent-hq-restore--client-frame-p)))
        (add-hook 'server-after-make-frame-hook #'agent-hq-restore--first-frame-h)
      (run-with-idle-timer 0.5 nil #'agent-hq-restore--startup-offer))))

;;;###autoload
(defun agent-hq-restore-shells (&optional pick)
  "Bring back the agent shells that were open when Emacs last quit.
Each resumes its session in a new shell, on the account that stores it,
in its old workspace, and the one you looked at last is shown.  Shells
whose session is open again, or whose directory is gone, are left out.
With PICK (\\[universal-argument]), ask about each one."
  (interactive "P")
  (require 'agent-hq)
  (agent-hq-restore--load)
  (when (eq agent-hq-restore--owner 'other)
    (user-error "Emacs %d keeps those shells; they are open there" agent-hq-restore--other-pid))
  (let ((records (agent-hq-restore--restorable
                  (agent-hq-restore--gone (append agent-hq-restore--pending
                                                  agent-hq-restore--declined)))))
    (cond ((null records)
           (message "No agent shells to restore: those open when Emacs quit are open again"))
          (pick (agent-hq-restore--pick records))
          (t (agent-hq-restore--restore records)))))

;;;; Workspaces (persp-mode)

(defun agent-hq-restore--stderr-buffer-p (name)
  "Return non-nil if NAME is the name of an ACP client's stderr buffer."
  (and (stringp name) (string-prefix-p "acp-client-stderr(" name)))

(defun agent-hq-restore-persp-save (buffer)
  "Save agent shell BUFFER in its workspace as its session, not its text.
Its viewport, subagent views and the agent's stderr are left out.  For
`persp-save-buffer-functions'; nil for any other buffer."
  (let ((mode (buffer-local-value 'major-mode buffer)))
    (cond ((provided-mode-derived-p mode 'agent-shell-mode)
           (if-let* ((record (ignore-errors (agent-hq-restore--record buffer))))
               (list 'def-agent-hq-shell (agent-hq-restore--string (buffer-name buffer)) record)
             'skip))
          ((or (provided-mode-derived-p mode agent-hq-restore--transient-modes)
               (agent-hq-restore--stderr-buffer-p (buffer-name buffer)))
           'skip))))

(defun agent-hq-restore-persp-load (savelist)
  "Restore a shell a workspace saved, from SAVELIST.
Returns the shell that has its session, or a new one resuming it.
Shells saved as plain buffers, before `agent-hq-restore-persp-save',
and the agent's stderr buffers are skipped: they'd come back empty.
For `persp-load-buffer-functions'; nil for anything else."
  (pcase savelist
    (`(def-agent-hq-shell ,name ,record . ,_)
     (or (condition-case err
             ;; The workspace being loaded takes the shell in.
             (agent-hq-restore--start record name t)
           (error (message "agent-hq: couldn't restore %s: %s" name (error-message-string err))
                  nil))
         'skip))
    (`(def-buffer ,name ,_file ,mode . ,_)
     (when (or (agent-hq-restore--stderr-buffer-p name)
               (and (symbolp mode)
                    (provided-mode-derived-p mode (cons 'agent-shell-mode
                                                        agent-hq-restore--transient-modes))))
       'skip))))

;;;###autoload
(defun agent-hq-restore-persp-setup ()
  "Make persp-mode save and load agent shells as their sessions."
  (add-hook 'persp-save-buffer-functions #'agent-hq-restore-persp-save)
  (add-hook 'persp-load-buffer-functions #'agent-hq-restore-persp-load))

;;;###autoload
(defun agent-hq-restore-setup ()
  "Keep the list of open agent shells current; see the Commentary."
  (unless noninteractive
    (add-hook 'agent-shell-mode-hook #'agent-hq-restore-track-h)
    (add-hook 'kill-emacs-hook #'agent-hq-restore-kill-emacs-h -50)))

(provide 'agent-hq-restore)

;;; agent-hq-restore.el ends here
