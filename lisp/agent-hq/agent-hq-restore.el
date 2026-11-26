;;; agent-hq-restore.el --- Bring agent shells back after Emacs quits -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs forgets its agent shells when it quits, on purpose or not.  This
;; keeps a short list of them in `agent-hq-restore-file': per shell its
;; agent, session id, directory, Claude account, workspaces, name, what
;; is typed at its prompt and its queued prompts.  The list is written a
;; couple of seconds after a shell starts, gets a prompt, ends a turn,
;; changes title, is typed in or is killed, and once more as Emacs quits,
;; so it is current after a crash too.  Nothing runs while shells sit
;; idle, and a write that would change nothing is skipped.
;;
;; At the next start `agent-hq-restore-startup-h' offers those shells back
;; (see `agent-hq-restore-on-startup'): each comes back as a shell for its
;; session, on the account that stores it, in its old workspace (created
;; if need be), with its draft back at the prompt and its queue held, and
;; the one you looked at last is shown.  A shell that never got a prompt
;; comes back as a new shell in its directory.  Declined or not,
;; \\[agent-hq-restore-shells] restores them later.
;;
;; A turn doesn't survive Emacs: the agent goes with it.  A shell that
;; was working, or waiting on your permission, comes back saying so, with
;; `agent-hq-restore-continue-prompt' at its prompt for RET to pick the
;; turn up.  Quitting while shells are mid-turn asks first, naming them,
;; in place of Doom's "Really quit Emacs?" (`agent-hq-restore-confirm-quit').
;;
;; Restored shells sleep (`agent-hq-restore-lazy'): the shell is there,
;; in the shell lists and the dashboard, but its agent starts, and its
;; session loads, only once a window shows it.  Ten shells cost nothing
;; until you look at them.
;;
;; persp-mode, which Doom's workspaces and its `doom/quickload-session'
;; (SPC q l) save and load, kept shells as plain buffers and brought them
;; back as dead `agent-shell-mode' buffers with no agent.  With
;; `agent-hq-restore-persp-setup' a saved workspace keeps its shells'
;; sessions, and loading it resumes them in the windows they had.  A
;; session already open in a shell is never started twice: both ways
;; adopt the shell that has it.
;;
;; Doom saves workspaces only when Emacs quits cleanly.  The persp setup
;; also saves them every `agent-hq-restore-workspaces-interval' seconds,
;; while Emacs is idle and only if they changed, to a file of their own
;; that a clean quit deletes.  So when the next Emacs finds one, the last
;; crashed: it is moved aside, to `autosave-crashed' in `persp-save-dir',
;; and the startup offer restores those workspaces, their windows and
;; their shells too.
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
(declare-function agent-shell--handle "agent-shell")
(declare-function agent-shell--resolve-config-designator "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell--prompt-input "agent-shell-prompt")
(declare-function agent-shell--prompt-input-start "agent-shell-prompt")
(declare-function shell-maker-set-buffer-name "shell-maker")
(declare-function agent-hq--shells "agent-hq")
(declare-function agent-hq--status "agent-hq")
(declare-function agent-hq--session-title "agent-hq")
(declare-function agent-hq--claude-config-for "agent-hq")
(declare-function agent-hq--count "agent-hq")
(declare-function agent-hq--claude-project-key "agent-hq")
(declare-function agent-hq--claude-default-config "agent-hq")
(declare-function agent-hq-context "agent-hq")
(declare-function agent-hq-context-label "agent-hq")
(declare-function agent-hq-context-root "agent-hq")
(declare-function agent-hq-visit-shell "agent-hq")
(declare-function agent-hq-queue-refresh "agent-hq-queue")
(declare-function persp--buffer-in-persps "persp-mode")
(declare-function persp-get-by-name "persp-mode")
(declare-function persp-add-new "persp-mode")
(declare-function persp-add-buffer "persp-mode")
(declare-function persp-p "persp-mode")
(declare-function persp-persps "persp-mode")
(declare-function persp-save-state "persp-mode")
(declare-function persps-to-savelist "persp-mode")
(declare-function persp-list-persp-names-in-file "persp-mode")
(declare-function persp-load-state-from-file "persp-mode")
(declare-function safe-persp-name "persp-mode")
(declare-function set-persp-parameter "persp-mode")
(declare-function +workspace-new "workspaces")
(declare-function doom-load-session "sessions")
(defvar agent-shell--state)
(defvar agent-hq--claude-started-with)
(defvar agent-hq--claude-config-override)
(defvar agent-hq--shell-number)
(defvar agent-hq-shell-asleep)
(defvar persp-save-buffer-functions)
(defvar persp-load-buffer-functions)
(defvar persp-before-save-state-to-file-functions)
(defvar persp-save-dir)
(defvar persp-nil-name)
(defvar *persp-hash*)
(defvar *persp-pretend-switched-off*)
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
without asking, nil leaves them to \\[agent-hq-restore-shells].  After a
crash this goes for the workspaces it saved too."
  :type '(choice (const :tag "Offer them back" ask)
                 (const :tag "Restore them" t)
                 (const :tag "Only on request" nil))
  :group 'agent-hq)

(defcustom agent-hq-restore-lazy t
  "Non-nil: a restored shell's agent starts only once a window shows it.
Until then the shell sleeps: it is listed with the others, its draft and
queue are back, but no agent runs and its session isn't loaded.  Nil
starts every restored shell at once."
  :type 'boolean
  :group 'agent-hq)

(defcustom agent-hq-restore-continue-prompt
  "Emacs quit mid-turn, so your last step may not have finished. \
Check where things stand, then carry on."
  "What a restored shell that was mid-turn has at its prompt, not sent.
Its turn stopped when Emacs quit; RET sends this to pick it up.  Nil
leaves the prompt empty.  A shell with a draft of its own keeps that."
  :type '(choice (const :tag "Nothing" nil) string)
  :group 'agent-hq)

(defcustom agent-hq-restore-confirm-quit t
  "Non-nil: quitting Emacs while agent shells are mid-turn asks first.
The question names them and replaces Doom's \"Really quit Emacs?\".
While this Emacs keeps the list, agent-shell's own question about
queued prompts is skipped: they come back, held, with their shells."
  :type 'boolean
  :group 'agent-hq)

(defcustom agent-hq-restore-save-delay 2
  "Seconds between a shell changing and the list being written.
Changes in that time make a single write."
  :type 'number
  :group 'agent-hq)

(defcustom agent-hq-restore-workspaces-interval 60
  "Seconds between saves of the workspaces for after a crash, or nil.
A save waits until Emacs is idle and writes only when they changed.
Doom itself saves them only when Emacs quits cleanly."
  :type '(choice (const :tag "Don't save them" nil) number)
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

(defvar agent-hq-restore--wake-timer nil
  "Timer that wakes the sleeping shells on screen, or nil.")

(defvar agent-hq-restore--workspaces-timer nil
  "Timer that saves the workspaces every so often, or nil.")

(defvar agent-hq-restore--workspaces-idle-timer nil
  "Timer of the workspace save waiting for Emacs to be idle, or nil.")

(defvar agent-hq-restore--workspaces-written nil
  "The text last written to the workspaces file, so writing it again is skipped.")

(defvar agent-hq-restore--workspaces-claimed nil
  "Non-nil once this Emacs moved aside the workspaces a crashed one left.
From then on the workspaces file is this Emacs's own.")

(defvar agent-hq-restore--crashed nil
  "The workspaces file of the Emacs that crashed, moved aside, or nil.
Set when this Emacs found it; the startup offer restores from it.")

(defvar-local agent-hq-restore--prompted nil
  "Non-nil once this shell's session has had a prompt.
Before that the agent may not have stored the session at all, so a
restore starts a new one instead of loading it.")

(defvar-local agent-hq-restore--origin nil
  "The record this shell was restored from, or nil.
While it sleeps, the shell has no session or account of its own yet, so
its record comes from this.")

(defvar-local agent-hq-restore--prefilled nil
  "Non-nil if this restored shell got `agent-hq-restore-continue-prompt'.")

(defvar agent-hq-restore--quit-confirmed nil
  "Non-nil once the question about shells mid-turn was answered yes.
Doom's own question then isn't asked as well.")

;;;; Records

(defun agent-hq-restore--string (string)
  "Return STRING without text properties, or nil if it isn't a string."
  (and (stringp string) (substring-no-properties string)))

(defun agent-hq-restore--ids (records)
  "Return the session ids of RECORDS."
  (mapcar (lambda (record) (plist-get record :session-id)) records))

(defun agent-hq-restore--without (records ids)
  "Return RECORDS but those whose session id is one of IDS."
  (seq-remove (lambda (record) (member (plist-get record :session-id) ids)) records))

(defun agent-hq-restore--asleep-p (buffer)
  "Return non-nil if BUFFER is a restored shell whose agent hasn't started."
  (and (buffer-live-p buffer)
       (boundp 'agent-hq-shell-asleep)
       (buffer-local-value 'agent-hq-shell-asleep buffer)))

(defun agent-hq-restore--workspaces (buffer)
  "Return the names of the persp-mode workspaces holding BUFFER."
  (when (and (bound-and-true-p persp-mode) (fboundp 'persp--buffer-in-persps))
    (delq nil (mapcar (lambda (persp)
                        (and persp (agent-hq-restore--string (safe-persp-name persp))))
                      (persp--buffer-in-persps buffer)))))

(defun agent-hq-restore--draft ()
  "Return what is typed at this shell's prompt, trimmed, or nil if nothing."
  (when (fboundp 'agent-shell--prompt-input)
    (agent-hq-restore--string (ignore-errors (agent-shell--prompt-input)))))

(defun agent-hq-restore--queued (state)
  "Return the prompts queued in shell STATE, or nil."
  (delq nil (mapcar #'agent-hq-restore--string
                    (ignore-errors (map-elt state :pending-prompts)))))

(defun agent-hq-restore--busy (status)
  "Return a record's :busy for shell STATUS: `blocked', t (working) or nil."
  (pcase status
    ('blocked 'blocked)
    ('busy t)))

(defun agent-hq-restore--record (buffer)
  "Return what restoring agent shell BUFFER takes, as a plist, or nil.
Nil while the shell has no session yet, and for remote shells.
:agent is the agent's identifier, :session-id its session, :resume
non-nil once it had a prompt or was resumed (for agents whose stored
sessions this can't look up), :cwd the shell's directory,
:account the Claude config dir it runs on (`default' for ~/.claude),
:name the buffer's name, :title the session's, :workspaces the names
of the workspaces holding it, :busy t while a turn was running or
`blocked' while it waited on your permission, :shown when a window last
showed it, :draft what is typed at its prompt and :queued its queued
prompts.  A sleeping shell keeps what it was restored from."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((state agent-shell--state)
             (origin agent-hq-restore--origin)
             (asleep (and origin (agent-hq-restore--asleep-p buffer)))
             (resumed (map-elt state :resume-session-id))
             (id (or (map-nested-elt state '(:session :id)) resumed
                     (plist-get origin :session-id)))
             (agent (map-nested-elt state '(:agent-config :identifier))))
        (when (and (stringp id) agent (symbolp agent)
                   (not (file-remote-p default-directory)))
          (list :agent agent
                :session-id (agent-hq-restore--string id)
                :resume (if asleep
                            (plist-get origin :resume)
                          (and (or resumed agent-hq-restore--prompted
                                   ;; Shells started before tracking was on.
                                   (> (or (map-nested-elt state '(:usage :total-tokens)) 0) 0))
                               t))
                :cwd (agent-hq-restore--string (expand-file-name default-directory))
                :account (if asleep
                             (plist-get origin :account)
                           (let ((config (bound-and-true-p agent-hq--claude-started-with)))
                             (if (stringp config) (agent-hq-restore--string config) config)))
                :name (agent-hq-restore--string (buffer-name))
                :title (or (agent-hq-restore--string (agent-hq--session-title buffer))
                           (plist-get origin :title))
                :workspaces (agent-hq-restore--workspaces buffer)
                :busy (if asleep
                          (plist-get origin :busy)
                        (agent-hq-restore--busy (agent-hq--status buffer)))
                :shown (and buffer-display-time (floor (float-time buffer-display-time)))
                :draft (agent-hq-restore--draft)
                :queued (agent-hq-restore--queued state)))))))

(defun agent-hq-restore--live-shells ()
  "Return the live agent shells, in the order they started."
  (when (and (featurep 'agent-shell) (require 'agent-hq nil t))
    (sort (copy-sequence (agent-hq--shells))
          (lambda (a b)
            (< (or (buffer-local-value 'agent-hq--shell-number a) 0)
               (or (buffer-local-value 'agent-hq--shell-number b) 0))))))

(defun agent-hq-restore--shell-session-ids (buffer)
  "Return the session ids shell BUFFER has, is loading or was restored from."
  (let ((state (buffer-local-value 'agent-shell--state buffer)))
    (delq nil (list (map-nested-elt state '(:session :id))
                    (map-elt state :resume-session-id)
                    (plist-get (buffer-local-value 'agent-hq-restore--origin buffer)
                               :session-id)))))

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
  (let ((live (delq nil (mapcar #'agent-hq-restore--record
                                (agent-hq-restore--live-shells)))))
    (append live (agent-hq-restore--without agent-hq-restore--pending
                                            (agent-hq-restore--ids live)))))

;;;; Files

(defun agent-hq-restore--server-suffix ()
  "Return \"\" for the main Emacs, or \".NAME\" for a daemon named NAME.
A daemon's name comes from `daemonp': its `server-name' is set only
once its init is over, after the list was first read."
  (let ((name (if (stringp (daemonp)) (daemonp) (bound-and-true-p server-name))))
    (if (or (not (stringp name)) (equal name "server"))
        ""
      (concat "." (replace-regexp-in-string "[^[:alnum:]_.-]" "_" name)))))

(defun agent-hq-restore--file ()
  "Return the file this Emacs keeps its list in.
`agent-hq-restore-file', or for a daemon with another `server-name' a
file beside it named after the server."
  (let ((suffix (agent-hq-restore--server-suffix)))
    (if (string-empty-p suffix)
        agent-hq-restore-file
      (concat (file-name-sans-extension agent-hq-restore-file) suffix
              (if-let* ((extension (file-name-extension agent-hq-restore-file)))
                  (concat "." extension)
                "")))))

(defun agent-hq-restore--write-text (file text)
  "Write TEXT to FILE through a temporary file beside it.
A crash while writing never leaves half a file."
  (let ((dir (file-name-directory file)))
    (make-directory dir t)
    (let ((temp (make-temp-file (expand-file-name (concat "." (file-name-nondirectory file) "-")
                                                  dir)))
          (coding-system-for-write 'utf-8-unix))
      (condition-case err
          (progn (write-region text nil temp nil 'silent)
                 (rename-file temp file t))
        (error (ignore-errors (delete-file temp))
               (signal (car err) (cdr err)))))))

;;;; The list on disk

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
  "Write RECORDS as the list, unless that is what it already holds."
  (let ((text (agent-hq-restore--format records)))
    (unless (equal text agent-hq-restore--written)
      (agent-hq-restore--write-text (agent-hq-restore--file) text)
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
  "Write the list one last time as Emacs quits.  For `kill-emacs-hook'.
A clean quit also deletes the workspaces saved for after a crash."
  (agent-hq-restore-save)
  (when (and (eq agent-hq-restore--owner t) agent-hq-restore--workspaces-claimed)
    (when-let* ((file (agent-hq-restore--workspaces-file)))
      (ignore-errors (delete-file file))))
  (setq agent-hq-restore--frozen t))

;;;; Quitting

(defun agent-hq-restore--mid-turn-shells ()
  "Return the live shells mid-turn, as (BUFFER . STATUS): `busy' or `blocked'."
  (delq nil (mapcar (lambda (shell)
                      (let ((status (ignore-errors (agent-hq--status shell))))
                        (and (memq status '(busy blocked)) (cons shell status))))
                    (agent-hq-restore--live-shells))))

(defun agent-hq-restore--quit-question (cut)
  "Return what quitting asks about CUT, shells mid-turn as (BUFFER . STATUS)."
  (let* ((places (lambda (entries)
                   (agent-hq-restore--summary
                    (mapcar (lambda (entry)
                              (list :cwd (buffer-local-value 'default-directory (car entry))))
                            entries))))
         (working (seq-filter (lambda (entry) (eq (cdr entry) 'busy)) cut))
         (waiting (seq-filter (lambda (entry) (eq (cdr entry) 'blocked)) cut))
         (n-working (length working))
         (n-waiting (length waiting)))
    (concat
     (string-join
      (delq nil
            (list (and working
                       (format "%s %s working (%s)"
                               (agent-hq--count n-working "agent shell")
                               (if (= n-working 1) "is" "are")
                               (funcall places working)))
                  (and waiting
                       ;; "2 are working (…) and 1 waits on you (…)".
                       (format "%s %s on you (%s)"
                               (if working
                                   (number-to-string n-waiting)
                                 (agent-hq--count n-waiting "agent shell"))
                               (if (= n-waiting 1) "waits" "wait")
                               (funcall places waiting)))))
      " and ")
     ".  Quitting stops "
     (if (cdr cut) "their turns" "its turn")
     (if (eq agent-hq-restore--owner t)
         (if (cdr cut) "; the shells come back on restart" "; the shell comes back on restart")
       "")
     ".  Quit anyway? ")))

(defun agent-hq-restore-kill-emacs-query-h ()
  "Ask before Emacs quits while agent shells are mid-turn.
Quitting stops their turns: their agents go with Emacs.  For
`kill-emacs-query-functions'; see `agent-hq-restore-confirm-quit'."
  (setq agent-hq-restore--quit-confirmed nil)
  (if-let* (((and agent-hq-restore-confirm-quit (not noninteractive)))
            (cut (agent-hq-restore--mid-turn-shells)))
      (when (yes-or-no-p (agent-hq-restore--quit-question cut))
        (setq agent-hq-restore--quit-confirmed t))
    t))

(defun agent-hq-restore--confirm-kill-a (confirm &rest args)
  "Around `confirm-kill-emacs' (CONFIRM, ARGS): skip it once quitting was asked.
Then the question about shells mid-turn was the one asked."
  (if agent-hq-restore--quit-confirmed
      (progn (setq agent-hq-restore--quit-confirmed nil) t)
    (apply confirm args)))

(defun agent-hq-restore--queues-kept-p (&rest _)
  "Return non-nil while this Emacs keeps the list, queued prompts and all.
Before-until advice for `agent-shell--prompt-queue-confirm-kill-emacs':
quitting loses no queued prompt then, so there is nothing to ask."
  (and agent-hq-restore-confirm-quit
       (eq agent-hq-restore--owner t)
       (not agent-hq-restore--frozen)))

(defun agent-hq-restore-track-h ()
  "Keep the list current with this shell.  For `agent-shell-mode-hook'.
Commands run in it count as changes too: they type the draft and edit
the queue."
  (let ((buffer (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'input-submitted
     :on-event (lambda (_event)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq agent-hq-restore--prompted t)))))
    (dolist (event '(init-finished session-restored session-title-changed
                     input-submitted turn-complete clean-up
                     ;; Waiting on you, and no longer.
                     permission-request permission-response))
      (agent-shell-subscribe-to :shell-buffer buffer :event event
                                :on-event #'agent-hq-restore--schedule))
    (add-hook 'post-command-hook #'agent-hq-restore--schedule nil t)))

;;;; Sleeping shells

(defun agent-hq-restore--shown-h (frame)
  "Wake the sleeping shells FRAME shows, soon.
For `window-buffer-change-functions', while any shell sleeps."
  (when (seq-some (lambda (window) (agent-hq-restore--asleep-p (window-buffer window)))
                  (window-list frame 'nomini))
    (agent-hq-restore--wake-soon)))

(defun agent-hq-restore--wake-soon ()
  "Wake the sleeping shells on screen once Emacs is idle, if not already due.
A timer that fires while Emacs is already idle uses a plain timer: an idle
one wouldn't fire until the next idle spell."
  (unless (timerp agent-hq-restore--wake-timer)
    (setq agent-hq-restore--wake-timer
          (if (current-idle-time)
              (run-at-time 0.1 nil #'agent-hq-restore--wake-shown)
            (run-with-idle-timer 0.15 nil #'agent-hq-restore--wake-shown)))))

(defun agent-hq-restore--minibuffer-exit-h ()
  "Look again for sleeping shells on screen once the minibuffer is done."
  (remove-hook 'minibuffer-exit-hook #'agent-hq-restore--minibuffer-exit-h)
  (agent-hq-restore--wake-soon))

(defun agent-hq-restore--wake-shown ()
  "Wake the sleeping shells a window shows.
Not while the minibuffer is up, which previews buffers in passing: once
it is done, whatever is still on screen wakes."
  (setq agent-hq-restore--wake-timer nil)
  (if (active-minibuffer-window)
      (add-hook 'minibuffer-exit-hook #'agent-hq-restore--minibuffer-exit-h)
    (dolist (shell (agent-hq-restore--live-shells))
      (when (and (agent-hq-restore--asleep-p shell) (get-buffer-window shell 'visible))
        (agent-hq-restore-wake shell)))))

(defun agent-hq-restore--sleep ()
  "Leave this new shell asleep, to wake once a window shows it."
  (setq-local agent-hq-shell-asleep t)
  (add-hook 'window-buffer-change-functions #'agent-hq-restore--shown-h))

(defun agent-hq-restore-wake (&optional buffer)
  "Start the agent of BUFFER, a restored shell still asleep; by default this one.
It loads the shell's session, as starting the shell would have.  Showing
a sleeping shell does this by itself."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (agent-hq-restore--asleep-p buffer)
      (with-current-buffer buffer
        (setq agent-hq-shell-asleep nil)
        (condition-case err
            (agent-shell--handle :shell-buffer buffer)
          (error (message "agent-hq: couldn't start %s: %s"
                          (buffer-name buffer) (error-message-string err)))))
      (unless (seq-some #'agent-hq-restore--asleep-p (agent-hq-restore--live-shells))
        (remove-hook 'window-buffer-change-functions #'agent-hq-restore--shown-h)))))

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

(defun agent-hq-restore--insert-draft (draft)
  "Put DRAFT at this shell's prompt, before anything typed there since."
  (let ((start (agent-shell--prompt-input-start)))
    (save-excursion
      (goto-char start)
      (insert draft (if (< start (point-max)) "\n\n" "")))
    (when (= (point) start)
      (goto-char (point-max)))))

(defun agent-hq-restore--put-back (record resumed)
  "Give this new shell back RECORD's draft and queued prompts.
The draft goes at the prompt, once there is one; the queue is held
until you send it, as a queue left by a turn that stopped is.  A shell
RESUMED from a session that was mid-turn, with no draft, gets
`agent-hq-restore-continue-prompt' there instead."
  (when-let* ((queued (plist-get record :queued)))
    (setf (map-elt agent-shell--state :pending-prompts)
          (append (map-elt agent-shell--state :pending-prompts) queued))
    (when (fboundp 'agent-hq-queue-refresh)
      (agent-hq-queue-refresh)))
  (when-let* ((draft (or (plist-get record :draft)
                         (and resumed (plist-get record :busy)
                              (stringp agent-hq-restore-continue-prompt)
                              (setq agent-hq-restore--prefilled t)
                              agent-hq-restore-continue-prompt)))
              ((not (string-empty-p draft))))
    (if (agent-shell--prompt-input-start)
        (agent-hq-restore--insert-draft draft)
      (let ((buffer (current-buffer))
            (token nil))
        (setq token (agent-shell-subscribe-to
                     :shell-buffer buffer
                     :event 'prompt-ready
                     :on-event (lambda (_event)
                                 (agent-shell-unsubscribe :subscription token)
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer
                                     (agent-hq-restore--insert-draft draft))))))))))

(defun agent-hq-restore--start (record &optional name no-place)
  "Return a shell for RECORD: the one that has its session, or a new one.
A new shell is for RECORD's session (a new session if it never got a
prompt), in its directory and not shown, with RECORD's draft and queue.
It is named NAME or as before, and joins its old workspaces unless
NO-PLACE.  With `agent-hq-restore-lazy' it sleeps until shown."
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
             (lazy (and agent-hq-restore-lazy (fboundp 'agent-shell--handle)))
             (resumed (and (agent-hq-restore--resumable-p record) (plist-get record :session-id)))
             (start (lambda ()
                      (agent-shell--start :config (agent-hq-restore--config record)
                                          :session-id resumed
                                          :session-strategy 'new
                                          :new-session t
                                          :no-focus t
                                          :cwd cwd)))
             (buffer (if lazy
                         ;; The shell and its prompt, but not its agent.
                         (cl-letf (((symbol-function 'agent-shell--handle) #'ignore))
                           (funcall start))
                       (funcall start))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq agent-hq-restore--origin record)
            (when lazy
              (agent-hq-restore--sleep))
            (with-demoted-errors "agent-hq: couldn't put the draft back: %S"
              (agent-hq-restore--put-back record resumed)))
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

(defun agent-hq-restore--drop (records)
  "Take RECORDS off the pending and declined shells."
  (let ((ids (agent-hq-restore--ids records)))
    (setq agent-hq-restore--pending (agent-hq-restore--without agent-hq-restore--pending ids)
          agent-hq-restore--declined (agent-hq-restore--without agent-hq-restore--declined ids))))

(defun agent-hq-restore--forget (records)
  "Take RECORDS off the pending and declined shells, and write the list."
  (agent-hq-restore--drop records)
  (agent-hq-restore-save))

(defun agent-hq-restore--decline (records)
  "Keep RECORDS for \\[agent-hq-restore-shells] only, out of the list."
  (let ((ids (agent-hq-restore--ids records)))
    (setq agent-hq-restore--declined
          (append (agent-hq-restore--without agent-hq-restore--declined ids) records)
          agent-hq-restore--pending (agent-hq-restore--without agent-hq-restore--pending ids)))
  (agent-hq-restore-save))

(defun agent-hq-restore--restore (records &optional workspaces)
  "Restore the shells of RECORDS and show the one looked at last.
WORKSPACES, the names of workspaces just loaded, may have brought some
of them back already; their windows are left as those had them.
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
    (unless workspaces
      (when-let* ((last (car (sort (copy-sequence shells)
                                   (lambda (a b) (> (or (plist-get (car a) :shown) 0)
                                                    (or (plist-get (car b) :shown) 0)))))))
        (agent-hq-restore-wake (cdr last))
        (agent-hq-visit-shell (cdr last))))
    (let ((asleep (seq-count (lambda (entry) (agent-hq-restore--asleep-p (cdr entry))) shells)))
      (message "%s%s%s%s"
               (cond ((and workspaces shells)
                      (format "Restored %s and %s (%s)"
                              (agent-hq--count (length workspaces) "workspace")
                              (agent-hq--count (length shells) "agent shell")
                              (agent-hq-restore--summary (mapcar #'car shells))))
                     (workspaces
                      (format "Restored %s" (agent-hq--count (length workspaces) "workspace")))
                     (shells
                      (format "Restored %s (%s)"
                              (agent-hq--count (length shells) "agent shell")
                              (agent-hq-restore--summary (mapcar #'car shells))))
                     (t "Restored no agent shells"))
               (if (> asleep 0)
                   (format "; %s once shown" (if (= asleep 1) "one starts" (format "%d start" asleep)))
                 "")
               (if failed
                   (format "; %d failed, %s tries again"
                           (length failed) (substitute-command-keys "\\[agent-hq-restore-shells]"))
                 "")
               (agent-hq-restore--mid-turn-note shells)))
    (mapcar #'cdr shells)))

(defun agent-hq-restore--state-note (record)
  "Return \" (was working)\", \" (was waiting on you)\" or \"\" for RECORD."
  (pcase (plist-get record :busy)
    ('blocked " (was waiting on you)")
    ('nil "")
    (_ " (was working)")))

(defun agent-hq-restore--mid-turn-note (shells)
  "Return what a restore says of the SHELLS, (RECORD . BUFFER) pairs, cut mid-turn.
Their turn stopped with Emacs; their prompt may hold a note to carry on."
  (if-let* ((cut (seq-filter (lambda (entry) (plist-get (car entry) :busy)) shells)))
      (format "; mid-turn when Emacs quit%s: %s"
              (pcase (seq-count (lambda (entry)
                                  (buffer-local-value 'agent-hq-restore--prefilled (cdr entry)))
                                cut)
                (0 "")
                (1 ", with a prompt to carry on")
                (_ ", each with a prompt to carry on"))
              (mapconcat (lambda (entry)
                           (concat (buffer-name (cdr entry))
                                   (and (eq (plist-get (car entry) :busy) 'blocked)
                                        " (was waiting on you)")))
                         cut ", "))
    ""))

(defun agent-hq-restore--pick (records)
  "Ask about each of RECORDS, restore the ones chosen and decline the rest."
  (let (chosen)
    (map-y-or-n-p (lambda (record)
                    (format "Restore %s%s? " (agent-hq-restore--label record)
                            (agent-hq-restore--state-note record)))
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
      (agent-hq-restore--drop gone)
      (message "agent-hq: not restoring %s, its directory is gone: %s"
               (agent-hq--count (length gone) "shell")
               (mapconcat (lambda (record) (abbreviate-file-name (plist-get record :cwd)))
                          gone ", ")))
    (seq-difference records gone)))

(defun agent-hq-restore--places (records)
  "Return RECORDS' places, and how many were mid-turn if any were."
  (let ((cut (seq-count (lambda (record) (plist-get record :busy)) records)))
    (concat (agent-hq-restore--summary records)
            (if (> cut 0) (format "; %d mid-turn" cut) ""))))

(defun agent-hq-restore--ask (records &optional workspaces)
  "Ask what to do with RECORDS and the WORKSPACES a crash left.
Return `all', `shells', `pick' or `no'."
  (pcase (car (read-multiple-choice
               (concat
                (if workspaces
                    (format "Emacs quit unexpectedly.  Restore its %s (%s)%s? "
                            (if (cdr workspaces) "workspaces" "workspace")
                            (string-join workspaces ", ")
                            (if records
                                (format " and %s (%s)"
                                        (agent-hq--count (length records) "agent shell")
                                        (agent-hq-restore--places records))
                              ""))
                  (format "Restore the %s open when Emacs quit (%s)? "
                          (agent-hq--count (length records) "agent shell")
                          (agent-hq-restore--places records))))
               `((?y "yes" ,(if workspaces
                                "Restore the workspaces, their windows and the shells"
                              "Restore them all"))
                 ,@(when (and workspaces records)
                     '((?s "shells" "Only the agent shells, in their workspaces")))
                 ,@(when records
                     '((?p "pick" "Ask about each shell")))
                 (?n "no" ,(substitute-command-keys
                            (if workspaces
                                "Not now: \\[doom/load-session] loads autosave-crashed, \
\\[agent-hq-restore-shells] the shells"
                              "Not now: \\[agent-hq-restore-shells] restores them later"))))))
    (?y 'all)
    (?s 'shells)
    (?p 'pick)
    (_ 'no)))

(defun agent-hq-restore--load-workspaces (file)
  "Load the workspaces saved in FILE, as \\[doom/load-session] would.
Return their names, or nil if that failed."
  (condition-case err
      (let ((names (agent-hq-restore--workspace-names file)))
        (if (fboundp 'doom-load-session)
            (doom-load-session file)
          (persp-load-state-from-file file))
        names)
    (error (message "agent-hq: couldn't load the workspaces in %s: %s"
                    (abbreviate-file-name file) (error-message-string err))
           nil)))

(defun agent-hq-restore--workspace-names (file)
  "Return the names of the workspaces saved in FILE, but persp-mode's none."
  (remove (bound-and-true-p persp-nil-name)
          (ignore-errors (persp-list-persp-names-in-file file))))

(defun agent-hq-restore--offer (&optional ask)
  "Offer back the shells open when the last Emacs quit.
After a crash, the workspaces it saved too.  With ASK nil they are
restored without asking."
  (require 'agent-hq)
  (agent-hq-restore--load)
  (when (eq agent-hq-restore--owner t)
    (let* ((crashed (and agent-hq-restore--crashed (file-readable-p agent-hq-restore--crashed)
                         agent-hq-restore--crashed))
           (workspaces (and crashed (agent-hq-restore--workspace-names crashed)))
           (records (agent-hq-restore--restorable
                     (agent-hq-restore--gone agent-hq-restore--pending))))
      (when (or records workspaces)
        (pcase (if (not ask)
                   'all
                 (condition-case nil
                     (agent-hq-restore--ask records workspaces)
                   ;; C-g: neither yes nor no, so they stay pending.
                   (quit nil)))
          ('all
           (if-let* ((names (and workspaces (agent-hq-restore--load-workspaces crashed))))
               (progn (setq agent-hq-restore--crashed nil)
                      ;; The workspaces brought back the shells they held,
                      ;; which this adopts; the others join them.
                      (agent-hq-restore--restore records names))
             (when records (agent-hq-restore--restore records))))
          ('shells (agent-hq-restore--restore records))
          ('pick (agent-hq-restore--pick records))
          ('no (when records (agent-hq-restore--decline records))
               (message "Not restoring them; %s"
                        (substitute-command-keys
                         (if workspaces
                             "\\[doom/load-session] loads the workspaces (autosave-crashed), \
\\[agent-hq-restore-shells] the shells"
                           "\\[agent-hq-restore-shells] still can")))))))))

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

(defun agent-hq-restore--claim ()
  "Make the list and the workspaces file this Emacs's, if no other Emacs has them.
Writing the list with this Emacs's pid tells an Emacs started later that
this one keeps it."
  (agent-hq-restore--load)
  (when (eq agent-hq-restore--owner t)
    (agent-hq-restore-save)
    (agent-hq-restore--claim-workspaces)))

;;;###autoload
(defun agent-hq-restore-startup-h ()
  "Offer back the shells open when Emacs last quit, once it is up.
For `emacs-startup-hook' or Doom's `doom-after-init-hook'.
Waits until Emacs is idle; a daemon that should ask waits for its first
frame.  See `agent-hq-restore-on-startup'."
  (unless noninteractive
    (with-demoted-errors "agent-hq: couldn't read the shell list: %S"
      (agent-hq-restore--claim))
    (cond ((not agent-hq-restore-on-startup)
           (when agent-hq-restore--crashed
             (message "Emacs quit unexpectedly; %s loads its workspaces (autosave-crashed)"
                      (substitute-command-keys "\\[doom/load-session]"))))
          ((and (eq agent-hq-restore-on-startup 'ask) (daemonp)
                (not (agent-hq-restore--client-frame-p)))
           (add-hook 'server-after-make-frame-hook #'agent-hq-restore--first-frame-h))
          (t (run-with-idle-timer 0.5 nil #'agent-hq-restore--startup-offer)))))

;;;###autoload
(defun agent-hq-restore-shells (&optional pick)
  "Bring back the agent shells that were open when Emacs last quit.
Each comes back for its session, on the account that stores it, in its
old workspace, with its draft and queue, and the one you looked at last
is shown.  Shells whose session is open again, or whose directory is
gone, are left out.  With PICK (\\[universal-argument]), ask about each one."
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
Returns the shell that has its session, or a new one for it.  When the
list has the shell too, its record, which is newer, is used.  Shells
saved as plain buffers, before `agent-hq-restore-persp-save', and the
agent's stderr buffers are skipped: they'd come back empty.
For `persp-load-buffer-functions'; nil for anything else."
  (pcase savelist
    (`(def-agent-hq-shell ,name ,record . ,_)
     (let* ((id (plist-get record :session-id))
            (record (or (seq-find (lambda (pending) (equal (plist-get pending :session-id) id))
                                  agent-hq-restore--pending)
                        record))
            (shell (condition-case err
                       ;; The workspace being loaded takes the shell in.
                       (agent-hq-restore--start record name t)
                     (error (message "agent-hq: couldn't restore %s: %s"
                                     name (error-message-string err))
                            nil))))
       (if (not shell)
           'skip
         (agent-hq-restore--drop (list record))
         (agent-hq-restore--schedule)
         shell)))
    (`(def-buffer ,name ,_file ,mode . ,_)
     (when (or (agent-hq-restore--stderr-buffer-p name)
               (and (symbolp mode)
                    (provided-mode-derived-p mode (cons 'agent-shell-mode
                                                        agent-hq-restore--transient-modes))))
       'skip))))

(defun agent-hq-restore--workspaces-file (&optional crashed)
  "Return the file the workspaces are saved in for after a crash, or nil.
With CRASHED, the one a crashed Emacs's file is moved to.  Both are in
`persp-save-dir', where \\[doom/load-session] finds them; nil without
persp-mode."
  (when (bound-and-true-p persp-save-dir)
    (expand-file-name (concat (if crashed "autosave-crashed" "autosave-live")
                              (agent-hq-restore--server-suffix))
                      persp-save-dir)))

(defun agent-hq-restore--claim-workspaces ()
  "Move aside the workspaces a crashed Emacs left, once, before saving any.
Only an Emacs that quit uncleanly leaves its file; it goes to
`autosave-crashed' and `agent-hq-restore--crashed' says so."
  (unless agent-hq-restore--workspaces-claimed
    (when-let* ((file (agent-hq-restore--workspaces-file)))
      (when (file-exists-p file)
        (let ((crashed (agent-hq-restore--workspaces-file t)))
          (rename-file file crashed t)
          (setq agent-hq-restore--crashed crashed)))
      (setq agent-hq-restore--workspaces-claimed t))))

(defun agent-hq-restore--without-persp-files (savelist)
  "Return SAVELIST without `persp-file' parameters, as persp-mode saves it."
  (mapcar (lambda (persp)
            (mapcar (lambda (part)
                      (if (eq (car-safe part) 'def-params)
                          (list 'def-params (seq-remove (lambda (param)
                                                          (eq (car-safe param) 'persp-file))
                                                        (cadr part)))
                        part))
                    persp))
          savelist))

(defun agent-hq-restore--workspaces-text (file)
  "Return the text of a workspaces file for FILE, of persp-mode's state now.
As `persp-save-state-to-file' writes, without pretty-printing it, which
takes most of its time.

Quietly, neither shown nor logged: persp-mode reports in full each
workspace parameter it can't save and drops, such as the `winner-ring'
of window configurations Doom keeps on each, and this runs every
`agent-hq-restore-workspaces-interval' seconds."
  (let ((inhibit-message t)
        (message-log-max nil))
    (mapc #'persp-save-state (persp-persps))
    (run-hook-with-args 'persp-before-save-state-to-file-functions file *persp-hash* nil)
    (let ((print-length nil)
          (print-level nil)
          (print-circle nil)
          (print-escape-newlines t))
      (concat ";; -*- mode: lisp-data -*-\n"
              ";; The workspaces of a running Emacs, for after a crash (agent-hq-restore).\n"
              (prin1-to-string (agent-hq-restore--without-persp-files (persps-to-savelist)))
              "\n"))))

(defun agent-hq-restore-save-workspaces ()
  "Save the workspaces now for after a crash, if this Emacs keeps the list.
A clean quit deletes the file again: Doom saves them then."
  (interactive)
  (when (timerp agent-hq-restore--workspaces-idle-timer)
    (cancel-timer agent-hq-restore--workspaces-idle-timer))
  (setq agent-hq-restore--workspaces-idle-timer nil)
  (when (and (bound-and-true-p persp-mode)
             (not (bound-and-true-p *persp-pretend-switched-off*))
             (not agent-hq-restore--frozen)
             ;; It may show a buffer in passing.
             (not (active-minibuffer-window)))
    (with-demoted-errors "agent-hq: couldn't save the workspaces: %S"
      (agent-hq-restore--load)
      (when (eq agent-hq-restore--owner t)
        (agent-hq-restore--claim-workspaces)
        (when-let* ((file (agent-hq-restore--workspaces-file)))
          (let ((text (agent-hq-restore--workspaces-text file)))
            (unless (equal text agent-hq-restore--workspaces-written)
              (agent-hq-restore--write-text file text)
              (setq agent-hq-restore--workspaces-written text))))))))

(defun agent-hq-restore--workspaces-tick ()
  "Save the workspaces now if Emacs is idle, else once it is."
  (unless (timerp agent-hq-restore--workspaces-idle-timer)
    (let ((idle (current-idle-time)))
      (if (and idle (>= (float-time idle) 1))
          (agent-hq-restore-save-workspaces)
        (setq agent-hq-restore--workspaces-idle-timer
              (run-with-idle-timer 1 nil #'agent-hq-restore-save-workspaces))))))

;;;###autoload
(defun agent-hq-restore-persp-setup ()
  "Make persp-mode save and load agent shells as their sessions.
Also save the workspaces every `agent-hq-restore-workspaces-interval'
seconds, for after a crash."
  (add-hook 'persp-save-buffer-functions #'agent-hq-restore-persp-save)
  (add-hook 'persp-load-buffer-functions #'agent-hq-restore-persp-load)
  (when (and agent-hq-restore-workspaces-interval (not noninteractive)
             (not (timerp agent-hq-restore--workspaces-timer)))
    (setq agent-hq-restore--workspaces-timer
          (run-at-time agent-hq-restore-workspaces-interval agent-hq-restore-workspaces-interval
                       #'agent-hq-restore--workspaces-tick))
    (add-hook 'kill-emacs-hook #'agent-hq-restore-kill-emacs-h -50)))

;;;###autoload
(defun agent-hq-restore-setup ()
  "Keep the list of open agent shells current; see the Commentary."
  (unless noninteractive
    (add-hook 'agent-shell-mode-hook #'agent-hq-restore-track-h)
    (add-hook 'kill-emacs-hook #'agent-hq-restore-kill-emacs-h -50)
    ;; One question when quitting stops turns, not three.
    (add-hook 'kill-emacs-query-functions #'agent-hq-restore-kill-emacs-query-h)
    (when (functionp confirm-kill-emacs)
      (add-function :around confirm-kill-emacs #'agent-hq-restore--confirm-kill-a))
    (advice-add 'agent-shell--prompt-queue-confirm-kill-emacs
                :before-until #'agent-hq-restore--queues-kept-p)))

(provide 'agent-hq-restore)

;;; agent-hq-restore.el ends here
