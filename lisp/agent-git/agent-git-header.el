;;; agent-git-header.el --- Model, plan usage and session time in agent-shell headers -*- lexical-binding: t; -*-

;;; Commentary:

;; Adds to the header of every agent-shell:
;;
;;   Claude ➤ Opus 5.5 ➤ Xhigh ➤ Auto ➤ 38k/1m (4%)
;;   webapp ➤ 5h 2% (12:10pm) · week 3% (Mon 7pm) ➤ 1h 05m
;;
;; - The model Claude actually answered with, where agent-shell shows the
;;   picker's label ("Default (recommended)").  Before the first answer it
;;   is read from the picker's description ("Opus 5.5 · Best for …").
;; - The account's 5-hour and weekly plan usage, with when each resets.
;;   Each account (see `agent-git-claude-config-dirs') has its own.  Claude
;;   Code reports them through the Agent SDK's usage call, the data behind
;;   /usage (agent-git-usage.mjs), so nothing here reads credentials and no
;;   model is called.  They're fetched in the background and kept for
;;   `agent-git-header-usage-ttl' seconds.  A window the plan doesn't have
;;   (or that isn't running) is left out.
;; - How long the shell has been open, refreshed every minute.
;;
;; Codex shells only get the session time.  Turn it all on with
;; `agent-git-header-setup'.

;;; Code:

(require 'agent-git)
(require 'iso8601)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell--update-header-and-mode-line "agent-shell")
(declare-function agent-shell--current-model-id "agent-shell-config")
(declare-function agent-shell--get-available-models "agent-shell-config")
(defvar agent-shell--state)
(defvar agent-shell-header-style)

(defgroup agent-git-header nil
  "Model, plan usage and session time in agent-shell headers."
  :group 'agent-git
  :prefix "agent-git-header-")

(defcustom agent-git-header-usage-ttl 180
  "Seconds plan usage is shown before it is fetched again."
  :type 'natnum)

(defcustom agent-git-header-claude-sdk nil
  "The Agent SDK's sdk.mjs, or nil for the copy claude-agent-acp ships."
  :type '(choice (const :tag "claude-agent-acp's" nil) file))

(defconst agent-git-header--script
  (expand-file-name "agent-git-usage.mjs"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The script that prints an account's plan usage.")

;;;; The model Claude answered with

(defvar-local agent-git-header--answered-with nil
  "(CHOICE . MODEL): Claude last answered here with MODEL, picker on CHOICE.")

(defun agent-git-header-model-name (model)
  "Return a short name for MODEL, an API model id.
\"claude-opus-5-5\" gives \"Opus 5.5\"; ids it doesn't know come back as
they are."
  (let ((case-fold-search nil)
        (id (replace-regexp-in-string "\\[[^]]*\\]\\'" "" model)))
    (if (string-match (rx bos "claude-" (group (+ (any "a-z"))) "-" (group (+ digit))
                          (? "-" (group (** 1 2 digit)))
                          (? "-" (= 8 digit))
                          eos)
                      id)
        (concat (capitalize (match-string 1 id)) " " (match-string 2 id)
                (when (match-string 3 id) (concat "." (match-string 3 id))))
      model)))

(defun agent-git-header--leading-model (description)
  "Return the model DESCRIPTION starts with, as in \"Opus 5.5 · Best for …\"."
  (when (and (stringp description)
             (string-match "\\`\\([^·\n]+?\\) *·" description))
    (match-string 1 description)))

(defun agent-git-header--described-model (state choice)
  "Return the model that picker entry CHOICE stands for in STATE, or nil.
Its description either starts with it (\"Opus 5.5 · Best for …\") or, as
for \"default\" in Claude's config options, names another entry (\"Opus\"),
whose description or name then gives it (\"Opus 5.5\")."
  (let* ((models (agent-shell--get-available-models state))
         (entry-for (lambda (id)
                      (seq-find (lambda (model) (equal (map-elt model :model-id) id)) models)))
         (description (map-elt (funcall entry-for choice) :description)))
    (when (stringp description)
      (or (agent-git-header--leading-model description)
          (when-let* ((alias (funcall entry-for (downcase (string-trim description))))
                      ((not (equal (map-elt alias :model-id) choice))))
            (or (agent-git-header--leading-model (map-elt alias :description))
                (map-elt alias :name)))))))

(defun agent-git-header--model (state label)
  "Return the model to show for STATE in place of LABEL, the picker's name."
  (let ((choice (agent-shell--current-model-id state)))
    (if (and agent-git-header--answered-with
             (equal (car agent-git-header--answered-with) choice))
        (agent-git-header-model-name (cdr agent-git-header--answered-with))
      (or (agent-git-header--described-model state choice) label))))

(defun agent-git-header--note-model (buffer event)
  "Remember the model of BUFFER's usage-update EVENT, and redraw if it's new."
  (when-let* ((model (map-nested-elt event '(:data :meta _claude/model)))
              ((stringp model))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((seen (cons (agent-shell--current-model-id agent-shell--state) model)))
        (unless (equal seen agent-git-header--answered-with)
          (setq agent-git-header--answered-with seen)
          (agent-shell--update-header-and-mode-line))))))

;;;; Plan usage per account

(defvar agent-git-header--usage (make-hash-table :test #'equal)
  "Plan usage per account, keyed by CLAUDE_CONFIG_DIR (\"\" for the default).
Each value is a plist: :data, the parsed JSON; :time, when it was last
fetched or tried; :process, the fetch running now.")

(defun agent-git-header--account ()
  "Return this shell's usage key, or nil if it isn't a Claude shell.
Only Claude clients are made through `agent-git-claude-environment-a',
which records the account."
  (pcase agent-git--claude-started-with
    ('nil nil)
    ('default "")
    (dir dir)))

(defun agent-git-header--usage-command ()
  "Return the command that prints plan usage, or nil if node or the SDK is missing."
  (let* ((acp (executable-find "claude-agent-acp"))
         (sdk (or agent-git-header-claude-sdk
                  (when-let* ((acp)
                              (root (locate-dominating-file (file-truename acp)
                                                            "package.json")))
                    (expand-file-name "node_modules/@anthropic-ai/claude-agent-sdk/sdk.mjs"
                                      root))))
         ;; The node claude-agent-acp runs on, so fnm's PATH doesn't matter.
         (node (or (and acp (let ((node (expand-file-name "node" (file-name-directory acp))))
                              (and (file-executable-p node) node)))
                   (executable-find "node"))))
    (when (and node sdk (file-readable-p sdk))
      (list node agent-git-header--script sdk))))

(defun agent-git-header--parse-usage (buffer)
  "Return the usage JSON in BUFFER's last line that starts with \"{\"."
  (with-current-buffer buffer
    (goto-char (point-max))
    (when (re-search-backward "^{" nil t)
      (ignore-errors (json-parse-buffer :object-type 'alist :null-object nil)))))

(defun agent-git-header--fetch-usage (account)
  "Start fetching ACCOUNT's plan usage unless a fetch is already running."
  (let ((entry (gethash account agent-git-header--usage)))
    (unless (process-live-p (plist-get entry :process))
      (setq entry (plist-put (copy-sequence entry) :time (float-time)))
      (when-let* ((command (agent-git-header--usage-command)))
        (let* ((default-directory (expand-file-name "~/"))
               (process-environment
                (cons (if (string-empty-p account)
                          "CLAUDE_CONFIG_DIR"   ; unset: ~/.claude
                        (concat "CLAUDE_CONFIG_DIR=" (directory-file-name account)))
                      process-environment)))
          (setq entry (plist-put entry :process
                                 (make-process
                                  :name "agent-git-usage"
                                  :buffer (generate-new-buffer " *agent-git-usage*")
                                  :command command
                                  :connection-type 'pipe
                                  :noquery t
                                  :sentinel (lambda (process _)
                                              (agent-git-header--usage-done process account)))))))
      (puthash account entry agent-git-header--usage))))

(defun agent-git-header--usage-done (process account)
  "Store what PROCESS fetched for ACCOUNT and redraw that account's shells.
A failed fetch keeps the last usage; it's tried again after the TTL."
  (unless (process-live-p process)
    (let* ((buffer (process-buffer process))
           (data (and (buffer-live-p buffer)
                      (eql (process-exit-status process) 0)
                      (agent-git-header--parse-usage buffer)))
           (entry (gethash account agent-git-header--usage)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (puthash account
               (list :data (or data (plist-get entry :data)) :time (float-time) :process nil)
               agent-git-header--usage)
      (when data
        (agent-git-header--redraw
         (lambda (shell)
           (equal (with-current-buffer shell (agent-git-header--account)) account)))))))

(defun agent-git-header--clock (time with-day zone)
  "Return TIME as \"12:10pm\", or \"Mon 7pm\" with WITH-DAY, in time zone ZONE.
Rounded to the minute, since resets come as 10:59:59.8; \":00\" is dropped."
  (let* ((time (time-add time 30))
         (minute (decoded-time-minute (decode-time time zone))))
    (replace-regexp-in-string
     "[AP]M" #'downcase
     (format-time-string (concat (if with-day "%a " "")
                                 (if (zerop minute) "%-I%p" "%-I:%M%p"))
                         time zone)
     t t)))

(defun agent-git-header--window (label window with-day zone)
  "Return \"LABEL N% (RESET)\" for WINDOW, or nil without a percentage.
RESET is a clock time, with the day when WITH-DAY, in time zone ZONE."
  (when-let* ((window)
              (percent (alist-get 'utilization window))
              ((numberp percent)))
    (concat label " " (format "%d%%" (round percent))
            (when-let* ((resets (alist-get 'resets_at window))
                        ((stringp resets))
                        (time (ignore-errors (encode-time (iso8601-parse resets)))))
              (format " (%s)" (agent-git-header--clock time with-day zone))))))

(defun agent-git-header-format-usage (data &optional zone)
  "Return \"5h 2% (12:10pm) · week 3% (Mon 7pm)\" for DATA, or nil.
DATA is what agent-git-usage.mjs prints, as an alist.  Times are in time
zone ZONE, by default the local one."
  (when-let* ((parts (delq nil (list (agent-git-header--window
                                      "5h" (alist-get 'five_hour data) nil zone)
                                     (agent-git-header--window
                                      "week" (alist-get 'seven_day data) t zone)))))
    (string-join parts " · ")))

(defun agent-git-header--usage-text (account)
  "Return ACCOUNT's plan usage for the header, fetching it when stale."
  (let ((entry (gethash account agent-git-header--usage)))
    (when (or (null entry)
              (> (- (float-time) (or (plist-get entry :time) 0)) agent-git-header-usage-ttl))
      (agent-git-header--fetch-usage account))
    (when-let* ((data (plist-get entry :data)))
      (agent-git-header-format-usage data))))

;;;; Session time

(defvar-local agent-git-header--opened nil
  "When this shell was opened, as a float time.")

(defun agent-git-header-duration (seconds)
  "Return SECONDS as \"12m\" or \"1h 05m\"."
  (let ((minutes (floor seconds 60)))
    (if (< minutes 60)
        (format "%dm" minutes)
      (format "%dh %02dm" (/ minutes 60) (% minutes 60)))))

;;;; The header

(defun agent-git-header--status ()
  "Return the usage and session time to show for this shell, or nil."
  (when-let* ((parts (delq nil
                           (list (when-let* ((account (agent-git-header--account)))
                                   (agent-git-header--usage-text account))
                                 (when agent-git-header--opened
                                   (agent-git-header-duration
                                    (- (float-time) agent-git-header--opened)))))))
    (string-join parts " ➤ ")))

;;;###autoload
(defun agent-git-header-model-a (fn state &rest args)
  "Around advice for `agent-shell--make-header-model' (FN, STATE and ARGS).
Shows the model Claude answered with, then usage and session time after
the project on the header's second line (the session id's place, which
agent-shell leaves empty unless `agent-shell-show-session-id')."
  (let ((model (apply fn state args))
        (shell (map-elt state :buffer)))
    (if (not (buffer-live-p shell))
        model
      (with-current-buffer shell
        (let* ((label (map-elt model :model-name))
               (name (if (and label (agent-git-header--account))
                         (agent-git-header--model state label)
                       label))
               (status (when-let* ((status (agent-git-header--status)))
                         ;; The text header is a mode-line format, where a
                         ;; lone % is a construct; the SVG one is drawn as is.
                         (if (eq agent-shell-header-style 'text)
                             (string-replace "%" "%%" status)
                           status)))
               (session-id (map-elt model :session-id))
               (bottom (if (and session-id status)
                           (concat session-id " ➤ " status)
                         (or session-id status))))
          (append (list (cons :model-name name)
                        (cons :session-id bottom))
                  model))))))

(defun agent-git-header--redraw (predicate)
  "Redraw the header of each shell PREDICATE returns non-nil for."
  (dolist (shell (agent-shell-buffers))
    (when (funcall predicate shell)
      (with-current-buffer shell
        (ignore-errors (agent-shell--update-header-and-mode-line))))))

(defvar agent-git-header--timer nil
  "Redraws visible shells every minute, for the session time.")

(defun agent-git-header--tick ()
  "Redraw visible shells; stop once no shell is left."
  (if (agent-shell-buffers)
      (agent-git-header--redraw (lambda (shell) (get-buffer-window shell t)))
    (cancel-timer agent-git-header--timer)
    (setq agent-git-header--timer nil)))

;;;###autoload
(defun agent-git-header-setup-shell-h ()
  "Track this shell's open time and the model Claude answers with.
For `agent-shell-mode-hook'."
  (setq agent-git-header--opened (float-time))
  (let ((buffer (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'usage-update
     :on-event (lambda (event) (agent-git-header--note-model buffer event))))
  (unless (timerp agent-git-header--timer)
    (setq agent-git-header--timer (run-at-time 60 60 #'agent-git-header--tick))))

;;;###autoload
(defun agent-git-header-setup ()
  "Show the real model, plan usage and session time in agent-shell headers."
  (advice-add 'agent-shell--make-header-model :around #'agent-git-header-model-a)
  (add-hook 'agent-shell-mode-hook #'agent-git-header-setup-shell-h))

(provide 'agent-git-header)
;;; agent-git-header.el ends here
