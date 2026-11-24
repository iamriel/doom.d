;;; emacs-everywhere-input.el --- Import the active textbox -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)

(defvar my-emacs-everywhere--input-files (make-hash-table :test #'equal)
  "Generated edit files and whether they have a source textbox.")
(defvar-local my-emacs-everywhere--source-textbox 'unknown
  "Whether this buffer has a source textbox, or `unknown' for external files.")

(defconst my-emacs-everywhere--capture-script
  "function run() {
    try {
    ObjC.import('AppKit');
    const se = Application('System Events');
    // App IDs can be bundle IDs or window IDs, not process names. Capture
    // the frontmost process before emacsclient opens the editing frame.
    const app = se.applicationProcesses.whose({frontmost: true})[0];
    if (!app || /^emacs(?:\\s|$)/i.test(app.name())) return 'null';
    let field;
    let role;
    try {
      field = app.attributes.byName('AXFocusedUIElement').value();
      role = field.attributes.byName('AXRole').value();
    } catch (_) { return 'null'; }
    // Never select/copy an entire page or another non-text UI element.
    if (role !== 'AXTextField' && role !== 'AXTextArea' && role !== 'AXComboBox') {
      return 'null';
    }
    let text;
    try {
      const value = field.attributes.byName('AXValue').value();
      if (typeof value === 'string') text = value;
    } catch (_) {}
    // Select the whole textbox so finishing replaces its original contents.
    se.keystroke('a', {using: ['command down']});
    delay(0.05);
    if (text !== undefined) return JSON.stringify(text);
    // Some web editors do not expose AXValue. Only accept a fresh copy.
    const clipboard = $.NSPasteboard.generalPasteboard;
    const previous = clipboard.changeCount;
    se.keystroke('c', {using: ['command down']});
    for (let i = 0; i < 30; i++) {
      delay(0.03);
      if (clipboard.changeCount !== previous) {
        const copied = ObjC.unwrap(clipboard.stringForType($.NSPasteboardTypeString));
        if (typeof copied === 'string') return JSON.stringify(copied);
        break;
      }
    }
    return 'null';
    } catch (_) { return 'null'; }
  }"
  "Capture the source textbox before the Emacs frame takes focus.")

(defun my-emacs-everywhere--capture-text (_app)
  "Return the complete textbox text, or nil if no textbox can be read."
  (condition-case nil
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (when (eq 0 (call-process "osascript" nil t nil
                                   "-l" "JavaScript" "-e"
                                   my-emacs-everywhere--capture-script))
            (let ((text (json-parse-string (buffer-string) :null-object nil)))
              (when (stringp text) text)))))
    (error nil)))

(defun my-emacs-everywhere--optional-app-info (app-info)
  "Allow a macOS session even when APP-INFO cannot find a source window."
  (if (not (eq system-type 'darwin))
      (funcall app-info)
    (condition-case nil
        (or (funcall app-info)
            (make-emacs-everywhere-app :id "" :class "" :title ""))
      (error (make-emacs-everywhere-app :id "" :class "" :title "")))))

(defun my-emacs-everywhere--prepare-input (command-param app &optional file line column)
  "Populate the generated edit file before COMMAND-PARAM launches its frame."
  (let ((params (funcall command-param app file line column)))
    (when (and (eq system-type 'darwin) (not file))
      ;; Put the text in a file, never in emacsclient's argument list.
      (let ((text (my-emacs-everywhere--capture-text app))
            (coding-system-for-write 'utf-8-unix)
            (path (car (last params))))
        (with-file-modes #o600
          (write-region (or text "") nil path nil 'silent))
        (puthash (file-truename path) (stringp text)
                 my-emacs-everywhere--input-files)))
    params))

(defun my-emacs-everywhere--insert-selection (insert-selection)
  "Keep imported or externally supplied file content instead of inserting twice."
  (if (and (eq system-type 'darwin)
           buffer-file-name (file-exists-p buffer-file-name))
      (let ((path (file-truename buffer-file-name)))
        (setq-local my-emacs-everywhere--source-textbox
                    (gethash path my-emacs-everywhere--input-files 'unknown))
        (remhash path my-emacs-everywhere--input-files))
    (funcall insert-selection)))

(defun my-emacs-everywhere-buffer-setup ()
  "Make Emacs Everywhere buffers ready for editing and finishing."
  (set-buffer-modified-p nil)
  (setq-local kill-buffer-query-functions nil)
  (when (null my-emacs-everywhere--source-textbox)
    (setq-local emacs-everywhere--window-focus-command nil
                emacs-everywhere--paste-command nil))
  (when-let* ((process (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag process nil))
  (when (bound-and-true-p evil-local-mode)
    (evil-insert-state)))

;; An older config populated this variable before the package's defcustom,
;; suppressing its default hooks. Restore them without dropping Doom hooks.
(dolist (hook (eval (car (get 'emacs-everywhere-init-hooks 'standard-value)) t))
  (unless (memq hook emacs-everywhere-init-hooks)
    (add-hook 'emacs-everywhere-init-hooks hook t)))
(remove-hook 'emacs-everywhere-init-hooks
             #'emacs-everywhere-remove-trailing-whitespace)

(advice-add 'emacs-everywhere-command-param :around
            #'my-emacs-everywhere--prepare-input)
(advice-add 'emacs-everywhere-app-info :around
            #'my-emacs-everywhere--optional-app-info)
(advice-add 'emacs-everywhere-insert-selection :around
            #'my-emacs-everywhere--insert-selection)

(provide 'emacs-everywhere-input)
;;; emacs-everywhere-input.el ends here
