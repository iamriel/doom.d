;;; agent-hq-logo-test.el -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -L lisp/agent-hq -l tests/agent-hq-logo-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: the functions advised here are stand-ins
;; shaped like agent-shell's.

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'svg)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/agent-hq/agent-hq-logo.el" dir) nil t))

(defvar-local agent-shell--state nil)
(defvar agent-shell-header-style 'graphical)

(define-derived-mode agent-shell-mode fundamental-mode "Shell")

(defconst agent-hq-logo-test--icon "data:image/png;base64,TE9HTw=="
  "Stands in for the data URL of the icon agent-shell embeds.")

(defun agent-shell--render-header-model-uncached (model)
  "Draw MODEL's header as agent-shell's graphical one is laid out.
A 63-pixel icon at 6, 8, the text at 79; key hints under the icon."
  (if (not (eq agent-shell-header-style 'graphical))
      (concat " " (map-elt model :buffer-name))
    (let ((svg (svg-create 700 79)))
      (dom-append-child svg (dom-node 'image `((xlink:href . ,agent-hq-logo-test--icon)
                                               (x . 6) (y . 8) (width . 63) (height . 63))))
      (dom-append-child svg (dom-node 'text '((x . 79) (y . 31))
                                      (dom-node 'tspan nil (map-elt model :buffer-name))))
      (when (map-elt model :key-hints)
        (dom-append-child svg (dom-node 'text '((x . 6) (y . 79))
                                        (dom-node 'tspan nil "q quit"))))
      (propertize (format " %s" (with-temp-buffer
                                  (insert-image (svg-image svg :scale 1))
                                  (buffer-string)))
                  'help-echo "Open settings menu"
                  'mouse-face 'mode-line-highlight))))

(defvar agent-hq-logo-test--beats nil
  "(BEAT STATUS) of each beat agent-shell's own handler saw, latest first.")

(defun agent-shell--make-heartbeat-handler (_shell-buffer)
  (lambda (beat status) (push (list beat status) agent-hq-logo-test--beats)))

(defun agent-shell--text-change-unsafe-p () nil)

(agent-hq-logo-setup)

(defun agent-hq-logo-test--model (&rest fields)
  "Return a header model of FIELDS over a Claude shell's."
  (append (map-into fields 'alist)
          '((:buffer-name . "Claude") (:icon-name . "claudecode.png")
            (:background-mode . dark))))

(defmacro agent-hq-logo-test--in-shell (&rest body)
  "Run BODY in a fresh shell buffer shown in the selected window."
  (declare (indent 0))
  `(let ((shell (generate-new-buffer "*agent-hq-logo-test*"))
         (window-buffer (window-buffer)))
     (unwind-protect
         (with-current-buffer shell
           (agent-shell-mode)
           (set-window-buffer (selected-window) shell)
           ,@body)
       (set-window-buffer (selected-window) window-buffer)
       (kill-buffer shell))))

(defun agent-hq-logo-test--busy (beat)
  "Make this shell busy at BEAT of the shared clock, or idle if nil."
  (setq agent-hq-clock-beat (or beat 0)
        agent-shell--state
        `((:heartbeat (:status . ,(if beat 'busy 'ended)) (:value . ,beat)))))

(defun agent-hq-logo-test--svg (string at)
  "Return the SVG data of the image STRING shows at AT."
  (plist-get (cdr (get-text-property at 'display string)) :data))

(defun agent-hq-logo-test--render (&rest fields)
  "Render the header of a model of FIELDS."
  (agent-shell--render-header-model-uncached (apply #'agent-hq-logo-test--model fields)))

(ert-deftest agent-hq-logo-test-cut ()
  (agent-hq-logo-test--in-shell
    (let* ((header (agent-hq-logo-test--render))
           (logo (agent-hq-logo-test--svg header 1))
           (text (agent-hq-logo-test--svg header 2)))
      ;; " ", the logo, then the text, which keeps the image's properties.
      (should (= (length header) 3))
      (should (equal (get-text-property 1 'help-echo header) "Open settings menu"))
      (should (equal (get-text-property 2 'help-echo header) "Open settings menu"))
      ;; Cut halfway between the icon's right edge (69) and the text (79).
      (should (string-match-p "width=\"74\"" logo))
      (should (string-match-p "<path" logo))
      (should-not (string-match-p "<image" logo))
      (should (string-match-p "width=\"626\"" text))
      (should (string-match-p "translate(-74 0)" text))
      (should (string-match-p "Claude" text))
      (should-not (string-match-p "<image" text)))))

(ert-deftest agent-hq-logo-test-left-whole ()
  (agent-hq-logo-test--in-shell
    ;; Key hints run under the logo.
    (should (= (length (agent-hq-logo-test--render :key-hints '(((:key . "q"))))) 2))
    ;; Another agent's logo.
    (should (= (length (agent-hq-logo-test--render :icon-name "gemini.png")) 2))
    (let ((agent-shell-header-style 'text))
      (should (equal (agent-hq-logo-test--render) " Claude"))))
  ;; Not a shell (a viewport, say).
  (with-temp-buffer
    (let ((header (agent-hq-logo-test--render)))
      (should (= (length header) 2))
      (should (string-match-p "<image" (agent-hq-logo-test--svg header 1))))))

(ert-deftest agent-hq-logo-test-render-busy ()
  (agent-hq-logo-test--in-shell
    (cl-flet ((logo (beat)
                (agent-hq-logo-test--busy beat)
                (agent-hq-logo-test--svg (agent-hq-logo-test--render) 1)))
      ;; The logo moves every other beat: beat 37 shows frame 36, which is
      ;; frame 0 of 36, a step, as beat 1 does: not the logo at rest.
      (should (equal (logo 37) (logo 1)))
      (should-not (equal (logo 37) (logo nil)))
      ;; Frame 4 stands between steps.
      (should (equal (logo 40) (logo nil))))))

(ert-deftest agent-hq-logo-test-beat ()
  (agent-hq-logo-test--in-shell
    (let* ((handler (agent-shell--make-heartbeat-handler shell))
           (key (cons 'agent-hq-logo shell))
           (rest (agent-hq-logo-test--render))
           (agent-hq-logo-test--beats nil))
      (unwind-protect
          (progn
            (setq header-line-format rest)
            ;; The turn's start sets the logo going on the shared clock.
            (funcall handler 0 'started)
            (should (agent-hq-clock-running-p key))
            (let ((animate (alist-get key agent-hq-clock--animations nil nil #'equal)))
              ;; The heartbeat's own beats draw nothing; agent-shell's
              ;; handler still runs.
              (funcall handler 5 'busy)
              (should (equal agent-hq-logo-test--beats '((5 busy) (0 started))))
              (should (eq header-line-format rest))
              (funcall animate 4)
              (let ((four header-line-format))
                (should-not (eq four rest))
                (should (eq (get-text-property 2 'display four)
                            (get-text-property 2 'display rest)))
                ;; Only every other beat redraws the header.
                (funcall animate 5)
                (should (eq header-line-format four))
                ;; Frames are made once: 36 beats later finds the same string.
                (funcall animate 40)
                (should (eq header-line-format four))
                (funcall animate 6)
                (should-not (eq header-line-format four))
                ;; Not shown: left as it is.
                (set-window-buffer (selected-window) (get-buffer-create "*Messages*"))
                (funcall animate 4)
                (should-not (eq header-line-format four))
                ;; At the end, at rest, shown or not, and off the clock.
                (funcall handler nil 'ended)
                (should (eq header-line-format rest))
                (should-not (agent-hq-clock-running-p key)))))
        (agent-hq-clock-stop key)))))

(ert-deftest agent-hq-logo-test-beat-other-header ()
  (agent-hq-logo-test--in-shell
    (setq header-line-format " plain")
    (agent-hq-logo--show shell 4)
    (should (equal header-line-format " plain"))
    (setq header-line-format '(:eval "x"))
    (agent-hq-logo--show shell 4)
    (should (equal header-line-format '(:eval "x")))))

(ert-deftest agent-hq-logo-test-frames-shared ()
  (agent-hq-logo-test--in-shell
    (agent-hq-logo-test--busy 7)
    (let ((one (agent-hq-logo-test--render))
          (two (agent-hq-logo-test--render :buffer-name "Claude 2")))
      (should (eq (get-text-property 1 'display one) (get-text-property 1 'display two)))
      (should-not (eq (get-text-property 2 'display one) (get-text-property 2 'display two))))))

(defun agent-hq-logo-test--frame (name frame)
  "Return the SVG data of frame FRAME of the logo of icon NAME."
  (let ((svg (svg-create 74 79)))
    (funcall (nth 2 (assoc name agent-hq-logo-animations))
             svg frame '(6 8 63 63) agent-hq-logo-test--icon 'dark)
    (with-temp-buffer (svg-print svg) (buffer-string))))

(ert-deftest agent-hq-logo-test-claude-frames ()
  (cl-flet ((frame (frame) (agent-hq-logo-test--frame "claudecode.png" frame)))
    ;; A pose holds 3 beats; 4 poses make a step cycle of 12.
    (should (equal (frame 0) (frame 2)))
    (should-not (equal (frame 0) (frame 3)))
    (should-not (equal (frame 0) (frame 6)))
    (should (equal (frame 0) (frame 12)))
    ;; Standing between steps is the logo at rest.
    (should (equal (frame nil) (frame 3)))
    (should (equal (frame nil) (frame 9)))
    ;; It blinks at 30 and 31, mid-step.
    (should-not (equal (frame 30) (frame 18)))
    (should (equal (frame 32) (frame 20)))
    (should (string-match-p "fill=\"#ffffff\"" (frame nil)))))

(ert-deftest agent-hq-logo-test-codex-frames ()
  (cl-flet ((frame (frame) (agent-hq-logo-test--frame "openai.png" frame)))
    (should-not (string-match-p "rotate" (frame nil)))
    (should (string-match-p "rotate(18 37.5 " (frame 3)))
    (should (string-match-p (regexp-quote agent-hq-logo-test--icon) (frame 3)))))

(ert-deftest agent-hq-logo-test-cut-after-other-wrappers ()
  ;; Whatever else wraps `svg-image' around agent-shell's call (the
  ;; statusline colors the text) changes SVG before it's drawn: the text
  ;; image has the change too.
  (let ((recolor (lambda (fn model)
                   (let ((svg-image (symbol-function 'svg-image)))
                     (cl-letf (((symbol-function 'svg-image)
                                (lambda (svg &rest props)
                                  (dolist (tspan (dom-by-tag svg 'tspan))
                                    (dom-set-attribute tspan 'fill "#123456"))
                                  (apply svg-image svg props))))
                       (funcall fn model))))))
    (advice-add 'agent-shell--render-header-model-uncached :around recolor)
    (unwind-protect
        (agent-hq-logo-test--in-shell
          (should (string-match-p "#123456" (agent-hq-logo-test--svg
                                             (agent-hq-logo-test--render) 2))))
      (advice-remove 'agent-shell--render-header-model-uncached recolor))))

;;; agent-hq-logo-test.el ends here
