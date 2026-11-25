;;; agent-hq-logo.el --- Animated agent logos in shell headers -*- lexical-binding: t; -*-

;;; Commentary:

;; agent-shell's graphical header shows the agent's logo left of two
;; lines of text, all drawn as one SVG image.  Here the logos of Claude
;; and Codex move while their agent works, from a turn's first beat to
;; its last: Claude's critter trots in place and blinks now and then,
;; Codex's knot turns.  Idle, they stand still and look as before.
;;
;; agent-shell's own header animation drew the whole image anew for each
;; frame, which churned GC (agent-shell#718).  Here the header is cut in
;; two images at the logo's right edge instead: the logo, and the text
;; beside it.  Logo frames are small images made once for each size and
;; shared by all shells, and the header string pairing a frame with the
;; text image is made the first time that frame shows, then kept until
;; the text changes.  A frame (5 a second while busy, on the clock every
;; animation shares, agent-hq-clock) only puts its string in
;; `header-line-format': it allocates nothing, and the text image is
;; never drawn again.
;;
;; Only a shell's own header moves, and only while a window shows it.
;; Other agents keep agent-shell's logo as it is.
;;
;; Turn it on with `agent-hq-logo-setup'.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'map)
(require 'seq)
(require 'svg)
(require 'agent-hq-clock)

(declare-function agent-shell--text-change-unsafe-p "agent-shell")
(defvar agent-shell--state)
(defvar agent-shell-header-style)

(defgroup agent-hq-logo nil
  "Animated agent logos in shell headers."
  :group 'agent-hq
  :prefix "agent-hq-logo-")

(defcustom agent-hq-logo-animations
  '(("claudecode.png" 36 agent-hq-logo-draw-claude)
    ("openai.png" 10 agent-hq-logo-draw-codex))
  "Logos that move while their agent works, as (ICON-NAME FRAMES DRAW).
ICON-NAME is the agent config's :icon-name.  FRAMES is how many beats
the animation runs before it starts over, at 10 a second.  DRAW is
called as (DRAW SVG FRAME BOX ICON MODE) to draw into SVG frame FRAME,
counted from 0, or the logo at rest if FRAME is nil.  BOX is the
logo's (X Y WIDTH HEIGHT) in SVG, ICON the data URL of the image
agent-shell shows for it, and MODE the frame's background mode."
  :type '(repeat (list (string :tag "Icon name")
                       (integer :tag "Frames")
                       (function :tag "Draw"))))

(defcustom agent-hq-logo-every 2
  "Beats of the shared clock between logo frames: 2 moves it 5 times a second.
Each frame redraws the header.  The animation keeps its pace either
way, skipping the frames in between."
  :type 'natnum)

(cl-defstruct (agent-hq-logo--header (:constructor agent-hq-logo--make-header)
                                     (:copier nil))
  "A shell header cut in two: its logo, and its text."
  frames draw key icon lead text strings)

(defvar agent-hq-logo--images (make-hash-table :test #'equal)
  "Logo frames made so far, by (FRAME . KEY) of `agent-hq-logo--header'.")

(defvar-local agent-hq-logo--failed nil
  "Non-nil once a beat failed here; the logo then stays as it is.")

;;; Drawing the logos

(defun agent-hq-logo--rect (x y width height)
  "Return SVG path data for the rectangle at X, Y of WIDTH and HEIGHT."
  (format "M%d %dh%dv%dh%dz" x y width height (- width)))

(defun agent-hq-logo-draw-claude (svg frame box _icon mode)
  "Draw Claude Code's critter into SVG at BOX, frame FRAME of 36 or at rest.
For `agent-hq-logo-animations', which see for MODE.  It trots in place,
a step every 3 beats: a pair of legs comes up with one arm while the
other goes down, the critter stands, and the other legs and arm go.
Every 36 beats it blinks.  Drawn on the 640-pixel grid of the icon
agent-shell shows, which is white on a dark background, black on a
light one."
  (pcase-let* ((`(,x ,y ,width ,height) box)
               (step (and frame (/ (mod frame 12) 3)))
               (lifted (pcase step (0 '(0 2)) (2 '(1 3))))
               (arm (pcase step (0 -40) (2 40) (_ 0)))
               (blink (and frame (<= 30 frame 31))))
    (dom-append-child
     svg
     (dom-node 'path
               `((d . ,(concat
                        ;; Body, its eyes cut out (even-odd).
                        (agent-hq-logo--rect 80 133 480 322)
                        (agent-hq-logo--rect 160 (if blink 270 216) 40 (if blink 22 76))
                        (agent-hq-logo--rect 440 (if blink 270 216) 40 (if blink 22 76))
                        ;; Arms.
                        (agent-hq-logo--rect 0 (+ 292 arm) 80 83)
                        (agent-hq-logo--rect 560 (- 292 arm) 80 83)
                        ;; Legs, half as long while up.
                        (mapconcat (lambda (leg)
                                     (agent-hq-logo--rect (nth leg '(120 200 400 480)) 455
                                                          40 (if (memq leg lifted) 39 78)))
                                   '(0 1 2 3))))
                 (fill-rule . "evenodd")
                 (fill . ,(if (eq mode 'light) "#000000" "#ffffff"))
                 (transform . ,(format "translate(%s %s) scale(%s %s)"
                                       x y (/ width 640.0) (/ height 640.0))))))))

(defun agent-hq-logo-draw-codex (svg frame box icon _mode)
  "Draw OpenAI's knot from ICON into SVG at BOX, turned 6° a FRAME of 10.
For `agent-hq-logo-animations'.  The knot looks the same every 60°,
so it turns on without a jump as the frames start over.  Its center
in the 640-pixel icon is (320, 317)."
  (pcase-let ((`(,x ,y ,width ,height) box))
    (dom-append-child
     svg
     (dom-node 'g
               (when frame
                 `((transform . ,(format "rotate(%d %s %s)" (* 6 frame)
                                         (+ x (/ width 2.0))
                                         (+ y (* height (/ 317 640.0)))))))
               (dom-node 'image `((xlink:href . ,icon) (x . ,x) (y . ,y)
                                  (width . ,width) (height . ,height)))))))

;;; Cutting the header in two

(defun agent-hq-logo--animation (model)
  "Return (ICON-NAME FRAMES DRAW) for the logo of header MODEL, if it moves here.
Only in a shell's own graphical header: one with key hints draws them
under the logo, where the cut would go through them."
  (and (eq agent-shell-header-style 'graphical)
       (derived-mode-p 'agent-shell-mode)
       (not (map-elt model :key-hints))
       (assoc (map-elt model :icon-name) agent-hq-logo-animations)))

(defun agent-hq-logo--cut (svg image-fn)
  "Return header SVG cut at its logo's right edge, or nil if it can't be.
SVG is left as it is.  The result is (STRIP HEIGHT BOX ICON TEXT): the
STRIP pixels at the left hold the logo, at BOX, (X Y WIDTH HEIGHT), from
the data URL ICON; TEXT is the image IMAGE-FN makes of the rest, shifted
left by STRIP.  Nil unless SVG has one image, with all text right of it.
The cut goes halfway between them, so a logo turning past its box still
shows."
  (when-let* ((images (dom-by-tag svg 'image))
              ((null (cdr images)))
              (logo (car images))
              (box (mapcar (lambda (attribute) (dom-attr logo attribute))
                           '(x y width height)))
              ((seq-every-p #'numberp box))
              (icon (dom-attr logo 'xlink:href))
              (lefts (mapcar (lambda (text) (dom-attr text 'x)) (dom-by-tag svg 'text)))
              ((seq-every-p #'numberp lefts))
              (logo-end (+ (nth 0 box) (nth 2 box)))
              ((<= logo-end (seq-min lefts)))
              (strip (floor (+ logo-end (seq-min lefts)) 2))
              (width (dom-attr svg 'width))
              (height (dom-attr svg 'height))
              ((and (numberp width) (numberp height) (< strip width))))
    (let ((rest (copy-sequence svg)))
      (setcar (cdr rest) (copy-alist (dom-attributes svg)))
      (dom-set-attribute rest 'width (- width strip))
      (setcdr (cdr rest)
              (list (apply #'dom-node 'g `((transform . ,(format "translate(%d 0)" (- strip))))
                           (remq logo (dom-children svg)))))
      (list strip height box icon (funcall image-fn rest)))))

(defun agent-hq-logo--image (header frame)
  "Return the image of HEADER's logo at FRAME, nil for at rest.
Made once for all shells with a logo of the same kind, mode and size."
  (let ((key (cons frame (agent-hq-logo--header-key header))))
    (or (gethash key agent-hq-logo--images)
        (puthash key
                 (pcase-let* ((`(,_name ,mode ,strip ,height ,box ,props)
                               (agent-hq-logo--header-key header))
                              (svg (svg-create strip height)))
                   (funcall (agent-hq-logo--header-draw header)
                            svg frame box (agent-hq-logo--header-icon header) mode)
                   (apply #'svg-image svg props))
                 agent-hq-logo--images))))

(defun agent-hq-logo--string (header frame)
  "Return the header string showing HEADER's logo at FRAME, nil for at rest.
Made once per frame and kept, so a beat finds it ready."
  (let* ((strings (agent-hq-logo--header-strings header))
         (index (or frame (1- (length strings)))))
    (or (aref strings index)
        (aset strings index
              (let* ((text (agent-hq-logo--header-text header))
                     ;; The logo takes the text image's properties (its
                     ;; menu on click, the help echo), its own image aside.
                     (logo (substring text 0 1))
                     (string (progn
                               (put-text-property 0 1 'display
                                                  (agent-hq-logo--image header frame) logo)
                               (concat (agent-hq-logo--header-lead header) logo text))))
                (put-text-property 0 (length string) 'agent-hq-logo header string)
                string)))))

(defun agent-hq-logo--frame-at (beat frames)
  "Return which of FRAMES the logo shows at BEAT of the shared clock.
The logo changes every `agent-hq-logo-every' beats, so it skips the
frames in between and keeps the animation's pace."
  (mod (* agent-hq-logo-every (/ beat agent-hq-logo-every)) frames))

(defun agent-hq-logo--frame (frames)
  "Return which of FRAMES this shell's logo shows now, nil while it rests."
  (when-let* ((heartbeat (map-elt (bound-and-true-p agent-shell--state) :heartbeat))
              ((memq (map-elt heartbeat :status) '(started busy))))
    (agent-hq-logo--frame-at agent-hq-clock-beat frames)))

(defun agent-hq-logo--split (string whole cut animation model)
  "Return header STRING with its logo apart, or nil if it can't be.
WHOLE is the image STRING shows the header with, CUT what
`agent-hq-logo--cut' made of it, plus the props the image was made with,
ANIMATION the logo's entry in `agent-hq-logo-animations' and MODEL the
header's."
  (when-let* ((at (cl-loop for at below (length string)
                           ;; `insert-image' conses up a spec of its own,
                           ;; sharing the rest of WHOLE.
                           when (eq (cdr-safe (get-text-property at 'display string))
                                    (cdr whole))
                           return at)))
    (pcase-let* ((`(,strip ,height ,box ,icon ,text ,props) cut)
                 (`(,name ,frames ,draw) animation)
                 (rest (substring string at)))
      (put-text-property 0 1 'display text rest)
      (agent-hq-logo--string
       (agent-hq-logo--make-header
        :frames frames :draw draw :icon icon
        :key (list name (map-elt model :background-mode) strip height box props)
        :lead (substring string 0 at)
        :text rest
        ;; One per frame, and the last for at rest.
        :strings (make-vector (1+ frames) nil))
       (agent-hq-logo--frame frames)))))

(defun agent-hq-logo--render-a (fn model)
  "Around advice for `agent-shell--render-header-model-uncached' (FN, MODEL).
Cuts the header image FN makes in two when its logo moves, see
`agent-hq-logo--cut'.  The image is made whole too, and kept if the
cut doesn't work out."
  (if-let* ((animation (agent-hq-logo--animation model)))
      (let* ((svg-image (symbol-function 'svg-image))
             whole cut)
        (let ((string
               (cl-letf (((symbol-function 'svg-image)
                          (lambda (svg &rest props)
                            ;; Whatever else wraps `svg-image' may change
                            ;; SVG (coloring text, say) before it's drawn,
                            ;; so cut it after.
                            (let ((image (apply svg-image svg props)))
                              (unless whole
                                (setq whole image
                                      cut (ignore-errors
                                            (append (agent-hq-logo--cut
                                                     svg (lambda (rest) (apply svg-image rest props)))
                                                    (list props)))))
                              image))))
                 (funcall fn model))))
          (or (and cut (stringp string)
                   (ignore-errors (agent-hq-logo--split string whole cut animation model)))
              string)))
    (funcall fn model)))

;;; Animating it

(defun agent-hq-logo--show (shell beat)
  "Show SHELL's logo at BEAT of the shared clock, nil for at rest.
At rest it always shows; moving, only every `agent-hq-logo-every' beats
and while a window shows SHELL.  Each change redraws the header."
  (when (buffer-live-p shell)
    (with-current-buffer shell
      (when-let* (((not agent-hq-logo--failed))
                  ((stringp header-line-format))
                  (header (get-text-property 0 'agent-hq-logo header-line-format))
                  ((or (null beat)
                       (and (zerop (mod beat agent-hq-logo-every))
                            ;; Not mid-redisplay: agent-shell skips such beats too.
                            (not (agent-shell--text-change-unsafe-p))
                            (get-buffer-window shell 'visible)))))
        (condition-case err
            (let ((string (agent-hq-logo--string
                           header (and beat (agent-hq-logo--frame-at
                                             beat (agent-hq-logo--header-frames header))))))
              (unless (eq string header-line-format)
                (setq header-line-format string)
                (force-mode-line-update)))
          (error
           ;; Beats come 10 a second: say so once, not on each.
           (setq agent-hq-logo--failed t)
           (agent-hq-clock-stop (cons 'agent-hq-logo shell))
           (message "agent-hq-logo: stopped in %s: %S" (buffer-name) err)))))))

(defun agent-hq-logo--beat (shell _beat status)
  "Start or stop SHELL's logo as its heartbeat's STATUS says.
STATUS is `started', `busy' or `ended', as `agent-shell-heartbeat-make'
says.  While busy the shared clock (agent-hq-clock) moves it, in step
with every other animation; at the end, the logo rests."
  (pcase status
    ('started
     (agent-hq-clock-start (cons 'agent-hq-logo shell)
                           (lambda (beat)
                             (if (buffer-live-p shell)
                                 (agent-hq-logo--show shell beat)
                               (agent-hq-clock-stop (cons 'agent-hq-logo shell))))))
    ('ended
     (agent-hq-clock-stop (cons 'agent-hq-logo shell))
     (agent-hq-logo--show shell nil))))

(defun agent-hq-logo--heartbeat-handler-a (orig shell-buffer)
  "Around advice for `agent-shell--make-heartbeat-handler'.
The handler ORIG makes for SHELL-BUFFER also moves its logo, after
agent-shell has drawn its header for the beat."
  (let ((handler (funcall orig shell-buffer)))
    (lambda (beat status)
      (funcall handler beat status)
      (agent-hq-logo--beat shell-buffer beat status))))

;;;###autoload
(defun agent-hq-logo-setup ()
  "Move Claude's and Codex's logos in the header while they work.
For shells started from now on."
  (advice-add 'agent-shell--render-header-model-uncached :around #'agent-hq-logo--render-a)
  (advice-add 'agent-shell--make-heartbeat-handler :around #'agent-hq-logo--heartbeat-handler-a))

(provide 'agent-hq-logo)

;;; agent-hq-logo.el ends here
