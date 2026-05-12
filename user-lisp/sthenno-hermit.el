;;; sthenno-hermit.el --- Hermit child-frame companion -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the Hermit child-frame companion.

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'gv)
  (require 'subr-x))

;;; Options

(defgroup sthenno/hermit nil
  "A small image pet attached to an Emacs frame."
  :group 'applications)

(defcustom sthenno/hermit-image-file (locate-user-emacs-file "resources/hmt-01.png")
  "PNG file used as the pet image."
  :type 'file
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-image-height 600
  "Rendered pet image height in pixels."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-image-mask nil
  "Optional mask passed to `create-image'."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Heuristic" heuristic))
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-placement 'outside-right-bottom
  "Initial pet placement relative to the parent frame."
  :type '(choice (const :tag "Inside bottom right" inside-bottom-right)
                 (const :tag "Outside right bottom" outside-right-bottom)
                 (const :tag "Outside left bottom" outside-left-bottom)
                 (const :tag "Center" center))
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-gap 10
  "Gap between the pet, bubble, and parent frame in pixels."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-alpha-background 0
  "Background opacity of the pet image frame."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-no-accept-focus t
  "Whether Hermit child frames avoid accepting focus."
  :type 'boolean
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-timeout 4.0
  "Seconds before the speech bubble is hidden."
  :type '(choice number (const nil))
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-render-delay 0.0125
  "Seconds used to coalesce rapid speech-bubble updates."
  :type 'number
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-message-delay 0.0125
  "Idle seconds used to coalesce echo-area messages."
  :type 'number
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-max-columns 50
  "Maximum display columns used by bubble text before wrapping."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-min-columns 10
  "Minimum display columns used by bubble text."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-max-lines 2
  "Maximum number of text lines shown in the bubble."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-padding-columns 2
  "Horizontal bubble padding in `default' face character cells."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-padding-lines 1
  "Vertical bubble padding in `default' face lines."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-border-width 1
  "Bubble child-frame border width in pixels."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-use-text-box nil
  "Whether to draw an additional `:box' around the bubble text."
  :type 'boolean
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-enable-dnd t
  "Whether Shift-mouse-1 starts drag-and-drop from pet frames."
  :type 'boolean
  :group 'sthenno/hermit)

(defface sthenno/hermit-bubble-face
  '((t :foreground "#303030" :background "#fff7fb"))
  "Speech bubble face."
  :group 'sthenno/hermit)

(defface sthenno/hermit-bubble-border-face
  '((t :background "#ff9ec4"))
  "Speech bubble border face."
  :group 'sthenno/hermit)

(defface sthenno/hermit-bubble-box-face
  '((t :inherit sthenno/hermit-bubble-face :box (:line-width 1 :color "#ff9ec4")))
  "Optional text-box face."
  :group 'sthenno/hermit)

;;; State

(defvar sthenno/hermit-mode nil)
(defvar sthenno/hermit--state nil)
(defvar sthenno/hermit--request nil)

(defun sthenno/hermit--state (key)
  "Return Hermit state value for KEY."
  (plist-get sthenno/hermit--state key))

(gv-define-setter sthenno/hermit--state (value key)
  `(setq sthenno/hermit--state
         (plist-put sthenno/hermit--state ,key ,value)))

(defun sthenno/hermit--set-state (&rest pairs)
  "Update Hermit state by KEY VALUE PAIRS."
  (unless (cl-evenp (length pairs))
    (error "sthenno/hermit--set-state expects even KEY VALUE pairs, got: %S" pairs))
  (cl-loop for (key value) on pairs by #'cddr
           do (setf (sthenno/hermit--state key) value))
  pairs)

(cl-defmacro sthenno/hermit--with-slots (pairs &body body)
  "Update state by PAIRS, then evaluate BODY."
  `(progn
     (sthenno/hermit--set-state ,@pairs)
     ,@body))

(defmacro sthenno/hermit--keymap (&rest bindings)
  "Return a sparse keymap from BINDINGS."
  `(let ((map (make-sparse-keymap)))
     ,@(mapcar (lambda (binding)
                 `(define-key map ,(car binding) #',(cdr binding)))
               bindings)
     map))

(cl-defmacro sthenno/hermit--with-live ((var key predicate) &body body)
  "Bind VAR to live state KEY accepted by PREDICATE, then run BODY."
  (declare (indent 1))
  `(when-let* ((value (sthenno/hermit--state ,key))
               ((funcall ,predicate value)))
     (let ((,var value))
       ,@body)))

(cl-defmacro sthenno/hermit--with-live-frame ((var key) &body body)
  "Bind VAR to live frame state KEY, then run BODY."
  (declare (indent 1))
  `(sthenno/hermit--with-live (,var ,key #'frame-live-p) ,@body))

(cl-defmacro sthenno/hermit--with-live-buffer ((var key) &body body)
  "Bind VAR to live buffer state KEY, then run BODY."
  (declare (indent 1))
  `(sthenno/hermit--with-live (,var ,key #'buffer-live-p) ,@body))

;;; Utility

(defun sthenno/hermit--n (value minimum &optional fallback maximum)
  "Clamp VALUE to MINIMUM and optional MAXIMUM.
Invalid VALUE falls back to FALLBACK, or 0 when FALLBACK is not numeric."
  (let* ((value-number (and (numberp value) (truncate value)))
         (fallback-number (and (numberp fallback) (truncate fallback))))
    (if (null maximum)
        (max minimum (or value-number fallback-number 0))
      (min maximum (max minimum (or value-number fallback-number 0))))))

(defun sthenno/hermit--live-frame (key)
  "Return live frame stored at state KEY."
  (let ((frame (sthenno/hermit--state key)))
    (and (framep frame) (frame-live-p frame) frame)))

(defun sthenno/hermit--live-buffer (key)
  "Return live buffer stored at state KEY."
  (let ((buffer (sthenno/hermit--state key)))
    (and (bufferp buffer) (buffer-live-p buffer) buffer)))

(defun sthenno/hermit--plain (text)
  "Return TEXT as a trimmed single-line plain string."
  (when (stringp text)
    (string-trim
     (replace-regexp-in-string "[ \t\n\r\f\v]+" " "
                               (substring-no-properties text)))))

(defun sthenno/hermit--blank-string-p (text)
  "Return non-nil when TEXT is nil or blank."
  (or (not (stringp text)) (string-match-p "\\`[ \t\n\r\f\v]*\\'" text)))

(defun sthenno/hermit--wrap (text columns rows)
  "Wrap TEXT to at most COLUMNS display columns and ROWS rows."
  (let ((columns (sthenno/hermit--n columns 4 4))
        (rows (sthenno/hermit--n rows 1 1)))
    (named-let collect ((rest (or (sthenno/hermit--plain text) ""))
                        (lines nil))
      (if (or (>= (length lines) rows)
              (sthenno/hermit--blank-string-p rest))
          (let ((lines (nreverse lines)))
            (when (and lines (not (sthenno/hermit--blank-string-p rest)))
              (setcar (last lines)
                      (concat (truncate-string-to-width
                               (car (last lines)) (max 1 (1- columns)) nil nil "")
                              "...")))
            lines)
        (let ((line (truncate-string-to-width rest columns nil nil "")))
          (when (sthenno/hermit--blank-string-p line)
            (setq line (substring rest 0 1)))
          (collect (replace-regexp-in-string "\\` +" ""
                                             (substring rest (length line)))
                   (cons line lines)))))))

(defun sthenno/hermit--width (strings minimum)
  "Return widest display width of STRINGS, no smaller than MINIMUM."
  (cl-loop for string in strings
           maximize (string-width string) into width
           finally return (max minimum (or width 0))))

(defun sthenno/hermit--bubble-render-lines (lines columns)
  "Return padded bubble LINES for COLUMNS."
  (let* ((pad-x (sthenno/hermit--n sthenno/hermit-bubble-padding-columns 0))
         (pad-y (sthenno/hermit--n sthenno/hermit-bubble-padding-lines 0))
         (inner (max (sthenno/hermit--n columns 1 1)
                     (sthenno/hermit--n sthenno/hermit-bubble-min-columns 1 10)
                     (sthenno/hermit--width lines 1)))
         (blank (make-string (+ inner (* 2 pad-x)) ?\s))
         (left (make-string pad-x ?\s)))
    (append (make-list pad-y blank)
            (mapcar (lambda (line)
                      (concat left line
                              (make-string (max 0 (- inner (string-width line))) ?\s)
                              left))
                    lines)
            (make-list pad-y blank))))

(defun sthenno/hermit--bubble-layout (text parent)
  "Return child-frame layout plist for bubble TEXT under PARENT."
  (when-let* ((lines (sthenno/hermit--wrap
                      text
                      (sthenno/hermit--n sthenno/hermit-bubble-max-columns 4 50)
                      (sthenno/hermit--n sthenno/hermit-bubble-max-lines 1 3))))
    (let* ((min-cols (sthenno/hermit--n sthenno/hermit-bubble-min-columns 1 10))
           (render-lines
            (sthenno/hermit--bubble-render-lines lines
                                                 (sthenno/hermit--width lines min-cols)))
           (content (cons
                     (* (sthenno/hermit--width render-lines 1)
                        (max 1 (frame-char-width parent)))
                     (* (length render-lines) (max 1 (frame-char-height parent)))))
           (border (sthenno/hermit--n sthenno/hermit-bubble-border-width 0))
           (outer (cons (+ (car content) (* 2 border))
                        (+ (cdr content) (* 2 border)))))
      `(:plain-lines ,lines :render-lines ,render-lines
                     :content-size ,content :outer-size ,outer))))

(defun sthenno/hermit--pet-position (placement parent-size pet-size gap)
  "Return pet position for PLACEMENT."
  (pcase-let
      ((`(,pw . ,ph) parent-size)
       (`(,w . ,h) pet-size)
       (gap (sthenno/hermit--n gap 0 0)))
    (pcase placement
      ('outside-right-bottom (cons (+ pw gap) (max 0 (- ph h gap))))
      ('outside-left-bottom (cons (- 0 w gap) (max 0 (- ph h gap))))
      ('center (cons (max 0 (/ (- pw w) 2)) (max 0 (/ (- ph h) 2))))
      (_ (cons (max 0 (- pw w gap)) (max 0 (- ph h gap)))))))

(defun sthenno/hermit--raw-image-size (file)
  "Return raw image size for FILE in pixels."
  (pcase-let ((`(,w . ,h) (image-size (create-image file nil nil) t)))
    (cons (round w) (round h))))

(defun sthenno/hermit--scale-image-size (raw height &optional file)
  "Scale RAW image size to HEIGHT."
  (pcase-let ((`(,w . ,h) raw)
              (height (sthenno/hermit--n height 1 420)))
    (if (<= h 0)
        (error "Invalid image height%s" (if file (format ": %s" file) ""))
      (cons (round (* height (/ (float w) h))) height))))

(defun sthenno/hermit--parent-frame ()
  "Return Hermit's parent frame."
  (sthenno/hermit--state :parent-frame))

(defun sthenno/hermit--color (face attr fallback &optional frame)
  "Return FACE ATTR, or FALLBACK."
  (let ((value (face-attribute face attr frame t)))
    (if (and (stringp value) (> (length value) 0)) value fallback)))

(defun sthenno/hermit--frame-params (parent name size &optional bubble)
  "Return child-frame parameters for NAME, SIZE and optional BUBBLE."
  (pcase-let ((`(,w . ,h) (cons (sthenno/hermit--n (car size) 1 1)
                                (sthenno/hermit--n (cdr size) 1 1))))
    `((name . ,name) (title . ,name) (parent-frame . ,parent)
      (delete-before . ,parent) (mouse-wheel-frame . ,parent)
      (no-other-frame . t) (minibuffer)
      (visibility) (undecorated-round . t) (skip-taskbar . t)
      (no-focus-on-map . t)
      (no-accept-focus . ,(if sthenno/hermit-no-accept-focus t nil))
      (z-group . above) (left + 0) (top + 0)
      (width text-pixels . ,w) (height text-pixels . ,h)
      (min-width . 0) (min-height . 0) (border-width . 0)
      (left-fringe . 0) (right-fringe . 0)
      (vertical-scroll-bars) (horizontal-scroll-bars)
      (tool-bar-lines . 0) (tab-bar-lines . 0)
      (line-spacing . 0) (cursor-type) (no-special-glyphs . t)
      (font . ,(frame-parameter parent 'font))
      (foreground-color
       . ,(sthenno/hermit--color (if bubble 'sthenno/hermit-bubble-face 'default)
                                 :foreground "#303030" parent))
      (background-color
       . ,(sthenno/hermit--color (if bubble 'sthenno/hermit-bubble-face 'default)
                                 :background "#fff7fb" parent))
      (alpha-background
       . ,(if bubble 100
            (sthenno/hermit--n sthenno/hermit-alpha-background 0 0 100)))
      (internal-border-width . 0)
      (child-frame-border-width
       . ,(if bubble (sthenno/hermit--n sthenno/hermit-bubble-border-width 0) 0))
      ,@(when bubble
          `((border-color
             . ,(sthenno/hermit--color
                 'sthenno/hermit-bubble-border-face :background "#ff9ec4" parent)))))))

(defun sthenno/hermit--pet-frame-parameters (parent size)
  "Return pet frame parameters."
  (sthenno/hermit--frame-params parent "sthenno/hermit" size))

(defun sthenno/hermit--bubble-frame-parameters (parent size)
  "Return bubble frame parameters."
  (sthenno/hermit--frame-params parent "sthenno/hermit-bubble" size t))

(defun sthenno/hermit--display-frame-inert (frame)
  "Show FRAME without accepting focus."
  (when frame
    (modify-frame-parameters frame '((no-focus-on-map . t) (no-accept-focus . t)))
    (make-frame-visible frame)
    frame))

;;; Rendering

(defvar sthenno/hermit-image-mode-map
  (sthenno/hermit--keymap
   ([mouse-1] . sthenno/hermit-click)
   ([drag-mouse-1] . sthenno/hermit-drag)
   ([S-down-mouse-1] . sthenno/hermit-dnd-image)
   ([mouse-2] . sthenno/hermit-iconify-parent)
   ([mouse-3] . sthenno/hermit-menu)
   ("q" . sthenno/hermit-stop)))

(defvar sthenno/hermit-bubble-mode-map
  (sthenno/hermit--keymap
   ([mouse-1] . sthenno/hermit-clear-bubble)
   ([drag-mouse-1] . sthenno/hermit-drag)
   ([S-down-mouse-1] . sthenno/hermit-dnd-bubble)
   ([mouse-2] . sthenno/hermit-iconify-parent)
   ([mouse-3] . sthenno/hermit-menu)
   ("q" . sthenno/hermit-stop)))

(defun sthenno/hermit--buffer (key name map)
  "Return live buffer stored at KEY, creating NAME with MAP when needed."
  (or (sthenno/hermit--live-buffer key)
      (let ((buffer (generate-new-buffer name)))
        (with-current-buffer buffer
          (special-mode)
          (use-local-map map)
          (dolist (cell '((cursor-type) (cursor-in-non-selected-windows)
                          (mode-line-format) (header-line-format)
                          (tab-line-format) (truncate-lines . t)
                          (line-spacing . 0) (display-line-numbers)
                          (left-margin-width . 0) (right-margin-width . 0)
                          (buffer-read-only . t)))
            (set (make-local-variable (car cell)) (cdr cell))))
        (setf (sthenno/hermit--state key) buffer)
        buffer)))

(defun sthenno/hermit--paint (frame key name map body)
  "Paint FRAME using buffer KEY/NAME/MAP and BODY."
  (let ((buffer (sthenno/hermit--buffer key name map))
        (window (frame-root-window frame)))
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    (set-window-fringes window 0 0)
    (set-window-margins window 0 0)
    (dolist (parameter '(mode-line-format header-line-format tab-line-format))
      (set-window-parameter window parameter 'none))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall body)
        (goto-char (point-min))))))

(defun sthenno/hermit--render-pet ()
  "Render Hermit's image."
  (when-let* ((frame (sthenno/hermit--live-frame :pet-frame))
              (file (sthenno/hermit--state :file))
              (size (sthenno/hermit--state :pet-size)))
    (sthenno/hermit--paint
     frame :image-buffer " *sthenno/hermit-image*"
     sthenno/hermit-image-mode-map
     (lambda ()
       (pcase-let ((`(,w . ,h) size))
         (insert-image
          (apply #'create-image file nil nil
                 `(:width ,w :height ,h :ascent 100
                          ,@(when sthenno/hermit-image-mask
                              `(:mask ,sthenno/hermit-image-mask))))))))
    (modify-frame-parameters
     frame (sthenno/hermit--pet-frame-parameters
            (sthenno/hermit--parent-frame) size))
    (pcase-let ((`(,w . ,h) size))
      (set-frame-size frame w h t))))

(defun sthenno/hermit--render-bubble-now (text)
  "Render bubble TEXT immediately."
  (if-let* ((parent (sthenno/hermit--parent-frame))
            ((frame-live-p parent))
            (layout (sthenno/hermit--bubble-layout text parent)))
      (let* ((content (plist-get layout :content-size))
             (frame (or (sthenno/hermit--live-frame :bubble-frame)
                        (let ((new-frame
                               (make-frame
                                (sthenno/hermit--bubble-frame-parameters parent
                                                                         content))))
                          (setf (sthenno/hermit--state :bubble-frame) new-frame)
                          new-frame)))
             (face (if sthenno/hermit-bubble-use-text-box
                       'sthenno/hermit-bubble-box-face
                     'sthenno/hermit-bubble-face)))
        (sthenno/hermit--with-slots
         (:bubble-content-size content
                               :bubble-outer-size (plist-get layout :outer-size))
         (sthenno/hermit--paint
          frame :bubble-buffer " *sthenno/hermit-bubble*"
          sthenno/hermit-bubble-mode-map
          (lambda ()
            (insert (propertize
                     (mapconcat #'identity
                                (plist-get layout :render-lines) "\n")
                     'face face)))))
        (modify-frame-parameters frame
                                 (sthenno/hermit--bubble-frame-parameters parent content))
        (pcase-let ((`(,w . ,h) content))
          (set-frame-size frame w h t))
        (sthenno/hermit--place-bubble)
        (sthenno/hermit--display-frame-inert frame))
    (sthenno/hermit-clear-bubble)))

(defun sthenno/hermit--render-pending-bubble ()
  "Render the pending bubble text."
  (when sthenno/hermit--state
    (setf (sthenno/hermit--state :bubble-render-timer) nil)
    (when (sthenno/hermit--state :bubble-visible)
      (sthenno/hermit--render-bubble-now (sthenno/hermit--state :bubble)))))

(defun sthenno/hermit--schedule-bubble-render ()
  "Schedule a coalesced bubble render."
  (sthenno/hermit--cancel-timer :bubble-render-timer)
  (let ((delay (and (numberp sthenno/hermit-bubble-render-delay)
                    (> sthenno/hermit-bubble-render-delay 0)
                    sthenno/hermit-bubble-render-delay)))
    (if delay
        (setf (sthenno/hermit--state :bubble-render-timer)
              (run-at-time delay nil #'sthenno/hermit--render-pending-bubble))
      (sthenno/hermit--render-pending-bubble))))

(defun sthenno/hermit--frame-native-size (frame)
  "Return FRAME native pixel size."
  (pcase-let ((`(,left ,top ,right ,bottom) (frame-edges frame 'native-edges)))
    (cons (- right left) (- bottom top))))

(defun sthenno/hermit--move-frame (frame pos)
  "Move FRAME to POS."
  (when (and frame pos)
    (modify-frame-parameters frame `((left + ,(car pos)) (top + ,(cdr pos))))))

(defun sthenno/hermit--place-bubble ()
  "Place the bubble above Hermit."
  (when-let* ((bubble (sthenno/hermit--live-frame :bubble-frame))
              ((sthenno/hermit--state :bubble-visible))
              (pet-pos (sthenno/hermit--state :pet-pos))
              (pet-size (sthenno/hermit--state :pet-size))
              (bubble-size (sthenno/hermit--state :bubble-outer-size)))
    (sthenno/hermit--move-frame
     bubble
     (cons (round (+ (car pet-pos) (/ (- (car pet-size) (car bubble-size)) 2)))
           (round (- (cdr pet-pos) (cdr bubble-size)
                     (sthenno/hermit--n sthenno/hermit-gap 0)))))))

(defun sthenno/hermit--refresh-placement ()
  "Refresh Hermit's pet and bubble placement."
  (when-let* ((pet (sthenno/hermit--live-frame :pet-frame))
              (parent (sthenno/hermit--parent-frame))
              ((frame-live-p parent))
              (size (sthenno/hermit--state :pet-size)))
    (unless (sthenno/hermit--state :manual-position)
      (setf (sthenno/hermit--state :pet-pos)
            (sthenno/hermit--pet-position
             sthenno/hermit-placement
             (sthenno/hermit--frame-native-size parent)
             size sthenno/hermit-gap)))
    (sthenno/hermit--move-frame pet (sthenno/hermit--state :pet-pos))
    (sthenno/hermit--place-bubble)))

(defun sthenno/hermit--cancel-timer (key)
  "Cancel timer stored at KEY."
  (when-let* ((timer (sthenno/hermit--state key))
              ((timerp timer)))
    (cancel-timer timer))
  (when sthenno/hermit--state
    (setf (sthenno/hermit--state key) nil)))

;;; Speech bubbles and hooks

(defun sthenno/hermit-clear-bubble ()
  "Hide the Hermit bubble."
  (interactive)
  (when sthenno/hermit--state
    (sthenno/hermit--cancel-timer :bubble-timer)
    (sthenno/hermit--cancel-timer :bubble-render-timer)
    (sthenno/hermit--with-slots
     (:queued-bubble nil :bubble nil :bubble-visible nil
                     :bubble-content-size nil :bubble-outer-size nil)
     nil)
    (sthenno/hermit--with-live-frame (frame :bubble-frame)
      (make-frame-invisible frame t))))

(defun sthenno/hermit-say (text)
  "Show TEXT in Hermit's bubble."
  (interactive "sHermit says: ")
  (let ((plain (sthenno/hermit--plain text)))
    (cond
     ((sthenno/hermit--blank-string-p plain)
      (sthenno/hermit-clear-bubble))
     ((not sthenno/hermit--state) nil)
     ((eq (sthenno/hermit--state :status) 'probing)
      (setf (sthenno/hermit--state :queued-bubble) plain))
     (t
      (sthenno/hermit--cancel-timer :bubble-timer)
      (sthenno/hermit--set-state :bubble plain :bubble-visible t)
      (sthenno/hermit--schedule-bubble-render)
      (when-let* ((timeout sthenno/hermit-bubble-timeout)
                  ((numberp timeout))
                  ((> timeout 0)))
        (setf (sthenno/hermit--state :bubble-timer)
              (run-at-time timeout nil #'sthenno/hermit-clear-bubble)))))))

(defun sthenno/hermit--flush-queued-bubble ()
  "Show a bubble queued while Hermit was probing."
  (when-let* ((queued (sthenno/hermit--state :queued-bubble))
              ((eq (sthenno/hermit--state :status) 'ready)))
    (setf (sthenno/hermit--state :queued-bubble) nil)
    (sthenno/hermit-say queued)))

(defun sthenno/hermit--flush-message ()
  "Flush pending echo-area message to Hermit."
  (when sthenno/hermit--state
    (let ((plain (sthenno/hermit--state :pending-message)))
      (sthenno/hermit--set-state :message-timer nil :pending-message nil)
      (when (and sthenno/hermit-mode
                 (eq (sthenno/hermit--state :status) 'ready)
                 (sthenno/hermit--live-frame :pet-frame)
                 (not (sthenno/hermit--blank-string-p plain)))
        (sthenno/hermit-say plain)))))

(defun sthenno/hermit--message-filter (text)
  "Capture echo-area TEXT for Hermit."
  (when (and sthenno/hermit-mode
             (eq (sthenno/hermit--state :status) 'ready)
             (sthenno/hermit--live-frame :pet-frame)
             (stringp text))
    (let ((plain (sthenno/hermit--plain text)))
      (unless (sthenno/hermit--blank-string-p plain)
        (setf (sthenno/hermit--state :pending-message) plain)
        (sthenno/hermit--cancel-timer :message-timer)
        (setf (sthenno/hermit--state :message-timer)
              (run-with-idle-timer
               (if (and (numberp sthenno/hermit-message-delay)
                        (> sthenno/hermit-message-delay 0))
                   sthenno/hermit-message-delay
                 0.0125)
               nil #'sthenno/hermit--flush-message)))))
  nil)

(defun sthenno/hermit--hooks (action)
  "Install or remove Hermit hooks according to ACTION."
  (let ((op (if action #'add-hook #'remove-hook)))
    (when (boundp 'set-message-functions)
      (funcall op 'set-message-functions #'sthenno/hermit--message-filter))
    (dolist (hook '((window-size-change-functions . sthenno/hermit--on-window-size-change)
                    (delete-frame-functions . sthenno/hermit--on-delete-frame)))
      (funcall op (car hook) (cdr hook)))))

(defun sthenno/hermit--on-window-size-change (frame)
  "Reposition Hermit after FRAME changes size."
  (when (and sthenno/hermit-mode (eq frame (sthenno/hermit--parent-frame)))
    (sthenno/hermit--refresh-placement)))

(defun sthenno/hermit--on-delete-frame (frame)
  "Disable Hermit when its parent FRAME is deleted."
  (when (and sthenno/hermit-mode (eq frame (sthenno/hermit--parent-frame)))
    (sthenno/hermit-mode -1)))

;;; Interaction

(defun sthenno/hermit--event-frame (event)
  "Return frame associated with EVENT."
  (let ((where (posn-window (event-start event))))
    (cond ((windowp where) (window-frame where))
          ((framep where) where))))

(defun sthenno/hermit--posn-xy (posn)
  "Return POSN x/y relative to Hermit's parent child-frame space."
  (let* ((where (posn-window posn))
         (frame (cond ((windowp where) (window-frame where))
                      ((framep where) where)))
         (xy (posn-x-y posn)))
    (when (and frame xy)
      (pcase-let ((`(,x . ,y)
                   (if (frame-parent frame) (frame-position frame) '(0 . 0))))
        (cons (+ x (car xy)) (+ y (cdr xy)))))))

(defun sthenno/hermit--event-delta (event)
  "Return drag delta for EVENT."
  (when-let* ((start (sthenno/hermit--posn-xy (event-start event)))
              (end (sthenno/hermit--posn-xy (event-end event))))
    (cons (- (car end) (car start)) (- (cdr end) (cdr start)))))

(defun sthenno/hermit--begin-drag-file (frame file)
  "Begin dragging FILE from FRAME."
  (when (fboundp 'dnd-begin-file-drag)
    (dnd-begin-file-drag file frame 'copy t)))

(defun sthenno/hermit--begin-drag-text (frame text)
  "Begin dragging TEXT from FRAME."
  (when (fboundp 'dnd-begin-text-drag)
    (dnd-begin-text-drag text frame 'copy t)))

(defun sthenno/hermit--restore-focus ()
  "Restore focus to Hermit's return or parent frame."
  (when-let* ((frame (or (sthenno/hermit--state :return-frame)
                         (sthenno/hermit--parent-frame)))
              ((framep frame))
              ((frame-live-p frame)))
    (select-frame-set-input-focus frame)))

(defun sthenno/hermit-drag (event)
  "Move Hermit with mouse drag."
  (interactive "e")
  (when-let* ((pet (sthenno/hermit--live-frame :pet-frame))
              (delta (sthenno/hermit--event-delta event)))
    (pcase-let ((`(,x . ,y) (or (sthenno/hermit--state :pet-pos)
                                (frame-position pet))))
      (sthenno/hermit--set-state
       :pet-pos (cons (+ x (car delta)) (+ y (cdr delta)))
       :manual-position t)
      (sthenno/hermit--move-frame pet (sthenno/hermit--state :pet-pos))
      (sthenno/hermit--place-bubble)))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-dnd-image (event)
  "Drag Hermit's image file."
  (interactive "e")
  (when-let* ((sthenno/hermit-enable-dnd)
              (file (sthenno/hermit--state :file))
              (frame (sthenno/hermit--event-frame event)))
    (sthenno/hermit--begin-drag-file frame file))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-dnd-bubble (event)
  "Drag Hermit's bubble text."
  (interactive "e")
  (let ((text (sthenno/hermit--state :bubble)))
    (when-let* ((sthenno/hermit-enable-dnd)
                ((not (sthenno/hermit--blank-string-p text)))
                (frame (sthenno/hermit--event-frame event)))
      (sthenno/hermit--begin-drag-text frame text)))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-click (_event)
  "Toggle Hermit's bubble."
  (interactive "e")
  (if (sthenno/hermit--state :bubble-visible)
      (sthenno/hermit-clear-bubble)
    (sthenno/hermit-say (or (current-message) "...")))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-place ()
  "Reset Hermit to `sthenno/hermit-placement'."
  (interactive)
  (when sthenno/hermit--state
    (setf (sthenno/hermit--state :manual-position) nil)
    (sthenno/hermit--refresh-placement))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit--dispatch-menu-choice (choice)
  "Dispatch popup menu CHOICE."
  (pcase choice
    ('say (sthenno/hermit-say (or (current-message) "...")))
    ('clear (sthenno/hermit-clear-bubble))
    ('place (sthenno/hermit-place))
    ('hide (sthenno/hermit-hide))
    ('show (sthenno/hermit-show))
    ('iconify (sthenno/hermit-iconify-parent))
    ('restart (sthenno/hermit-restart))
    ('stop (sthenno/hermit-stop)))
  (and choice (not (eq choice 'iconify))))

(defun sthenno/hermit-menu (event)
  "Right-click menu for Hermit."
  (interactive "e")
  (when (sthenno/hermit--dispatch-menu-choice
         (x-popup-menu event
                       '("Emacs Hermit"
                         ("Emacs Hermit"
                          ("Say current echo" . say)
                          ("Clear bubble" . clear)
                          ("Reset position" . place)
                          ("Hide" . hide)
                          ("Show" . show)
                          ("Iconify parent" . iconify)
                          ("Restart" . restart)
                          ("Stop" . stop)))))
    (sthenno/hermit--restore-focus)))

(defun sthenno/hermit--root-frame (frame)
  "Return the root parent frame for FRAME."
  (named-let walk ((candidate frame))
    (if-let* ((parent (frame-parent candidate)))
        (walk parent)
      candidate)))

;;; Lifecycle

(defun sthenno/hermit--teardown-controller ()
  "Delete Hermit's frames, buffers and timers."
  (mapc #'sthenno/hermit--cancel-timer
        '(:bubble-timer :bubble-render-timer :message-timer))
  (dolist (key '(:bubble-frame :pet-frame))
    (sthenno/hermit--with-live-frame (frame key)
      (delete-frame frame t))
    (setf (sthenno/hermit--state key) nil))
  (dolist (key '(:bubble-buffer :image-buffer))
    (sthenno/hermit--with-live-buffer (buffer key)
      (kill-buffer buffer))
    (setf (sthenno/hermit--state key) nil))
  (setq sthenno/hermit--state nil))

(defun sthenno/hermit--disable ()
  "Disable Hermit internals."
  (sthenno/hermit--hooks nil)
  (sthenno/hermit--teardown-controller)
  (setq sthenno/hermit--request nil))

(defun sthenno/hermit--enable (&optional file parent)
  "Enable Hermit for image FILE under PARENT frame."
  (sthenno/hermit--disable)
  (let* ((file (expand-file-name (or file sthenno/hermit-image-file)))
         (parent (or parent (selected-frame))))
    (unless (display-graphic-p parent)
      (user-error "sthenno/hermit requires a graphical frame"))
    (unless (file-readable-p file)
      (user-error "Cannot read image file: %s" file))
    (when (fboundp 'clear-image-cache)
      (clear-image-cache file))
    (setq sthenno/hermit--state
          (list :status 'probing
                :parent-frame parent
                :return-frame (selected-frame)
                :file file))
    (let ((size (sthenno/hermit--scale-image-size
                 (sthenno/hermit--raw-image-size file)
                 (sthenno/hermit--n sthenno/hermit-image-height 1 420)
                 file)))
      (sthenno/hermit--set-state
       :status 'ready
       :pet-size size
       :pet-frame (make-frame (sthenno/hermit--pet-frame-parameters parent size)))
      (sthenno/hermit--render-pet)
      (sthenno/hermit--refresh-placement)
      (sthenno/hermit--display-frame-inert (sthenno/hermit--state :pet-frame))
      (sthenno/hermit--hooks t)
      (sthenno/hermit--flush-queued-bubble))))

;;;###autoload
(define-minor-mode sthenno/hermit-mode
  "Toggle Hermit Pet mode."
  :group 'sthenno/hermit
  (if sthenno/hermit-mode
      (condition-case err
          (sthenno/hermit--enable (plist-get sthenno/hermit--request :file)
                                  (plist-get sthenno/hermit--request :parent))
        (error
         (setq sthenno/hermit-mode nil)
         (sthenno/hermit--disable)
         (signal (car err) (cdr err))))
    (sthenno/hermit--disable))
  (setq sthenno/hermit--request nil))

;;;###autoload
(defun sthenno/hermit-start (&optional file parent)
  "Enable `sthenno/hermit-mode'."
  (interactive)
  (setq sthenno/hermit--request (list :file file :parent parent))
  (sthenno/hermit-mode 1))

;;;###autoload
(defun sthenno/hermit-stop ()
  "Disable `sthenno/hermit-mode'."
  (interactive)
  (sthenno/hermit-mode -1))

;;;###autoload
(defun sthenno/hermit-restart ()
  "Restart `sthenno/hermit-mode'."
  (interactive)
  (let ((file (sthenno/hermit--state :file))
        (parent (sthenno/hermit--parent-frame)))
    (sthenno/hermit-mode -1)
    (setq sthenno/hermit--request (list :file file :parent parent))
    (sthenno/hermit-mode 1)))

;;;###autoload
(defun sthenno/hermit-hide ()
  "Hide Hermit without disabling `sthenno/hermit-mode'."
  (interactive)
  (dolist (key '(:bubble-frame :pet-frame))
    (sthenno/hermit--with-live-frame (frame key)
      (make-frame-invisible frame t))))

;;;###autoload
(defun sthenno/hermit-show ()
  "Show Hermit without raising or selecting it."
  (interactive)
  (sthenno/hermit--with-live-frame (pet :pet-frame)
    (sthenno/hermit--display-frame-inert pet)
    (sthenno/hermit--refresh-placement))
  (when (sthenno/hermit--state :bubble-visible)
    (sthenno/hermit--with-live-frame (bubble :bubble-frame)
      (sthenno/hermit--display-frame-inert bubble)
      (sthenno/hermit--place-bubble))))

;;;###autoload
(defun sthenno/hermit-iconify-parent ()
  "Iconify Hermit root parent frame."
  (interactive)
  (when-let* ((parent (sthenno/hermit--parent-frame))
              (root (sthenno/hermit--root-frame parent))
              ((framep root))
              ((frame-live-p root)))
    (iconify-frame root)))

(provide 'sthenno-hermit)

;;; sthenno-hermit.el ends here
