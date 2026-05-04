;;; init-gpt.el --- AI assistance -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the interactive AI assistant configuration.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gv)
  (require 'peg)
  (require 'subr-x))
(require 'cl-lib)
(require 'gv)
(require 'peg)
(require 'subr-x)

;;; LLM protocol
(use-package gptel
  :ensure t
  :config (setopt gptel-default-mode #'org-mode
                  gptel-org-branching-context t
                  gptel-model 'sthenno
                  gptel-backend (gptel-make-openai "local"
                                  :protocol "http"
                                  :host "192.168.100.207:8000"
                                  :endpoint "/v1/chat/completions"
                                  :stream t
                                  :key "sk-tmp"
                                  :models '(sthenno)))
  :bind ((:map global-map
               ("s-p" . gptel))
         (:map gptel-mode-map
               ("s-<return>" . gptel-send))))


;;; Options

(defgroup sthenno/hermit nil
  "A small image pet attached to an Emacs frame."
  :group 'applications)

(defcustom sthenno/hermit-image-file (locate-user-emacs-file "resources/hmt-01.png")
  "PNG file used as the pet image."
  :type 'file
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-image-height 550
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

(defcustom sthenno/hermit-bubble-timeout 4.5
  "Seconds before the speech bubble is hidden."
  :type '(choice number (const nil))
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-message-delay 0.025
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

(cl-defstruct (sthenno/hermit-options
               (:constructor sthenno/hermit--make-options
                             (&key image-file image-height image-mask placement gap
                                   alpha-background no-accept-focus
                                   bubble-timeout message-delay bubble-max-columns
                                   bubble-min-columns
                                   bubble-max-lines bubble-padding-columns
                                   bubble-padding-lines
                                   bubble-border-width bubble-use-text-box enable-dnd))
               (:conc-name sthenno/hermit-options-)
               (:copier nil))
  (image-file sthenno/hermit-image-file)
  (image-height sthenno/hermit-image-height)
  (image-mask sthenno/hermit-image-mask)
  (placement sthenno/hermit-placement)
  (gap sthenno/hermit-gap)
  (alpha-background sthenno/hermit-alpha-background)
  (no-accept-focus sthenno/hermit-no-accept-focus)
  (bubble-timeout sthenno/hermit-bubble-timeout)
  (message-delay sthenno/hermit-message-delay)
  (bubble-max-columns sthenno/hermit-bubble-max-columns)
  (bubble-min-columns sthenno/hermit-bubble-min-columns)
  (bubble-max-lines sthenno/hermit-bubble-max-lines)
  (bubble-padding-columns sthenno/hermit-bubble-padding-columns)
  (bubble-padding-lines sthenno/hermit-bubble-padding-lines)
  (bubble-border-width sthenno/hermit-bubble-border-width)
  (bubble-use-text-box sthenno/hermit-bubble-use-text-box)
  (enable-dnd sthenno/hermit-enable-dnd))

(defvar sthenno/hermit-options (sthenno/hermit--make-options)
  "Personal Hermit options struct.")

(defconst sthenno/hermit--opt-vars
  '((:image-file . sthenno/hermit-image-file)
    (:image-height . sthenno/hermit-image-height)
    (:image-mask . sthenno/hermit-image-mask)
    (:placement . sthenno/hermit-placement)
    (:gap . sthenno/hermit-gap)
    (:alpha-background . sthenno/hermit-alpha-background)
    (:no-accept-focus . sthenno/hermit-no-accept-focus)
    (:bubble-timeout . sthenno/hermit-bubble-timeout)
    (:message-delay . sthenno/hermit-message-delay)
    (:bubble-max-columns . sthenno/hermit-bubble-max-columns)
    (:bubble-min-columns . sthenno/hermit-bubble-min-columns)
    (:bubble-max-lines . sthenno/hermit-bubble-max-lines)
    (:bubble-padding-columns . sthenno/hermit-bubble-padding-columns)
    (:bubble-padding-lines . sthenno/hermit-bubble-padding-lines)
    (:bubble-border-width . sthenno/hermit-bubble-border-width)
    (:bubble-use-text-box . sthenno/hermit-bubble-use-text-box)
    (:enable-dnd . sthenno/hermit-enable-dnd))
  "Keyword to public customization variable table for Hermit options.")

(cl-defmacro sthenno/hermit--define-access-tables
    (getter-table setter-table object getter-doc setter-doc &rest specs)
  "Define GETTER-TABLE and SETTER-TABLE for OBJECT slot SPECS."
  `(progn
     (defconst ,getter-table
       ',(mapcar (lambda (spec) (cons (car spec) (cadr spec))) specs)
       ,getter-doc)
     (defconst ,setter-table
       (list
        ,@(mapcar
           (lambda (spec)
             (let ((key (car spec))
                   (accessor (cadr spec)))
               `(cons ',key
                      (lambda (value)
                        (setf (,accessor ,object) value)))))
           specs))
       ,setter-doc)))

(sthenno/hermit--define-access-tables
 sthenno/hermit--opt-getters
 sthenno/hermit--opt-setters
 sthenno/hermit-options
 "Keyword to accessor table for Hermit options."
 "Keyword to setter table for Hermit options."
 (:image-file sthenno/hermit-options-image-file)
 (:image-height sthenno/hermit-options-image-height)
 (:image-mask sthenno/hermit-options-image-mask)
 (:placement sthenno/hermit-options-placement)
 (:gap sthenno/hermit-options-gap)
 (:alpha-background sthenno/hermit-options-alpha-background)
 (:no-accept-focus sthenno/hermit-options-no-accept-focus)
 (:bubble-timeout sthenno/hermit-options-bubble-timeout)
 (:message-delay sthenno/hermit-options-message-delay)
 (:bubble-max-columns sthenno/hermit-options-bubble-max-columns)
 (:bubble-min-columns sthenno/hermit-options-bubble-min-columns)
 (:bubble-max-lines sthenno/hermit-options-bubble-max-lines)
 (:bubble-padding-columns sthenno/hermit-options-bubble-padding-columns)
 (:bubble-padding-lines sthenno/hermit-options-bubble-padding-lines)
 (:bubble-border-width sthenno/hermit-options-bubble-border-width)
 (:bubble-use-text-box sthenno/hermit-options-bubble-use-text-box)
 (:enable-dnd sthenno/hermit-options-enable-dnd))

(defun sthenno/hermit--opt (key)
  (pcase (assq key sthenno/hermit--opt-getters)
    (`(,_ . ,getter) (funcall getter sthenno/hermit-options))
    (_ (error "Unknown Hermit option key: %S" key))))

(defun sthenno/hermit--set-opt-1 (key value)
  (pcase (assq key sthenno/hermit--opt-setters)
    (`(,_ . ,setter)
     (funcall setter value)
     (when-let* ((var (alist-get key sthenno/hermit--opt-vars)))
       (set var value)))
    (_ (error "Unknown Hermit option key: %S" key)))
  value)

(gv-define-setter sthenno/hermit--opt (value key)
  `(sthenno/hermit--set-opt-1 ,key ,value))

(defun sthenno/hermit--sync-options ()
  "Refresh internal Hermit option storage from public defcustom variables."
  (dolist (cell sthenno/hermit--opt-vars sthenno/hermit-options)
    (setf (sthenno/hermit--opt (car cell)) (symbol-value (cdr cell)))))

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

(defvar sthenno/hermit-mode nil)
(defvar sthenno/hermit--controller nil)
(defvar sthenno/hermit--request nil)

(cl-defstruct (sthenno/hermit-command
               (:constructor sthenno/hermit--make-command
                             (&key action text placement option value shell-command))
               (:conc-name sthenno/hermit-command-)
               (:copier nil))
  action text placement option value shell-command)

(cl-defstruct (sthenno/hermit-process-record
               (:constructor sthenno/hermit--make-process-record
                             (&key process command output))
               (:conc-name sthenno/hermit-process-record-)
               (:copier nil))
  process command (output ""))

(cl-defstruct (sthenno/hermit-controller
               (:constructor sthenno/hermit--make-controller
                             (&key status parent-frame return-frame file pet-size
                                   pet-frame image-buffer
                                   pet-pos manual-position pending-message message-timer
                                   bubble-timer
                                   bubble-frame bubble bubble-visible
                                   bubble-content-size
                                   bubble-outer-size bubble-buffer queued-bubble
                                   process-record))
               (:conc-name sthenno/hermit-controller-)
               (:copier nil))
  status parent-frame return-frame file pet-size pet-frame image-buffer
  pet-pos manual-position pending-message message-timer bubble-timer bubble-frame
  bubble bubble-visible bubble-content-size bubble-outer-size bubble-buffer
  queued-bubble process-record)

(sthenno/hermit--define-access-tables
 sthenno/hermit--state-getters
 sthenno/hermit--state-setters
 sthenno/hermit--controller
 "Keyword to accessor table for Hermit controller state."
 "Keyword to setter table for Hermit controller state."
 (:status sthenno/hermit-controller-status)
 (:parent-frame sthenno/hermit-controller-parent-frame)
 (:return-frame sthenno/hermit-controller-return-frame)
 (:file sthenno/hermit-controller-file)
 (:pet-size sthenno/hermit-controller-pet-size)
 (:pet-frame sthenno/hermit-controller-pet-frame)
 (:image-buffer sthenno/hermit-controller-image-buffer)
 (:pet-pos sthenno/hermit-controller-pet-pos)
 (:manual-position sthenno/hermit-controller-manual-position)
 (:pending-message sthenno/hermit-controller-pending-message)
 (:message-timer sthenno/hermit-controller-message-timer)
 (:bubble-timer sthenno/hermit-controller-bubble-timer)
 (:bubble-frame sthenno/hermit-controller-bubble-frame)
 (:bubble sthenno/hermit-controller-bubble)
 (:bubble-visible sthenno/hermit-controller-bubble-visible)
 (:bubble-content-size sthenno/hermit-controller-bubble-content-size)
 (:bubble-outer-size sthenno/hermit-controller-bubble-outer-size)
 (:bubble-buffer sthenno/hermit-controller-bubble-buffer)
 (:queued-bubble sthenno/hermit-controller-queued-bubble)
 (:process-record sthenno/hermit-controller-process-record))

(defun sthenno/hermit--state (key)
  (when (sthenno/hermit-controller-p sthenno/hermit--controller)
    (pcase (assq key sthenno/hermit--state-getters)
      (`(,_ . ,getter) (funcall getter sthenno/hermit--controller))
      (_ (error "Unknown Hermit state key: %S" key)))))

(defun sthenno/hermit--set-state-1 (key value)
  (unless (sthenno/hermit-controller-p sthenno/hermit--controller)
    (setq sthenno/hermit--controller (sthenno/hermit--make-controller)))
  (pcase (assq key sthenno/hermit--state-setters)
    (`(,_ . ,setter) (funcall setter value))
    (_ (error "Unknown Hermit state key: %S" key)))
  value)

(gv-define-setter sthenno/hermit--state (value key)
  `(sthenno/hermit--set-state-1 ,key ,value))

(defun sthenno/hermit--set-state (&rest pairs)
  "Update the Hermit controller by KEY VALUE pairs."
  (unless (cl-evenp (length pairs))
    (error "sthenno/hermit--set-state expects even KEY VALUE pairs, got: %S" pairs))
  (cl-loop for (key value) on pairs by #'cddr
           do (setf (sthenno/hermit--state key) value))
  pairs)

(cl-defmacro sthenno/hermit--with-slots (pairs &body body)
  "Update state by a plist of KEY VALUE pairs, then evaluate BODY."
  `(progn
     (sthenno/hermit--set-state ,@pairs)
     ,@body))

(defmacro sthenno/hermit--keymap (&rest bindings)
  `(let ((map (make-sparse-keymap)))
     ,@(mapcar (lambda (binding)
                 `(define-key map ,(car binding) #',(cdr binding)))
               bindings)
     map))

(cl-defmacro sthenno/hermit--with-live ((var key predicate) &body body)
  (declare (indent 1))
  `(when-let* ((value (sthenno/hermit--state ,key))
               ((funcall ,predicate value)))
     (let ((,var value))
       ,@body)))

(cl-defmacro sthenno/hermit--with-live-frame ((var key) &body body)
  (declare (indent 1))
  `(sthenno/hermit--with-live (,var ,key #'frame-live-p) ,@body))

(cl-defmacro sthenno/hermit--with-live-buffer ((var key) &body body)
  (declare (indent 1))
  `(sthenno/hermit--with-live (,var ,key #'buffer-live-p) ,@body))


(defun sthenno/hermit--n (value minimum &optional fallback maximum)
  "Clamp VALUE to MINIMUM and optional MAXIMUM.
Invalid VALUE falls back to FALLBACK, or 0 when FALLBACK is not numeric."
  (let* ((value-number (and (numberp value) (truncate value)))
         (fallback-number (and (numberp fallback) (truncate fallback))))
    (if (null maximum)
        (max minimum (or value-number fallback-number 0))
      (min maximum (max minimum (or value-number fallback-number 0))))))

(defun sthenno/hermit--live-frame (key)
  (let ((frame (sthenno/hermit--state key)))
    (and (framep frame) (frame-live-p frame) frame)))

(defun sthenno/hermit--live-buffer (key)
  (let ((buffer (sthenno/hermit--state key)))
    (and (bufferp buffer) (buffer-live-p buffer) buffer)))

(defun sthenno/hermit--plain (text)
  (when (stringp text)
    (replace-regexp-in-string "\\` +\\| +\\'" ""
                              (replace-regexp-in-string "[ \t\n\r\f\v]+" " "
                                                        (substring-no-properties text)))))

(defun sthenno/hermit--blank-string-p (text)
  (or (not (stringp text)) (string-match-p "\\`[ \t\n\r\f\v]*\\'" text)))

(defun sthenno/hermit--wrap (text columns rows)
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
                              "…")))
            lines)
        (let ((line (truncate-string-to-width rest columns nil nil "")))
          (when (sthenno/hermit--blank-string-p line)
            (setq line (substring rest 0 1)))
          (collect (replace-regexp-in-string "\\` +" ""
                                             (substring rest (length line)))
                   (cons line lines)))))))

(defun sthenno/hermit--width (strings minimum)
  (cl-loop for string in strings
           maximize (string-width string) into width
           finally return (max minimum (or width 0))))

(defun sthenno/hermit--bubble-render-lines (lines columns)
  (let* ((pad-x (sthenno/hermit--n (sthenno/hermit--opt :bubble-padding-columns) 0))
         (pad-y (sthenno/hermit--n (sthenno/hermit--opt :bubble-padding-lines) 0))
         (inner (max (sthenno/hermit--n columns 1 1)
                     (sthenno/hermit--n (sthenno/hermit--opt :bubble-min-columns) 1 10)
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
  (when-let* ((lines (sthenno/hermit--wrap
                      text
                      (sthenno/hermit--n (sthenno/hermit--opt :bubble-max-columns) 4 50)
                      (sthenno/hermit--n (sthenno/hermit--opt :bubble-max-lines) 1 3))))
    (let* ((min-cols (sthenno/hermit--n (sthenno/hermit--opt :bubble-min-columns) 1 10))
           (render-lines
            (sthenno/hermit--bubble-render-lines lines
                                                 (sthenno/hermit--width lines min-cols)))
           (content (cons
                     (* (sthenno/hermit--width render-lines 1)
                        (max 1 (frame-char-width parent)))
                     (* (length render-lines) (max 1 (frame-char-height parent)))))
           (border (sthenno/hermit--n (sthenno/hermit--opt :bubble-border-width) 0))
           (outer (cons (+ (car content) (* 2 border)) (+ (cdr content) (* 2 border)))))
      `(:plain-lines ,lines :render-lines ,render-lines :content-size ,content :outer-size ,outer))))

(defun sthenno/hermit--pet-position (placement parent-size pet-size gap)
  (pcase-let
      ((`(,pw . ,ph) parent-size) (`(,w . ,h) pet-size)
       (gap (sthenno/hermit--n gap 0 0)))
    (pcase placement
      ('outside-right-bottom (cons (+ pw gap) (max 0 (- ph h gap))))
      ('outside-left-bottom (cons (- 0 w gap) (max 0 (- ph h gap))))
      ('center (cons (max 0 (/ (- pw w) 2)) (max 0 (/ (- ph h) 2))))
      (_ (cons (max 0 (- pw w gap)) (max 0 (- ph h gap)))))))

(defun sthenno/hermit--raw-image-size (file)
  (pcase-let ((`(,w . ,h) (image-size (create-image file nil nil) t)))
    (cons (round w) (round h))))

(defun sthenno/hermit--scale-image-size (raw height &optional file)
  (pcase-let ((`(,w . ,h) raw) (height (sthenno/hermit--n height 1 420)))
    (if (<= h 0)
        (error "Invalid image height%s" (if file (format ": %s" file) ""))
      (cons (round (* height (/ (float w) h))) height))))

(defun sthenno/hermit--parent-frame ()
  (sthenno/hermit--state :parent-frame))

(defun sthenno/hermit--color (face attr fallback &optional frame)
  (let ((value (face-attribute face attr frame t)))
    (if (and (stringp value) (> (length value) 0)) value fallback)))

(defun sthenno/hermit--frame-params (parent name size &optional bubble)
  (pcase-let ((`(,w . ,h) (cons (sthenno/hermit--n (car size) 1 1)
                                (sthenno/hermit--n (cdr size) 1 1))))
    `((name . ,name) (title . ,name) (parent-frame . ,parent)
      (delete-before . ,parent) (mouse-wheel-frame . ,parent) (no-other-frame . t)
      (minibuffer)
      (visibility) (undecorated-round . t) (skip-taskbar . t) (no-focus-on-map . t)
      (no-accept-focus . ,(if (sthenno/hermit--opt :no-accept-focus) t nil))
      (z-group . above) (left + 0) (top + 0) (width text-pixels . ,w)
      (height text-pixels . ,h)
      (min-width . 0) (min-height . 0) (border-width . 0) (left-fringe . 0)
      (right-fringe . 0)
      (vertical-scroll-bars) (horizontal-scroll-bars) (tool-bar-lines . 0)
      (tab-bar-lines . 0)
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
            (sthenno/hermit--n (sthenno/hermit--opt :alpha-background) 0 0 100)))
      (internal-border-width . 0)
      (child-frame-border-width
       . ,(if bubble (sthenno/hermit--n (sthenno/hermit--opt :bubble-border-width) 0) 0))
      ,@(when bubble `((border-color . ,(sthenno/hermit--color
                                         'sthenno/hermit-bubble-border-face :background "#ff9ec4" parent)))))))

(defun sthenno/hermit--common-frame-parameters (parent name size)
  (sthenno/hermit--frame-params parent name size))
(defun sthenno/hermit--pet-frame-parameters (parent size)
  (sthenno/hermit--frame-params parent "sthenno/hermit" size))
(defun sthenno/hermit--bubble-frame-parameters (parent size)
  (sthenno/hermit--frame-params parent "sthenno/hermit-bubble" size t))

(defun sthenno/hermit--display-frame-inert (frame)
  (when frame
    (modify-frame-parameters frame '((no-focus-on-map . t) (no-accept-focus . t)))
    (make-frame-visible frame)
    frame))

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
  (or (sthenno/hermit--live-buffer key)
      (let ((buffer (generate-new-buffer name)))
        (with-current-buffer buffer
          (special-mode) (use-local-map map)
          (dolist (cell '((cursor-type) (cursor-in-non-selected-windows)
                          (mode-line-format) (header-line-format)
                          (tab-line-format) (truncate-lines . t) (line-spacing . 0)
                          (display-line-numbers)
                          (left-margin-width . 0) (right-margin-width . 0)
                          (buffer-read-only . t)))
            (set (make-local-variable (car cell)) (cdr cell))))
        (setf (sthenno/hermit--state key) buffer)
        buffer)))

(defun sthenno/hermit--paint (frame key name map body)
  (let ((buffer (sthenno/hermit--buffer key name map))
        (window (frame-root-window frame)))
    (set-window-buffer window buffer) (set-window-dedicated-p window t)
    (set-window-fringes window 0 0) (set-window-margins window 0 0)
    (dolist (parameter '(mode-line-format header-line-format tab-line-format))
      (set-window-parameter window parameter 'none))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer) (funcall body) (goto-char (point-min))))))

(defun sthenno/hermit--render-pet ()
  (when-let* ((frame (sthenno/hermit--live-frame :pet-frame))
              (file (sthenno/hermit--state :file))
              (size (sthenno/hermit--state :pet-size)))
    (sthenno/hermit--paint
     frame :image-buffer " *sthenno/hermit-image*"
     sthenno/hermit-image-mode-map (lambda ()
                                     (pcase-let ((`(,w . ,h) size))
                                       (insert-image (apply #'create-image file nil nil
                                                            `( :width ,w :height ,h
                                                               :ascent 100
                                                               ,@(when (sthenno/hermit--opt :image-mask)
                                                                   `(:mask ,(sthenno/hermit--opt :image-mask)))))))))
    (modify-frame-parameters frame
                             (sthenno/hermit--pet-frame-parameters
                              (sthenno/hermit--parent-frame) size))
    (pcase-let ((`(,w . ,h) size)) (set-frame-size frame w h t))))

(defun sthenno/hermit--render-bubble (text)
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
             (face 'sthenno/hermit-bubble-face))
        (sthenno/hermit--with-slots
         ( :bubble text
           :bubble-visible t
           :bubble-content-size content
           :bubble-outer-size
           (plist-get layout :outer-size))
         (sthenno/hermit--paint frame :bubble-buffer
                                " *sthenno/hermit-bubble*"
                                sthenno/hermit-bubble-mode-map
                                (lambda ()
                                  (insert (propertize (mapconcat #'identity
                                                                 (plist-get layout
                                                                            :render-lines)
                                                                 "\n")
                                                      'face
                                                      face)))))
        (modify-frame-parameters frame
                                 (sthenno/hermit--bubble-frame-parameters parent content))
        (pcase-let ((`(,w . ,h) content)) (set-frame-size frame w h t))
        (sthenno/hermit--place-bubble) (sthenno/hermit--display-frame-inert frame))
    (sthenno/hermit-clear-bubble)))

(defun sthenno/hermit--frame-native-size (frame)
  (pcase-let ((`(,left ,top ,right ,bottom) (frame-edges frame 'native-edges)))
    (cons (- right left) (- bottom top))))

(defun sthenno/hermit--move-frame (frame pos)
  (when (and frame pos)
    (modify-frame-parameters frame `((left + ,(car pos)) (top + ,(cdr pos))))))

(defun sthenno/hermit--place-bubble ()
  (when-let* ((bubble (sthenno/hermit--live-frame :bubble-frame))
              ((sthenno/hermit--state :bubble-visible))
              (pet-pos (sthenno/hermit--state :pet-pos))
              (pet-size (sthenno/hermit--state :pet-size))
              (bubble-size (sthenno/hermit--state :bubble-outer-size)))
    (sthenno/hermit--move-frame
     bubble
     (cons (round (+ (car pet-pos) (/ (- (car pet-size) (car bubble-size)) 2)))
           (round (- (cdr pet-pos) (cdr bubble-size)
                     (sthenno/hermit--n (sthenno/hermit--opt :gap) 0)))))))

(defun sthenno/hermit--refresh-placement ()
  (when-let* ((pet (sthenno/hermit--live-frame :pet-frame))
              (parent (sthenno/hermit--parent-frame))
              ((frame-live-p parent))
              (size (sthenno/hermit--state :pet-size)))
    (unless (sthenno/hermit--state :manual-position)
      (sthenno/hermit--with-slots
       (:pet-pos (sthenno/hermit--pet-position (sthenno/hermit--opt :placement)
                                               (sthenno/hermit--frame-native-size
                                                parent)
                                               size (sthenno/hermit--opt :gap)))
       nil))
    (sthenno/hermit--move-frame pet (sthenno/hermit--state :pet-pos))
    (sthenno/hermit--place-bubble)))

(defun sthenno/hermit--cancel-timer (key)
  (when-let* ((timer (sthenno/hermit--state key)) ((timerp timer)))
    (cancel-timer timer))
  (setf (sthenno/hermit--state key) nil))

(defun sthenno/hermit-clear-bubble ()
  "Hide the Hermit bubble."
  (interactive)
  (when sthenno/hermit--controller
    (sthenno/hermit--cancel-timer :bubble-timer)
    (sthenno/hermit--with-slots (:queued-bubble nil :bubble nil :bubble-visible nil
                                                :bubble-content-size nil
                                                :bubble-outer-size nil)
                                nil)
    (sthenno/hermit--with-live-frame (frame :bubble-frame)
      (make-frame-invisible frame t))))

(defun sthenno/hermit-say (text)
  "Show TEXT in Hermit's bubble."
  (interactive "sHermit says: ")
  (let ((plain (sthenno/hermit--plain text)))
    (cond ((sthenno/hermit--blank-string-p plain) (sthenno/hermit-clear-bubble))
          ((not sthenno/hermit--controller) nil)
          ((eq (sthenno/hermit--state :status) 'probing)
           (setf (sthenno/hermit--state :queued-bubble) plain))
          (t
           (sthenno/hermit--cancel-timer :bubble-timer)
           (sthenno/hermit--render-bubble plain)
           (when-let* ((timeout (sthenno/hermit--opt :bubble-timeout))
                       ((numberp timeout)) ((> timeout 0)))
             (setf (sthenno/hermit--state :bubble-timer)
                   (run-at-time timeout nil #'sthenno/hermit-clear-bubble)))))))
(defun sthenno/hermit--flush-queued-bubble ()
  (when-let*
      ((queued (sthenno/hermit--state :queued-bubble))
       ((eq (sthenno/hermit--state :status) 'ready)))
    (sthenno/hermit--with-slots (:queued-bubble nil))
    (sthenno/hermit-say queued)))

(defun sthenno/hermit--flush-message ()
  (when sthenno/hermit--controller
    (let ((plain (sthenno/hermit--state :pending-message)))
      (sthenno/hermit--set-state :message-timer nil :pending-message nil)
      (when (and sthenno/hermit-mode (eq (sthenno/hermit--state :status) 'ready)
                 (sthenno/hermit--live-frame :pet-frame)
                 (not (sthenno/hermit--blank-string-p plain)))
        (sthenno/hermit-say plain)))))

(defun sthenno/hermit--message-filter (text)
  (when (and sthenno/hermit-mode (eq (sthenno/hermit--state :status) 'ready)
             (sthenno/hermit--live-frame :pet-frame) (stringp text))
    (let ((plain (sthenno/hermit--plain text)))
      (unless (sthenno/hermit--blank-string-p plain)
        (sthenno/hermit--with-slots (:pending-message plain)
                                    (sthenno/hermit--cancel-timer :message-timer)
                                    (sthenno/hermit--set-state :message-timer
                                                               (run-with-idle-timer
                                                                (if-let*
                                                                    ((delay
                                                                      (sthenno/hermit--opt
                                                                       :message-delay))
                                                                     ((numberp delay))
                                                                     ((> delay 0)))
                                                                    delay
                                                                  0.0125)
                                                                nil
                                                                #'sthenno/hermit--flush-message))))))
  nil)

(defun sthenno/hermit--hooks (action)
  (let ((op (if action #'add-hook #'remove-hook)))
    (when (boundp 'set-message-functions)
      (funcall op 'set-message-functions #'sthenno/hermit--message-filter))
    (dolist (hook
             '((window-size-change-functions . sthenno/hermit--on-window-size-change)
               (delete-frame-functions . sthenno/hermit--on-delete-frame)))
      (funcall op (car hook) (cdr hook)))))

(defun sthenno/hermit--on-window-size-change (frame)
  (when (and sthenno/hermit-mode (eq frame (sthenno/hermit--parent-frame)))
    (sthenno/hermit--refresh-placement)))

(defun sthenno/hermit--on-delete-frame (frame)
  (when (and sthenno/hermit-mode (eq frame (sthenno/hermit--parent-frame)))
    (sthenno/hermit-mode -1)))

(defun sthenno/hermit--event-frame (event)
  (let ((where (posn-window (event-start event))))
    (cond ((windowp where) (window-frame where))
          ((framep where) where))))

(defun sthenno/hermit--posn-xy (posn)
  (let* ((where (posn-window posn))
         (frame (cond ((windowp where) (window-frame where))
                      ((framep where) where)))
         (xy (posn-x-y posn)))
    (when (and frame xy)
      (pcase-let
          ((`(,x . ,y) (if (frame-parent frame) (frame-position frame) '(0 . 0))))
        (cons (+ x (car xy)) (+ y (cdr xy)))))))

(defun sthenno/hermit--event-delta (event)
  (when-let* ((start (sthenno/hermit--posn-xy (event-start event)))
              (end (sthenno/hermit--posn-xy (event-end event))))
    (cons (- (car end) (car start)) (- (cdr end) (cdr start)))))

(defun sthenno/hermit--begin-drag-file (frame file)
  (when (fboundp 'dnd-begin-file-drag)
    (dnd-begin-file-drag file frame 'copy t)))

(defun sthenno/hermit--begin-drag-text (frame text)
  (when (fboundp 'dnd-begin-text-drag)
    (dnd-begin-text-drag text frame 'copy t)))

(defun sthenno/hermit--restore-focus ()
  (when-let* ((frame (or (sthenno/hermit--state :return-frame)
                         (sthenno/hermit--parent-frame)))
              ((framep frame)) ((frame-live-p frame)))
    (select-frame-set-input-focus frame)))

(defun sthenno/hermit-drag (event)
  "Move Hermit with mouse drag."
  (interactive "e")
  (when-let* ((pet (sthenno/hermit--live-frame :pet-frame))
              (delta (sthenno/hermit--event-delta event)))
    (pcase-let ((`(,x . ,y) (or (sthenno/hermit--state :pet-pos) (frame-position pet))))
      (sthenno/hermit--set-state :pet-pos (cons (+ x (car delta)) (+ y (cdr delta)))
                                 :manual-position t)
      (sthenno/hermit--move-frame pet (sthenno/hermit--state :pet-pos))
      (sthenno/hermit--place-bubble)))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-dnd-image (event)
  "Drag Hermit's image file."
  (interactive "e")
  (when-let* (((sthenno/hermit--opt :enable-dnd))
              (file (sthenno/hermit--state :file))
              (frame (sthenno/hermit--event-frame event)))
    (sthenno/hermit--begin-drag-file frame file))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-dnd-bubble (event)
  "Drag Hermit's bubble text."
  (interactive "e")
  (let ((text (sthenno/hermit--state :bubble)))
    (when-let* (((sthenno/hermit--opt :enable-dnd))
                ((not (sthenno/hermit--blank-string-p text)))
                (frame (sthenno/hermit--event-frame event)))
      (sthenno/hermit--begin-drag-text frame text)))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-click (_event)
  "Toggle Hermit's bubble."
  (interactive "e")
  (if (sthenno/hermit--state :bubble-visible)
      (sthenno/hermit-clear-bubble)
    (sthenno/hermit-say (or (current-message) "…")))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-place ()
  "Reset Hermit to `sthenno/hermit-placement'."
  (interactive)
  (when sthenno/hermit--controller
    (sthenno/hermit--with-slots (:manual-position nil))
    (sthenno/hermit--refresh-placement))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit--dispatch-menu-choice (choice)
  (pcase choice
    ('say (sthenno/hermit-say (or (current-message) "…")))
    ('clear (sthenno/hermit-clear-bubble))
    ('place (sthenno/hermit-place)) ('hide (sthenno/hermit-hide))
    ('show (sthenno/hermit-show))
    ('iconify (sthenno/hermit-iconify-parent)) ('restart (sthenno/hermit-restart))
    ('stop (sthenno/hermit-stop)))
  (and choice (not (eq choice 'iconify))))

(defun sthenno/hermit-menu (event)
  "Right-click menu for Hermit."
  (interactive "e")
  (when (sthenno/hermit--dispatch-menu-choice
         (x-popup-menu event
                       '("Emacs Hermit" ("Emacs Hermit"
                                         ("Say current echo" . say)
                                         ("Clear bubble" . clear)
                                         ("Reset position" . place)
                                         ("Hide" . hide) ("Show" . show)
                                         ("Iconify parent" . iconify)
                                         ("Restart" . restart) ("Stop" . stop)))))
    (sthenno/hermit--restore-focus)))

(defun sthenno/hermit--root-frame (frame)
  (named-let walk ((candidate frame))
    (if-let* ((parent (frame-parent candidate)))
        (walk parent)
      candidate)))

(defconst sthenno/hermit--placements
  '(inside-bottom-right outside-right-bottom outside-left-bottom center)
  "Placements accepted by `sthenno/hermit-command'.")

(defun sthenno/hermit--command-parts (input)
  "Return a cons of command verb and rest parsed from INPUT with PEG."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (pcase-let ((`(,rest ,verb)
                     (peg-parse
                      (and (* [blank])
                           (substring (+ [alpha ?-]))
                           (* [blank])
                           (substring (* (any)))
                           (eob)))))
          (cons verb (string-trim rest)))
      (error nil))))

(defun sthenno/hermit--parse-option-args (text)
  "Return a cons of option key text and value text parsed from TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (condition-case nil
        (pcase-let ((`(,value ,key)
                     (peg-parse
                      (and (* [blank])
                           (substring (+ [alnum ?: ?-]))
                           (+ [blank])
                           (substring (* (any)))
                           (eob)))))
          (cons key (string-trim value)))
      (error nil))))

(defun sthenno/hermit--parse-placement (text)
  "Parse TEXT into a supported Hermit placement symbol."
  (let ((placement (intern (string-trim text))))
    (unless (memq placement sthenno/hermit--placements)
      (user-error "Unknown Hermit placement: %s" text))
    placement))

(defun sthenno/hermit--option-key (text)
  "Parse TEXT into a supported Hermit option keyword."
  (let* ((name (string-remove-prefix ":" (string-trim text)))
         (key (intern (concat ":" name))))
    (unless (assq key sthenno/hermit--opt-vars)
      (user-error "Unknown Hermit option: %s" text))
    key))

(defun sthenno/hermit--number-text-p (text)
  "Return non-nil when TEXT is a simple integer or float literal."
  (string-match-p "\\`[-+]?[0-9]+\\(?:\\.[0-9]+\\)?\\'" text))

(defun sthenno/hermit--parse-option-value (key text)
  "Parse option KEY value from TEXT."
  (let ((value (string-trim text)))
    (cond
     ((member value '("t" "true" "yes" "on")) t)
     ((member value '("nil" "false" "no" "off")) nil)
     ((eq key :placement) (sthenno/hermit--parse-placement value))
     ((eq key :image-mask)
      (cond ((string= value "heuristic") 'heuristic)
            ((string= value "nil") nil)
            (t (user-error "Unsupported Hermit image mask: %s" value))))
     ((sthenno/hermit--number-text-p value) (string-to-number value))
     (t value))))

(defun sthenno/hermit--parse-command (input)
  "Parse INPUT as a Hermit command."
  (pcase-let ((`(,verb . ,rest) (sthenno/hermit--command-parts input)))
    (unless verb
      (user-error "Invalid Hermit command: %s" input))
    (pcase verb
      ("say"
       (when (sthenno/hermit--blank-string-p rest)
         (user-error "Hermit say command needs text"))
       (sthenno/hermit--make-command :action 'say :text rest))
      ("place"
       (sthenno/hermit--make-command
        :action 'place
        :placement (sthenno/hermit--parse-placement rest)))
      ("option"
       (pcase-let ((`(,key-text . ,value-text)
                    (sthenno/hermit--parse-option-args rest)))
         (unless key-text
           (user-error "Hermit option command needs KEY VALUE"))
         (let ((key (sthenno/hermit--option-key key-text)))
           (sthenno/hermit--make-command
            :action 'option
            :option key
            :value (sthenno/hermit--parse-option-value key value-text)))))
      ("run"
       (when (sthenno/hermit--blank-string-p rest)
         (user-error "Hermit run command needs a shell command"))
       (sthenno/hermit--make-command :action 'run :shell-command rest))
      (_ (user-error "Unknown Hermit command: %s" verb)))))

(defun sthenno/hermit--process-record-for (process)
  "Return the active process record when PROCESS is current."
  (let ((record (sthenno/hermit--state :process-record)))
    (and (sthenno/hermit-process-record-p record)
         (eq (sthenno/hermit-process-record-process record) process)
         record)))

(defun sthenno/hermit--cancel-process ()
  "Delete the active Hermit process if it is still live."
  (when-let* ((record (sthenno/hermit--state :process-record))
              (process (sthenno/hermit-process-record-process record)))
    (when (process-live-p process)
      (delete-process process)))
  (setf (sthenno/hermit--state :process-record) nil))

(defun sthenno/hermit--process-filter (process text)
  "Collect PROCESS output TEXT and show it in Hermit's bubble."
  (when-let* ((record (sthenno/hermit--process-record-for process)))
    (setf (sthenno/hermit-process-record-output record)
          (concat (or (sthenno/hermit-process-record-output record) "") text))
    (let ((plain (sthenno/hermit--plain
                  (sthenno/hermit-process-record-output record))))
      (unless (sthenno/hermit--blank-string-p plain)
        (sthenno/hermit-say plain)))))

(defun sthenno/hermit--process-sentinel (process event)
  "Report PROCESS EVENT and clear the active process record."
  (when-let* ((record (sthenno/hermit--process-record-for process)))
    (let ((command (sthenno/hermit-process-record-command record))
          (event (string-trim event)))
      (setf (sthenno/hermit--state :process-record) nil)
      (sthenno/hermit-say
       (if (string= event "finished")
           (format "run finished: %s" command)
         (format "run stopped (%s): %s" event command))))))

(defun sthenno/hermit-run-command (command)
  "Run shell COMMAND asynchronously and send its output to Hermit."
  (interactive "sHermit run: ")
  (unless sthenno/hermit--controller
    (user-error "sthenno/hermit-mode is not active"))
  (when (sthenno/hermit--blank-string-p command)
    (user-error "Hermit run command needs a shell command"))
  (when-let* ((record (sthenno/hermit--state :process-record))
              (process (sthenno/hermit-process-record-process record))
              ((process-live-p process)))
    (unless (yes-or-no-p "Kill the running Hermit process? ")
      (user-error "Hermit process is still running"))
    (sthenno/hermit--cancel-process))
  (let* ((process
          (make-process
           :name "sthenno/hermit-run"
           :buffer nil
           :command (list shell-file-name shell-command-switch command)
           :connection-type 'pipe
           :noquery t
           :filter #'sthenno/hermit--process-filter
           :sentinel #'sthenno/hermit--process-sentinel))
         (record (sthenno/hermit--make-process-record
                  :process process
                  :command command)))
    (setf (sthenno/hermit--state :process-record) record)
    (sthenno/hermit-say (format "run started: %s" command))
    process))

(defun sthenno/hermit--dispatch-command (command)
  "Execute parsed Hermit COMMAND."
  (pcase (sthenno/hermit-command-action command)
    ('say (sthenno/hermit-say (sthenno/hermit-command-text command)))
    ('place
     (setf (sthenno/hermit--opt :placement)
           (sthenno/hermit-command-placement command))
     (sthenno/hermit-place))
    ('option
     (setf (sthenno/hermit--opt (sthenno/hermit-command-option command))
           (sthenno/hermit-command-value command))
     (when (memq (sthenno/hermit-command-option command)
                 '(:placement :gap))
       (sthenno/hermit-place))
     (sthenno/hermit-command-value command))
    ('run (sthenno/hermit-run-command
           (sthenno/hermit-command-shell-command command)))))

;;;###autoload
(defun sthenno/hermit-command (input)
  "Read and execute a PEG-parsed Hermit command from INPUT."
  (interactive (list (read-string "Hermit command: ")))
  (sthenno/hermit--dispatch-command (sthenno/hermit--parse-command input)))

(defun sthenno/hermit--teardown-controller ()
  (sthenno/hermit--cancel-process)
  (mapc #'sthenno/hermit--cancel-timer '(:bubble-timer :message-timer))
  (dolist (key '(:bubble-frame :pet-frame))
    (sthenno/hermit--with-live-frame (frame key)
      (delete-frame frame t))
    (setf (sthenno/hermit--state key) nil))
  (dolist (key '(:bubble-buffer :image-buffer))
    (sthenno/hermit--with-live-buffer (buffer key)
      (kill-buffer buffer))
    (setf (sthenno/hermit--state key) nil))
  (setq sthenno/hermit--controller nil))

(defun sthenno/hermit--disable ()
  (sthenno/hermit--hooks nil)
  (sthenno/hermit--teardown-controller)
  (setq sthenno/hermit--request nil))

(defun sthenno/hermit--enable (&optional file parent)
  (sthenno/hermit--disable)
  (sthenno/hermit--sync-options)
  (let* ((file (expand-file-name (or file (sthenno/hermit--opt :image-file))))
         (parent (or parent (selected-frame))))
    (unless (display-graphic-p parent)
      (user-error "sthenno/hermit requires a graphical frame"))
    (unless (file-readable-p file) (user-error "Cannot read image file: %s" file))
    (when (fboundp 'clear-image-cache) (clear-image-cache file))
    (setq sthenno/hermit--controller
          (sthenno/hermit--make-controller
           :status 'probing
           :parent-frame parent
           :return-frame (selected-frame)
           :file file))
    (let ((size (sthenno/hermit--scale-image-size
                 (sthenno/hermit--raw-image-size file)
                 (sthenno/hermit--n (sthenno/hermit--opt :image-height) 1 420)
                 file)))
      (sthenno/hermit--set-state :status 'ready :pet-size size
                                 :pet-frame
                                 (make-frame
                                  (sthenno/hermit--pet-frame-parameters parent size)))
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
        (error (setq sthenno/hermit-mode nil)
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
              ((framep root)) ((frame-live-p root)))
    (iconify-frame root)))

(provide 'init-gpt)
