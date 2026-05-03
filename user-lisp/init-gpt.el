;;; init-gpt.el --- AI assistance -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the interactive AI assistant configuration.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

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


;;; Misc.

(defgroup sthenno/hermit nil
  "A small image pet attached to an Emacs frame."
  :group 'applications)

(defcustom sthenno/hermit-image-file
  (locate-user-emacs-file "resources/hmt-01.png")
  "PNG file used as the pet image."
  :type 'file
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-image-height 420
  "Rendered pet image height in pixels."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-image-mask nil
  "Optional mask passed to `create-image'.
Use nil for normal alpha rendering.  Use `heuristic' only as a fallback."
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

(defcustom sthenno/hermit-gap 15
  "Gap between the pet, bubble, and parent frame in pixels."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-alpha-background 0
  "Background opacity of the pet image frame.
0 means transparent; 100 means opaque."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-no-accept-focus nil
  "Whether pet frames should avoid accepting focus.
Keep this nil while mouse events are important.  Pet commands restore
focus to the parent frame after handling events."
  :type 'boolean
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-timeout 3.0
  "Seconds before the speech bubble is hidden.
Set to nil or a non-positive number to keep it visible until replaced."
  :type '(choice number (const nil))
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-max-columns 50
  "Maximum display columns used by bubble text before wrapping."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-min-columns 10
  "Minimum display columns used by bubble text."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-max-lines 3
  "Maximum number of text lines shown in the bubble."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-padding-columns 2
  "Horizontal bubble padding in `default' face character cells."
  :type 'integer
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-bubble-padding-lines 2
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
  "Whether Shift-mouse-1 starts drag-and-drop from pet frames.
On the pet image, the dragged object is `sthenno/hermit-image-file'.  On the
bubble, the dragged object is the current bubble text."
  :type 'boolean
  :group 'sthenno/hermit)

(defface sthenno/hermit-bubble-face
  '((t (:inherit default :foreground "#303030" :background "#fff7fb")))
  "Face used by bubble text.
The font is inherited from `default'."
  :group 'sthenno/hermit)

(defface sthenno/hermit-bubble-border-face
  '((t (:inherit default :background "#ff9ec4")))
  "Face whose background color is used as the bubble border color."
  :group 'sthenno/hermit)

(defface sthenno/hermit-bubble-box-face
  '((t (:inherit sthenno/hermit-bubble-face :box (:line-width -1 :color "#ff9ec4"))))
  "Optional bubble face used when `sthenno/hermit-bubble-use-text-box' is non-nil."
  :group 'sthenno/hermit)

(defvar sthenno/hermit--state nil)
(defvar sthenno/hermit--requested-file nil)
(defvar sthenno/hermit--requested-parent nil)

(defconst sthenno/hermit--image-buffer-name " *sthenno/hermit-image*")
(defconst sthenno/hermit--bubble-buffer-name " *sthenno/hermit-bubble*")

;;; State

(defun sthenno/hermit--get (key)
  (plist-get sthenno/hermit--state key))

(defun sthenno/hermit--put (&rest pairs)
  (while pairs
    (setq sthenno/hermit--state
          (plist-put sthenno/hermit--state (car pairs) (cadr pairs)))
    (setq pairs (cddr pairs)))
  sthenno/hermit--state)

(defun sthenno/hermit--live-frame (key)
  (let ((frame (sthenno/hermit--get key)))
    (if (and (framep frame) (frame-live-p frame))
        frame
      nil)))

(defun sthenno/hermit--parent-frame ()
  (sthenno/hermit--get :parent-frame))

;;; Text

(defun sthenno/hermit--trim-left (string)
  (if (string-match "\\`[[:space:]]+" string)
      (substring string (match-end 0))
    string))

(defun sthenno/hermit--trim-right (string)
  (if (string-match "[[:space:]]+\\'" string)
      (substring string 0 (match-beginning 0))
    string))

(defun sthenno/hermit--trim (string)
  (sthenno/hermit--trim-left (sthenno/hermit--trim-right string)))

(defun sthenno/hermit--blank-string-p (string)
  (or (not (stringp string))
      (string-match-p "\\`[[:space:]]*\\'" string)))

(defun sthenno/hermit--plain (text)
  (if (stringp text)
      (sthenno/hermit--trim (replace-regexp-in-string "[[:space:]]+" " "
                                                      (substring-no-properties text)))
    nil))

(defun sthenno/hermit--pad-right (string width)
  (concat string (make-string (max 0 (- width (string-width string))) ? )))

(defun sthenno/hermit--max-width (strings minimum)
  (let ((width minimum))
    (dolist (string strings width)
      (setq width (max width (string-width string))))))

(defun sthenno/hermit--join-lines (lines)
  (mapconcat #'identity lines "\n"))

(defun sthenno/hermit--wrap (text max-columns max-lines)
  (let ((rest (or (sthenno/hermit--plain text) ""))
        (columns (max 4 max-columns))
        (limit (max 1 max-lines))
        lines)
    (while (and (not (sthenno/hermit--blank-string-p rest))
                (< (length lines) limit))
      (let ((line (truncate-string-to-width rest columns nil nil "")))
        (if (sthenno/hermit--blank-string-p line)
            (setq line (substring rest 0 1)))
        (setq rest (sthenno/hermit--trim-left (substring rest (length line))))
        (setq lines (append lines (list line)))))
    (if (and lines (not (sthenno/hermit--blank-string-p rest)))
        (let ((last-cell (last lines)))
          (setcar last-cell
                  (concat (truncate-string-to-width (car last-cell)
                                                    (max 1 (1- columns)) nil nil "")
                          "…"))))
    lines))

;;; Faces

(defun sthenno/hermit--usable-color (value fallback)
  (if (and (stringp value) (> (length value) 0)) value fallback))

(defun sthenno/hermit--face-fg (face frame fallback)
  (sthenno/hermit--usable-color (face-foreground face frame t) fallback))

(defun sthenno/hermit--face-bg (face frame fallback)
  (sthenno/hermit--usable-color (face-background face frame t) fallback))

(defun sthenno/hermit--parent-font-parameter (parent)
  (let ((font (frame-parameter parent 'font)))
    (if font
        (list (cons 'font font))
      nil)))


;;; Buffers

(defvar sthenno/hermit-image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'sthenno/hermit-click)
    (define-key map [drag-mouse-1] #'sthenno/hermit-drag)
    (define-key map [S-down-mouse-1] #'sthenno/hermit-dnd-image)
    (define-key map [mouse-2] #'sthenno/hermit-iconify-parent)
    (define-key map [mouse-3] #'sthenno/hermit-menu)
    (define-key map (kbd "q") #'sthenno/hermit-stop)
    map))

(defvar sthenno/hermit-bubble-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'sthenno/hermit-clear-bubble)
    (define-key map [drag-mouse-1] #'sthenno/hermit-drag)
    (define-key map [S-down-mouse-1] #'sthenno/hermit-dnd-bubble)
    (define-key map [mouse-2] #'sthenno/hermit-iconify-parent)
    (define-key map [mouse-3] #'sthenno/hermit-menu)
    (define-key map (kbd "q") #'sthenno/hermit-stop)
    map))

(define-derived-mode sthenno/hermit-view-mode special-mode "sthenno/hermit-view"
  "Major mode used by Emacs pet child-frame buffers."
  (setq-local cursor-type nil)
  (setq-local cursor-in-non-selected-windows nil)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (setq-local tab-line-format nil)
  (setq-local truncate-lines t)
  (setq-local line-spacing 0)
  (setq-local display-line-numbers nil)
  (setq-local left-margin-width 0)
  (setq-local right-margin-width 0)
  (setq-local buffer-read-only t)) ; FIXME

(defun sthenno/hermit--buffer (name map)
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'sthenno/hermit-view-mode)
        (sthenno/hermit-view-mode))
      (use-local-map map))
    buffer))

(defun sthenno/hermit--install-window (frame buffer)
  (let ((window (frame-root-window frame)))
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    (set-window-fringes window 0 0)
    (set-window-margins window 0 0)
    (set-window-parameter window 'mode-line-format 'none)
    (set-window-parameter window 'header-line-format 'none)
    (set-window-parameter window 'tab-line-format 'none)))

;;; Geometry

(defun sthenno/hermit--raw-image-size (file)
  (let* ((image (create-image file nil nil))
         (size (image-size image t)))
    (cons (round (car size))
          (round (cdr size)))))

(defun sthenno/hermit--scaled-image-size (file height)
  (let* ((raw (sthenno/hermit--raw-image-size file))
         (raw-width (float (car raw)))
         (raw-height (float (cdr raw))))
    (if (<= raw-height 0)
        (error "Invalid image height: %s" file)
      (cons (round (* height (/ raw-width raw-height)))
            height))))

(defun sthenno/hermit--frame-native-size (frame)
  (let* ((edges (frame-edges frame 'native-edges))
         (left (nth 0 edges))
         (top (nth 1 edges))
         (right (nth 2 edges))
         (bottom (nth 3 edges)))
    (cons (- right left)
          (- bottom top))))

(defun sthenno/hermit--pet-position (placement parent-size pet-size gap)
  (let ((pw (car parent-size))
        (ph (cdr parent-size))
        (w (car pet-size))
        (h (cdr pet-size)))
    (cond ((eq placement 'inside-bottom-right)
           (cons (max 0 (- pw w gap))
                 (max 0 (- ph h gap))))
          ((eq placement 'outside-right-bottom)
           (cons (+ pw gap)
                 (max 0 (- ph h gap))))
          ((eq placement 'outside-left-bottom)
           (cons (- (+ w gap))
                 (max 0 (- ph h gap))))
          ((eq placement 'center)
           (cons (max 0 (/ (- pw w) 2))
                 (max 0 (/ (- ph h) 2))))
          (t
           (sthenno/hermit--pet-position
            'inside-bottom-right parent-size pet-size gap)))))

(defun sthenno/hermit--bubble-render-lines (lines columns)
  (let* ((pad-columns (max 0 sthenno/hermit-bubble-padding-columns))
         (pad-lines (max 0 sthenno/hermit-bubble-padding-lines))
         (inner-columns (max columns sthenno/hermit-bubble-min-columns))
         (full-columns (+ inner-columns (* 2 pad-columns)))
         (blank (make-string full-columns ? ))
         (left (make-string pad-columns ? )))
    (append (make-list pad-lines blank)
            (mapcar (lambda (line)
                      (concat left
                              (sthenno/hermit--pad-right line inner-columns)
                              left))
                    lines)
            (make-list pad-lines blank))))

(defun sthenno/hermit--bubble-layout (text parent)
  (let ((lines (sthenno/hermit--wrap
                text
                sthenno/hermit-bubble-max-columns
                sthenno/hermit-bubble-max-lines)))
    (if lines
        (let* ((inner-columns (sthenno/hermit--max-width lines
                                                         sthenno/hermit-bubble-min-columns))
               (render-lines (sthenno/hermit--bubble-render-lines lines inner-columns))
               (columns (sthenno/hermit--max-width render-lines 1))
               (rows (length render-lines))
               (char-width (max 1 (frame-char-width parent)))
               (char-height (max 1 (frame-char-height parent)))
               (content-size (cons (* columns char-width)
                                   (* rows char-height)))
               (outer-size (cons (+ (car content-size)
                                    (* 2 sthenno/hermit-bubble-border-width))
                                 (+ (cdr content-size)
                                    (* 2 sthenno/hermit-bubble-border-width)))))
          (list :plain-lines lines
                :render-lines render-lines
                :content-size content-size
                :outer-size outer-size))
      nil)))

(defun sthenno/hermit--bubble-position (pet-pos pet-size bubble-size)
  (cons (round (+ (car pet-pos) (/ (- (car pet-size) (car bubble-size)) 2)))
        (round (- (cdr pet-pos) (cdr bubble-size) sthenno/hermit-gap))))

(defun sthenno/hermit--position-parameters (pos)
  (list (cons 'left (list '+ (car pos)))
        (cons 'top (list '+ (cdr pos)))))

(defun sthenno/hermit--move-frame (frame pos)
  (if (and (framep frame) (frame-live-p frame) pos)
      (modify-frame-parameters frame
                               (sthenno/hermit--position-parameters pos))))

;;; Frame parameters

(defun sthenno/hermit--common-frame-parameters (parent name content-size)
  (append (list (cons 'name name)
                (cons 'title name)
                (cons 'parent-frame parent)
                (cons 'delete-before parent)
                (cons 'mouse-wheel-frame parent)
                (cons 'no-other-frame t)
                (cons 'minibuffer nil)
                (cons 'visibility nil)
                (cons 'undecorated-round t) ; FIXME
                (cons 'skip-taskbar t)
                (cons 'no-focus-on-map t)
                (cons 'no-accept-focus sthenno/hermit-no-accept-focus)
                (cons 'z-group 'above)
                (cons 'left '(+ 0))
                (cons 'top '(+ 0))
                (cons 'width (cons 'text-pixels (car content-size)))
                (cons 'height (cons 'text-pixels (cdr content-size)))
                (cons 'min-width 0)
                (cons 'min-height 0)
                (cons 'border-width 0)
                (cons 'left-fringe 0)
                (cons 'right-fringe 0)
                (cons 'vertical-scroll-bars nil)
                (cons 'horizontal-scroll-bars nil)
                (cons 'tool-bar-lines 0)
                (cons 'tab-bar-lines 0)
                (cons 'line-spacing 0)
                (cons 'cursor-type nil)
                (cons 'no-special-glyphs t))
          (sthenno/hermit--parent-font-parameter parent)))

(defun sthenno/hermit--pet-frame-parameters (parent image-size)
  (append (sthenno/hermit--common-frame-parameters
           parent "sthenno/hermit" image-size)
          (list (cons 'foreground-color
                      (sthenno/hermit--face-fg 'default parent "#ffffff"))
                (cons 'background-color
                      (sthenno/hermit--face-bg 'default parent "#000000"))
                (cons 'alpha-background sthenno/hermit-alpha-background)
                (cons 'internal-border-width 0)
                (cons 'child-frame-border-width 0))))

(defun sthenno/hermit--bubble-frame-parameters (parent content-size)
  (let ((fg (sthenno/hermit--face-fg
             'sthenno/hermit-bubble-face parent "#303030"))
        (bg (sthenno/hermit--face-bg
             'sthenno/hermit-bubble-face parent "#fff7fb"))
        (border (sthenno/hermit--face-bg
                 'sthenno/hermit-bubble-border-face parent "#ff9ec4")))
    (append
     (sthenno/hermit--common-frame-parameters parent "sthenno/hermit-bubble" content-size)
     (list (cons 'foreground-color fg)
           (cons 'background-color bg)
           (cons 'border-color border)
           (cons 'alpha-background 100)
           (cons 'internal-border-width 0)
           (cons 'child-frame-border-width sthenno/hermit-bubble-border-width)))))

;;; Rendering

(defun sthenno/hermit--png-image (file size)
  (if sthenno/hermit-image-mask
      (create-image file nil nil
                    :width (car size)
                    :height (cdr size)
                    :ascent 100
                    :mask sthenno/hermit-image-mask)
    (create-image file nil nil
                  :width (car size)
                  :height (cdr size)
                  :ascent 100)))

(defun sthenno/hermit--render-pet ()
  (let ((frame (sthenno/hermit--live-frame :pet-frame)))
    (if frame
        (let ((buffer (sthenno/hermit--buffer
                       sthenno/hermit--image-buffer-name
                       sthenno/hermit-image-mode-map))
              (file (sthenno/hermit--get :file))
              (size (sthenno/hermit--get :pet-size)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert-image (sthenno/hermit--png-image file size))
              (goto-char (point-min))))
          (sthenno/hermit--install-window frame buffer)))))

(defun sthenno/hermit--bubble-face ()
  (if sthenno/hermit-bubble-use-text-box
      'sthenno/hermit-bubble-box-face
    'sthenno/hermit-bubble-face))

(defun sthenno/hermit--ensure-bubble-frame (content-size)
  (let ((frame (sthenno/hermit--live-frame :bubble-frame)))
    (if frame
        frame
      (let* ((parent (sthenno/hermit--parent-frame))
             (buffer (sthenno/hermit--buffer sthenno/hermit--bubble-buffer-name
                                             sthenno/hermit-bubble-mode-map))
             (new-frame (make-frame
                         (sthenno/hermit--bubble-frame-parameters parent content-size))))
        (sthenno/hermit--put :bubble-frame new-frame)
        (sthenno/hermit--install-window new-frame buffer)
        new-frame))))

(defun sthenno/hermit--render-bubble (text)
  (let* ((parent (sthenno/hermit--parent-frame))
         (layout (if (and parent (frame-live-p parent))
                     (sthenno/hermit--bubble-layout text parent)
                   nil)))
    (if (not layout)
        (sthenno/hermit-clear-bubble)
      (let* ((render-lines (plist-get layout :render-lines))
             (content-size (plist-get layout :content-size))
             (outer-size (plist-get layout :outer-size))
             (frame (sthenno/hermit--ensure-bubble-frame content-size))
             (buffer (sthenno/hermit--buffer
                      sthenno/hermit--bubble-buffer-name
                      sthenno/hermit-bubble-mode-map))
             (body (sthenno/hermit--join-lines render-lines)))
        (sthenno/hermit--put :bubble text
                             :bubble-visible t
                             :bubble-content-size content-size
                             :bubble-outer-size outer-size)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize body 'face (sthenno/hermit--bubble-face)))
            (goto-char (point-min))))
        (sthenno/hermit--install-window frame buffer)
        (modify-frame-parameters frame
                                 (sthenno/hermit--bubble-frame-parameters parent
                                                                          content-size))
        (set-frame-size frame (car content-size) (cdr content-size) t)
        (sthenno/hermit--place-bubble)
        (make-frame-visible frame)
        (raise-frame frame)))))

;;; Placement

(defun sthenno/hermit--place-bubble ()
  (let ((bubble (sthenno/hermit--live-frame :bubble-frame)))
    (if (and bubble (sthenno/hermit--get :bubble-visible))
        (let ((pet-pos (sthenno/hermit--get :pet-pos))
              (pet-size (sthenno/hermit--get :pet-size))
              (bubble-size (sthenno/hermit--get :bubble-outer-size)))
          (if (and pet-pos pet-size bubble-size)
              (sthenno/hermit--move-frame
               bubble
               (sthenno/hermit--bubble-position
                pet-pos pet-size bubble-size)))))))

(defun sthenno/hermit--refresh-placement ()
  (let ((pet (sthenno/hermit--live-frame :pet-frame))
        (parent (sthenno/hermit--parent-frame)))
    (if (and pet parent (frame-live-p parent))
        (let ((pet-size (sthenno/hermit--get :pet-size)))
          (unless (sthenno/hermit--get :manual-position)
            (sthenno/hermit--put
             :pet-pos
             (sthenno/hermit--pet-position sthenno/hermit-placement
                                           (sthenno/hermit--frame-native-size parent)
                                           pet-size
                                           sthenno/hermit-gap)))
          (sthenno/hermit--move-frame pet (sthenno/hermit--get :pet-pos))
          (sthenno/hermit--place-bubble)))))

(defun sthenno/hermit-place ()
  "Reset the pet to `sthenno/hermit-placement'."
  (interactive)
  (sthenno/hermit--put :manual-position nil)
  (sthenno/hermit--refresh-placement)
  (sthenno/hermit--restore-focus))

;;; Bubble and echo-area

(defun sthenno/hermit-clear-bubble ()
  "Hide the bubble frame."
  (interactive)
  (let ((timer (sthenno/hermit--get :bubble-timer)))
    (if (timerp timer)
        (cancel-timer timer)))
  (sthenno/hermit--put :bubble nil
                       :bubble-visible nil
                       :bubble-timer nil
                       :bubble-content-size nil
                       :bubble-outer-size nil)
  (let ((bubble (sthenno/hermit--live-frame :bubble-frame)))
    (if bubble
        (make-frame-invisible bubble t))))

(defun sthenno/hermit-say (text)
  "Show TEXT in the pet bubble."
  (interactive "sPet says: ")
  (let ((plain (sthenno/hermit--plain text)))
    (if (sthenno/hermit--blank-string-p plain)
        (sthenno/hermit-clear-bubble)
      (let ((timer (sthenno/hermit--get :bubble-timer)))
        (if (timerp timer)
            (cancel-timer timer)))
      (sthenno/hermit--render-bubble plain)
      (if (and sthenno/hermit-bubble-timeout
               (> sthenno/hermit-bubble-timeout 0))
          (sthenno/hermit--put :bubble-timer
                               (run-at-time sthenno/hermit-bubble-timeout
                                            nil
                                            #'sthenno/hermit-clear-bubble))))))

(defun sthenno/hermit--message-filter (text)
  (if (and sthenno/hermit-mode
           (sthenno/hermit--live-frame :pet-frame)
           (stringp text))
      (let ((plain (sthenno/hermit--plain text)))
        (unless (sthenno/hermit--blank-string-p plain)
          (run-at-time 0 nil #'sthenno/hermit-say plain))))
  nil)

;;; Hooks

(defun sthenno/hermit--on-window-size-change (frame)
  (if (and sthenno/hermit-mode
           (sthenno/hermit--live-frame :pet-frame)
           (eq frame (sthenno/hermit--parent-frame)))
      (sthenno/hermit--refresh-placement)))

(defun sthenno/hermit--on-delete-frame (frame)
  (if (and sthenno/hermit-mode
           (eq frame (sthenno/hermit--parent-frame)))
      (sthenno/hermit-mode -1)))

(defun sthenno/hermit--install-hooks ()
  (if (boundp 'set-message-functions)
      (add-hook 'set-message-functions #'sthenno/hermit--message-filter))
  (add-hook 'window-size-change-functions #'sthenno/hermit--on-window-size-change)
  (add-hook 'delete-frame-functions #'sthenno/hermit--on-delete-frame))

(defun sthenno/hermit--remove-hooks ()
  (if (boundp 'set-message-functions)
      (remove-hook 'set-message-functions #'sthenno/hermit--message-filter))
  (remove-hook 'window-size-change-functions #'sthenno/hermit--on-window-size-change)
  (remove-hook 'delete-frame-functions #'sthenno/hermit--on-delete-frame))

;;; Mouse and DND

(defun sthenno/hermit--event-frame (event)
  (let* ((pos (event-start event))
         (where (and pos (posn-window pos))))
    (cond ((windowp where) (window-frame where))
          ((framep where) where)
          (t nil))))

(defun sthenno/hermit--posn-parent-xy (posn)
  (let* ((where (posn-window posn))
         (frame (cond ((windowp where) (window-frame where))
                      ((framep where) where)
                      (t nil)))
         (xy (posn-x-y posn)))
    (if (and frame xy)
        (let ((base (if (frame-parent frame)
                        (frame-position frame)
                      (cons 0 0))))
          (cons (+ (car base) (car xy))
                (+ (cdr base) (cdr xy))))
      nil)))

(defun sthenno/hermit--event-delta (event)
  (let ((start (sthenno/hermit--posn-parent-xy (event-start event)))
        (end (sthenno/hermit--posn-parent-xy (event-end event))))
    (if (and start end)
        (cons (- (car end) (car start))
              (- (cdr end) (cdr start)))
      nil)))

(defun sthenno/hermit-drag (event)
  "Move the pet child frame with mouse drag."
  (interactive "e")
  (let ((pet (sthenno/hermit--live-frame :pet-frame))
        (delta (sthenno/hermit--event-delta event)))
    (if (and pet delta)
        (let* ((old (or (sthenno/hermit--get :pet-pos)
                        (frame-position pet)))
               (new (cons (+ (car old) (car delta))
                          (+ (cdr old) (cdr delta)))))
          (sthenno/hermit--put :pet-pos new
                               :manual-position t)
          (sthenno/hermit--move-frame pet new)
          (sthenno/hermit--place-bubble))))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-dnd-image (event)
  "Drag `sthenno/hermit-image-file' to another application, when supported."
  (interactive "e")
  (if sthenno/hermit-enable-dnd
      (let ((file (sthenno/hermit--get :file))
            (frame (sthenno/hermit--event-frame event)))
        (if (and file frame (fboundp 'dnd-begin-file-drag))
            (dnd-begin-file-drag file frame 'copy t))))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-dnd-bubble (event)
  "Drag current bubble text to another application, when supported."
  (interactive "e")
  (if sthenno/hermit-enable-dnd
      (let ((text (sthenno/hermit--get :bubble))
            (frame (sthenno/hermit--event-frame event)))
        (if (and (not (sthenno/hermit--blank-string-p text))
                 frame
                 (fboundp 'dnd-begin-text-drag))
            (dnd-begin-text-drag text frame 'copy t))))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-click (_event)
  "Left click toggles the current bubble."
  (interactive "e")
  (if (sthenno/hermit--get :bubble-visible)
      (sthenno/hermit-clear-bubble)
    (sthenno/hermit-say (or (current-message) "…")))
  (sthenno/hermit--restore-focus))

(defun sthenno/hermit-menu (event)
  "Right-click menu for the pet."
  (interactive "e")
  (let ((choice (x-popup-menu event
                              '("Emacs Pet"
                                ("Emacs Pet"
                                 ("Say current echo" . say)
                                 ("Clear bubble" . clear)
                                 ("Reset position" . place)
                                 ("Hide" . hide)
                                 ("Show" . show)
                                 ("Iconify parent" . iconify)
                                 ("Restart" . restart)
                                 ("Stop" . stop))))))
    (cond ((eq choice 'say)
           (sthenno/hermit-say (or (current-message) "…")))
          ((eq choice 'clear)
           (sthenno/hermit-clear-bubble))
          ((eq choice 'place)
           (sthenno/hermit-place))
          ((eq choice 'hide)
           (sthenno/hermit-hide))
          ((eq choice 'show)
           (sthenno/hermit-show))
          ((eq choice 'iconify)
           (sthenno/hermit-iconify-parent))
          ((eq choice 'restart)
           (sthenno/hermit-restart))
          ((eq choice 'stop)
           (sthenno/hermit-stop))))
  (sthenno/hermit--restore-focus))


;;; Lifecycle

(defun sthenno/hermit--restore-focus ()
  (let ((frame (or (sthenno/hermit--get :return-frame)
                   (sthenno/hermit--parent-frame))))
    (if (and (framep frame) (frame-live-p frame))
        (select-frame-set-input-focus frame))))

(defun sthenno/hermit--disable ()
  (sthenno/hermit--remove-hooks)
  (let ((timer (sthenno/hermit--get :bubble-timer)))
    (if (timerp timer)
        (cancel-timer timer)))
  (let ((bubble (sthenno/hermit--live-frame :bubble-frame)))
    (if bubble
        (delete-frame bubble t)))
  (let ((pet (sthenno/hermit--live-frame :pet-frame)))
    (if pet
        (delete-frame pet t)))
  (setq sthenno/hermit--state nil)
  (setq sthenno/hermit--requested-file nil)
  (setq sthenno/hermit--requested-parent nil))

(defun sthenno/hermit--enable (&optional file parent)
  (sthenno/hermit--disable)
  (let* ((image-file (expand-file-name (or file sthenno/hermit-image-file)))
         (parent-frame (or parent (selected-frame)))
         (return-frame (selected-frame)))
    (unless (display-graphic-p parent-frame)
      (user-error "sthenno/hermit requires a graphical frame"))
    (unless (file-readable-p image-file)
      (user-error "Cannot read image file: %s" image-file))
    (if (fboundp 'clear-image-cache)
        (clear-image-cache image-file))
    (let* ((pet-size (sthenno/hermit--scaled-image-size
                      image-file
                      sthenno/hermit-image-height))
           (buffer (sthenno/hermit--buffer
                    sthenno/hermit--image-buffer-name
                    sthenno/hermit-image-mode-map))
           (pet-frame (make-frame (sthenno/hermit--pet-frame-parameters parent-frame
                                                                        pet-size))))
      (setq sthenno/hermit--state (list :parent-frame parent-frame
                                        :return-frame return-frame
                                        :pet-frame pet-frame
                                        :bubble-frame nil
                                        :file image-file
                                        :pet-size pet-size
                                        :pet-pos nil
                                        :manual-position nil
                                        :bubble nil
                                        :bubble-visible nil
                                        :bubble-timer nil
                                        :bubble-content-size nil
                                        :bubble-outer-size nil))
      (sthenno/hermit--install-window pet-frame buffer)
      (sthenno/hermit--render-pet)
      (set-frame-size pet-frame (car pet-size) (cdr pet-size) t)
      (sthenno/hermit--refresh-placement)
      (make-frame-visible pet-frame)
      (raise-frame pet-frame)
      (sthenno/hermit--install-hooks)
      (sthenno/hermit--restore-focus))))

;;;###autoload
(define-minor-mode sthenno/hermit-mode
  "Toggle Hermit Pet mode.
This is a global minor mode. Enabling it creates a pair of child frames
attached to the selected graphical frame: one image frame for the pet
and one dynamic text frame for the speech bubble. Disabling it deletes
those frames and removes the hooks used to mirror echo-area messages."
  :group 'sthenno/hermit
  (if sthenno/hermit-mode
      (condition-case err
          (sthenno/hermit--enable
           sthenno/hermit--requested-file
           sthenno/hermit--requested-parent)
        (error (setq sthenno/hermit-mode nil)
               (sthenno/hermit--disable)
               (signal (car err) (cdr err))))
    (sthenno/hermit--disable))
  (setq sthenno/hermit--requested-file nil)
  (setq sthenno/hermit--requested-parent nil))

;;; Commands

;;;###autoload
(defun sthenno/hermit-start (&optional file parent)
  "Enable `sthenno/hermit-mode'.
Optional FILE and PARENT are used only for this start operation."
  (interactive)
  (setq sthenno/hermit--requested-file file)
  (setq sthenno/hermit--requested-parent parent)
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
  (let ((file (sthenno/hermit--get :file))
        (parent (sthenno/hermit--parent-frame)))
    (sthenno/hermit-mode -1)
    (setq sthenno/hermit--requested-file file)
    (setq sthenno/hermit--requested-parent parent)
    (sthenno/hermit-mode 1)))

;;;###autoload
(defun sthenno/hermit-hide ()
  "Hide the pet and bubble frames without disabling `sthenno/hermit-mode'."
  (interactive)
  (let ((bubble (sthenno/hermit--live-frame :bubble-frame)))
    (if bubble
        (make-frame-invisible bubble t)))
  (let ((pet (sthenno/hermit--live-frame :pet-frame)))
    (if pet
        (make-frame-invisible pet t))))

;;;###autoload
(defun sthenno/hermit-show ()
  "Show the pet frame and the active bubble frame."
  (interactive)
  (let ((pet (sthenno/hermit--live-frame :pet-frame)))
    (if pet
        (progn
          (make-frame-visible pet)
          (sthenno/hermit--refresh-placement)
          (raise-frame pet))))
  (let ((bubble (sthenno/hermit--live-frame :bubble-frame)))
    (if (and bubble (sthenno/hermit--get :bubble-visible))
        (progn
          (make-frame-visible bubble)
          (sthenno/hermit--place-bubble)
          (raise-frame bubble))))
  (sthenno/hermit--restore-focus))

;;;###autoload
(defun sthenno/hermit-iconify-parent ()
  "Iconify the parent Emacs frame."
  (interactive)
  (let ((parent (sthenno/hermit--parent-frame)))
    (if (and (framep parent) (frame-live-p parent))
        (iconify-frame parent))))

(provide 'init-gpt)
