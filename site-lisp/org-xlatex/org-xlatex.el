;; org-xlatex.el --- instant LaTeX preview in an xwidget  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  ksqsf

;; Author: ksqsf <justksqsf@gmail.com>
;; URL: https://github.com/ksqsf/org-xlatex
;; Keywords:
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org "9.6"))

;;; Commentary:

;; This package provides a minor mode `org-xlatex-mode'.  It provides
;; almost instant LaTeX previewing in Org buffers by embedding MathJax
;; in an xwidget inside a child frame.  The child frame automatically
;; appears and renders the formula at the point.

;; You can turn off and then turn on `org-xlatex-mode' to reset the
;; internal states, in case you run into problems.

;; org-xlatex is self-contained.  It does not require any external
;; programs.

;;; Code:

(require 'org)
(require 'xwidget)
(require 'pixel-scroll)

(eval-and-compile
  (unless (featurep 'xwidget-internal)
    (error "Your Emacs was not built with Xwidget support")))

(defgroup org-xlatex nil
  "Instant LaTeX preview using xwidget and mathjax"
  :group 'org
  :prefix "org-xlatex-")
(defcustom org-xlatex-width 400
  "The width of the preview window."
  :type 'integer
  :group 'org-xlatex)
(defcustom org-xlatex-height 200
  "The height of the preview window."
  :type 'integer
  :group 'org-xlatex)
(defcustom org-xlatex-position-indicator nil
  "Display an indicator for the current poisition in the preview."
  :type 'bool
  :group 'org-xlatex)
(defcustom org-xlatex-frame-adaptive-size t
  "Automatically adjust the width and/or the height of the preview frame when necessary."
  :type 'bool
  :group 'org-xlatex)

(defvar org-xlatex-timer nil)
(defvar org-xlatex-frame nil
  "The child frame used by org-xlatex.")
(defvar org-xlatex-xwidget nil
  "The xwidget used by org-xlatex.")
(defconst org-xlatex-html-uri (concat "file://" (expand-file-name "org-xlatex.html" (file-name-directory (or load-file-name buffer-file-name)))))
(defvar org-xlatex-last-latex)
(defvar org-xlatex-last-js)
(defvar org-xlatex-last-frame)

(defvar org-xlatex-frame-parameters
  '((left . -1)
    (top . -1)
    (width . 0)
    (height . 0)

    (no-accept-focus . t)
    (no-focus-on-map . t)
    (skip-taskbar . t)
    (min-width . 0)
    (min-height . 0)
    (internal-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (right-fringe . 0)
    (left-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (line-spacing . 0)
    (unsplittable . t)
    (undecorated . t)
    (visibility . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (minibuffer . nil)
    (desktop-dont-save . t))
  "The default frame parameters to create the frame.")

;;;###autoload
(define-minor-mode org-xlatex-mode
  "Toggle org-xlatex-mode.
Interactively with no argument, this command tggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When org-xlatex-mode is enabled, a child frame appears with the
preview of the mathematical formula (LaTeX math formula) whenevr
the point is at a formula."
  :init-value nil
  :lighter " XLaTeX"
  :group 'org-xlatex
  (if org-xlatex-mode
      (org-xlatex--setup)
    (org-xlatex--teardown)))

(defun org-xlatex--setup ()
  "Arrange the org-xlatex frame to be displayed when the point enters LaTeX fragments or environments."
  (org-xlatex--cleanup)
  (add-hook 'after-delete-frame-functions 'org-xlatex--after-delete-frame-function)
  (when (and org-xlatex-timer (timerp org-xlatex-timer))
    (cancel-timer org-xlatex-timer))
  (setq org-xlatex-timer (run-with-idle-timer 0.1 'repeat 'org-xlatex--timer-function)))

(defun org-xlatex--teardown ()
  "Disable hooks and timers set up by org-xlatex."
  (cancel-timer org-xlatex-timer)
  (setq org-xlatex-timer nil)
  (remove-hook 'after-delete-frame-functions 'org-xlatex--after-delete-frame-function)
  (org-xlatex--cleanup))

(defun org-xlatex--timer-function (&rest _ignored)
  "Preview at point if the point is at a math formula."
  (if (and (or (and (derived-mode-p 'org-mode)
                    org-xlatex-mode)
               ;; for org-edit-special
               (and (string-match "\\*Org Src.*" (buffer-name))
                    (or (derived-mode-p 'latex-mode)
                        (derived-mode-p 'LaTeX-mode))))
           (org-inside-LaTeX-fragment-p))
      (org-xlatex-preview)
    (org-xlatex--hide)))

(defun org-xlatex--after-delete-frame-function (frame)
  "Check if the newly deleted frame was org-xlatex."
  (when (eq frame org-xlatex-frame)
    (org-xlatex--cleanup)
    (setq org-xlatex-frame nil)))

(defun org-xlatex--ensure-frame ()
  "Get the current org-xlatex-frame; initialize one if it does not exist."
  (if (and org-xlatex-frame (frame-live-p org-xlatex-frame))
      org-xlatex-frame
    (org-xlatex--cleanup)
    (setq org-xlatex-frame (make-frame org-xlatex-frame-parameters))
    (with-selected-frame org-xlatex-frame
      (delete-other-windows)
      (switch-to-buffer " *org-xlatex*")
      (setq mode-line-format nil)
      (setq header-line-format nil)
      (when (bound-and-true-p global-tab-line-mode)
        (setq tab-line-format nil))
      (setq display-line-numbers nil)
      (set-window-dedicated-p nil t)
      (erase-buffer)
      (insert " ")
      (setq org-xlatex-xwidget (xwidget-insert (point-min) 'webkit "org-xlatex" org-xlatex-width org-xlatex-height))
      (xwidget-put org-xlatex-xwidget 'callback #'xwidget-webkit-callback)
      (xwidget-put org-xlatex-xwidget 'display-callback #'xwidget-webkit-display-callback)
      (xwidget-webkit-goto-uri org-xlatex-xwidget org-xlatex-html-uri))
    org-xlatex-frame))

(defun org-xlatex--cleanup ()
  "Release resources used by org-xlatex."
  (when (buffer-live-p " *org-xlatex*")
    (with-current-buffer " *org-xlatex*"
      (erase-buffer))
    (kill-buffer " *org-xlatex*"))
  (when (and org-xlatex-xwidget (xwidget-live-p org-xlatex-xwidget))
    (kill-xwidget org-xlatex-xwidget)
    (setq org-xlatex-xwidget nil))
  (when (and org-xlatex-frame (frame-live-p org-xlatex-frame))
    (delete-frame org-xlatex-frame)
    (setq org-xlatex-frame nil)))

(defun org-xlatex--latex-at-point ()
  "Obtain the contents of the LaTeX fragment or environment at point."
  (let ((context (org-element-context)))
    (when (or (eq 'latex-fragment (org-element-type context))
              (eq 'latex-environment (org-element-type context)))
      (let ((beg (org-element-property :begin context))
            (end (- (org-element-property :end context)
	            (org-element-property :post-blank context))))
        (if org-xlatex-position-indicator
            (concat (buffer-substring-no-properties beg (point))
                    "{\\color{red}|}"
                    (buffer-substring-no-properties (point) end))
          (buffer-substring-no-properties beg end))))))

(defun org-xlatex--escape (latex)
  "Escape LaTeX code so that it can be used as JS strings."
  (string-replace "\n" " " (string-replace "'" "\\'" (string-replace "\\" "\\\\" latex))))

(defun org-xlatex--build-js (latex)
  "Build js code to rewrite the DOM and typeset LATEX."
  (let ((template "oxlTypeset('%s');"))
    (format template (org-xlatex--escape latex))))

(defun org-xlatex--resize (w h)
  "Resize both the xwidget and its container."
  (set-frame-size org-xlatex-frame w h t)
  (xwidget-resize org-xlatex-xwidget w h))

(defun org-xlatex--expose (parent-frame)
  "Expose the child frame."
  (set-frame-parameter org-xlatex-frame 'parent-frame parent-frame)
  (make-frame-visible org-xlatex-frame)
  (select-frame parent-frame)
  (if (not org-xlatex-frame-adaptive-size)
      (org-xlatex--resize org-xlatex-width org-xlatex-height)
    ;; Adaptively set frame size
    (xwidget-webkit-execute-script org-xlatex-xwidget "oxlSize();"
                                   #'(lambda (size)
                                       (let* ((w0 (frame-width org-xlatex-frame))
                                              (h0 (frame-height org-xlatex-frame))
                                              (w1 (ceiling (aref size 0)))
                                              (h1 (ceiling (aref size 1)))
                                              (w (max org-xlatex-width w0 w1))
                                              (h (max org-xlatex-height h0 h1)))
                                         (org-xlatex--resize w h)))))
  (with-selected-frame parent-frame
    (let* ((context (org-element-context))
           (latex-beg (org-element-property :begin context))
           (latex-end (- (org-element-property :end context)
                         (org-element-property :post-blank context)))
           (latex-beg-posn (posn-at-point latex-beg))
           (latex-beg-x (car (posn-x-y latex-beg-posn)))
           (latex-end-posn (posn-at-point latex-end))
           (latex-end-y (or (cdr (posn-x-y latex-end-posn))
                            (cdr (posn-x-y (posn-at-point)))))
           (y (+ (* 2 (pixel-line-height)) latex-end-y)))
      (set-frame-position org-xlatex-frame latex-beg-x y))))

(defun org-xlatex--hide ()
  (when (and org-xlatex-frame (frame-live-p org-xlatex-frame))
    (make-frame-invisible org-xlatex-frame)))

(defun org-xlatex--update (latex)
  (org-xlatex--ensure-frame)
  (setq org-xlatex-last-latex latex)
  (setq org-xlatex-last-js (org-xlatex--build-js latex))
  (xwidget-webkit-execute-script org-xlatex-xwidget org-xlatex-last-js))

(defun org-xlatex-preview ()
  "Preview the LaTeX formula inside a child frame at the point.

This function should only be used from `org-xlatex-mode'.  This
is due to MathJax's asynchronous typesetting process: sometimes
the first few typesetting requests are ignored (during the
initialization of mathjax).  Therefore, if you directly call
this, chances are you will see a blank preview."
  (setq org-xlatex-last-frame (selected-frame))
  (org-xlatex--ensure-frame)
  (when-let ((latex (org-xlatex--latex-at-point)))
    (org-xlatex--update latex)
    (org-xlatex--expose org-xlatex-last-frame)))

(defun org-xlatex--reset-frame ()
  (org-xlatex--cleanup)
  (org-xlatex--ensure-frame))
(with-eval-after-load 'tab-bar
  (add-hook 'tab-bar-mode-hook #'org-xlatex--reset-frame))
(with-eval-after-load 'tab-line
  (add-hook 'tab-line-mode-hook #'org-xlatex--reset-frame))

(provide 'org-xlatex)
;;; org-xlatex.el ends here
