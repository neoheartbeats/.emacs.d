;;; init-org.el --- Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This config is currently for a patched version of Org that is under development.
;; See https://code.tecosaur.net/tec/org-mode for more details.
;;
;; This file includes:
;;
;; - Org Mode basics
;; - TEC's `org-latex-preview' specific configurations
;; - Modern Org Mode
;; - Note-taking system using `denote'

;;; Code:

;;; Setup default directory
(setq org-directory user-note-directory)

;;; Org Mode buffer init behaviors
(setq org-startup-with-link-previews t
      org-startup-with-latex-preview t)

;; Fold titles by default
;; (setq org-startup-folded 'content)

;;; Install AUCTeX
(use-package tex :ensure auctex)

;;; Use CDLaTeX to improve editing experiences
(use-package cdlatex
  :ensure t
  :diminish (org-cdlatex-mode)
  :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

;; Default LaTeX preview image directory
(setq org-preview-latex-image-directory
      (expand-file-name "ltximg/" user-cache-directory))

(setq org-persist-directory (expand-file-name "org-persist" user-cache-directory))

;;; Experimental: `org-latex-preview'
(add-hook 'org-mode-hook #'org-latex-preview-auto-mode)

;; Preview functions
(defun sthenno/org-preview-fragments ()
  (interactive)
  (call-interactively 'org-latex-preview-clear-cache)
  (org-latex-preview 'buffer)
  (org-link-preview-refresh))
(bind-keys :map org-mode-map
           ("C-c p" . sthenno/org-preview-fragments))

;; Setup for `org-latex-preview'
(setq org-latex-packages-alist '(("T1" "fontenc" t)
                                 ("" "amsmath"   t)
                                 ("" "amssymb"   t)
                                 ("" "siunitx"   t)

                                 ;; Font packages
                                 ("libertinus" "newtx" t)

                                 ;; Load this after all math to give access to bold math
                                 ;; See https://ctan.org/pkg/newtx
                                 ("" "bm" t)

                                 ;; Package physics2 requires to be loaded after font
                                 ;; packages. See https://ctan.org/pkg/physics2
                                 ("" "physics2" t)

                                 ;; Differentiations
                                 ("normal" "fixdif" t)))

;; Add additional modules required by LaTeX packages like physics2 to the preamble

(defun sthenno/org-latex-preview-preamble-setup ()
  (require 'org-latex-preview)
  (let* ((physics2-modules '(("" "ab")
                             ("" "diagmat")
                             ("" "xmat")))
         (physics2-preamble (mapconcat
                             (lambda (m)
                               (let ((options (car  m))
                                     (module  (cadr m)))
                                 (if (string= options "")
                                     (format "\\usephysicsmodule{%s}" module)
                                   (format "\\usephysicsmodule[%s]{%s}" options module))))
                             physics2-modules
                             "\n")))
    (unless (and org-latex-preview-preamble
                 (string-match
                  (regexp-quote physics2-preamble) org-latex-preview-preamble))
      (setq org-latex-preview-preamble (concat (or org-latex-preview-preamble "")
                                               "\n" physics2-preamble
                                               "\\DeclareMathOperator*{\\argmax}{arg\\,max}"
                                               "\\DeclareMathOperator*{\\argmin}{arg\\,min}")))))
(add-hook 'after-init-hook #'sthenno/org-latex-preview-preamble-setup)

(setq org-highlight-latex-and-related '(native)) ; Highlight inline LaTeX code
(setq org-use-sub-superscripts '{})

;; (let ((factor (- (/ (face-attribute 'default :height)
;;                     100.0)
;;                  0.25)))
;;   (plist-put org-latex-preview-appearance-options :scale factor)
;;   (plist-put org-latex-preview-appearance-options :zoom  factor))

(setq org-latex-preview-process-default 'dvisvgm)
(let ((dvisvgm (alist-get 'dvisvgm org-latex-preview-process-alist))
      (libgs "/opt/homebrew/opt/ghostscript/lib/libgs.dylib"))
  (plist-put dvisvgm :image-converter
             `(,(concat "dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts"
                        " --libgs=" libgs

                        ;; Default is "-v3" here, but it does not seem to work correctly
                        ;; on my client.
                        " --bbox=preview -v4 -o %B-%%9p.svg %f"))))

;;; Modern Org Mode theme
(use-package org-modern
  :ensure t
  :config
  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X  . "􀃰")
                              (?-  . "􀃞")
                              (?\s . "􀂒")))
  (setq org-modern-timestamp '(" %Y-%m-%d " . " %H:%M "))
  (setq org-modern-block-fringe nil)
  (setq org-modern-block-name
        '((t . t)
          ("src" .  ("􀃥" "_"))
          ))
  ;; Hooks
  (add-hook 'org-mode-hook #'org-modern-mode))

(defun sthenno/org-modern-spacing ()
  "Adjust line-spacing for `org-modern' to correct svg display."
  (setq-local line-spacing (cond ((eq major-mode #'org-mode) 0.20)
                                 (t nil))))
(add-hook 'org-mode-hook #'sthenno/org-modern-spacing)

;; External settings for `org-modern'
(setq org-ellipsis "…")
(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

(setq org-use-property-inheritance t)
(setq org-auto-align-tags nil)
(setq org-tags-column 0)
(setq org-hide-emphasis-markers t)

;; Use this with "C-<return>"
(setq org-insert-heading-respect-content t)

;; Use this with "C-S-<return>"
(setq org-treat-insert-todo-heading-as-state-change t)

;; Better experiences jumping through headlines
(setq org-special-ctrl-a/e t)

;; Fold drawers by default
(setq org-cycle-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-fold-hide-drawer-all)

;;; Org fragments and overlays

;; Org images

(setq org-image-align 'left
      org-image-actual-width '(420)
      org-image-max-width 'fill-column)
(setq org-yank-dnd-method 'file-link)
(setq org-yank-image-save-method (expand-file-name "images/" org-directory))

;;; Org Hyperlinks
(setq org-return-follows-link t)

;;; Open file links in current window
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;;; Using shift-<arrow-keys> to select text
(setq org-support-shift-select 'always) ; Everywhere except timestamps
(setq org-use-fast-todo-selection 'expert)

;;; The Zettlekasten note-taking system by Denote
(use-package denote
  :ensure t
  :config
  (setq denote-directory org-directory) ; Use `org-directory' as default
  (setq denote-file-type 'org)
  (setq denote-known-keywords '("silos" "papers" "production" "static" "marked")
        denote-infer-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-history-completion-in-prompts t)
  (setq denote-save-buffers t
        denote-kill-buffers t)

  (setq denote-open-link-function 'find-file)

  ;; Do not include date, tags and ids in note files
  (setopt denote-org-front-matter "#+title: %1$s\n\n")

  ;; Automatically rename Denote buffers when opening them so that instead of their long
  ;; file name they have a literal "[D]" followed by the file's title.  Read the doc
  ;; string of `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)

  (setq denote-rename-buffer-format "[D] %T%b")

  ;; Do not issue any extra prompts. Always sort by the `title' file name component and
  ;; never do a reverse sort.
  (setq denote-sort-dired-extra-prompts nil)
  (setq denote-sort-dired-default-sort-component 'title)
  (setq denote-sort-dired-default-reverse-sort t)

  ;; The `denote-rename-buffer-mode' can now show if a file has backlinks
  (setq denote-rename-buffer-backlinks-indicator "↔")

  ;; Hooks
  (add-hook 'dired-mode-hook #'denote-dired-mode)

  :bind ((:map global-map
               ("s-o" . denote-open-or-create))
         (:map org-mode-map
               ("s-l" . denote-link-or-create)
               ("s-i" . denote-insert-link))))

(use-package denote-journal
  :ensure t
  :config
  (setq denote-journal-title-format "%Y-%m-%d") ; Format yyyy-mm-dd
  (setq denote-journal-directory (expand-file-name "stages/" user-note-directory))
  (setq denote-journal-keyword '("stages")) ; Stages are journals

  :bind ((:map global-map
               ("C-c d" . denote-journal-new-or-existing-entry))))

(use-package denote-org
  :ensure t
  :config
  ;; Org subtrees
  (setq denote-org-store-link-to-heading 'context))

;; Do not include date, tags and ids in note files
;; (setq denote-org-front-matter "#+title: %1$s\n\n"))

;; Bookmarks
;; (setq bookmark-use-annotations nil)

;; (setq bookmark-bmenu-type-column-width 2)
;; (setq bookmark-bmenu-toggle-filenames nil)
;; (setq bookmark-menu-length 45)

;; (setq bookmark-default-file
;;       (expand-file-name ".bookmarks.bmk" org-directory))
;; (setq bookmark-watch-bookmark-file 'silent)

;; (defvar parameters
;;   '(window-parameters . ((no-other-window . t)
;;                          (no-delete-other-windows . t))))
;; (setq fit-window-to-buffer-horizontally t)
;; (setq window-resize-pixelwise t)

;; (defun sthenno/bookmark-bmenu-on-left ()
;;   (let ((buffer (bookmark-bmenu-get-buffer)))
;;     (display-buffer-in-side-window
;;      buffer `((side . left) (slot . -1)
;;               (window-width . fit-window-to-buffer)
;;               (window-width . 0.25)
;;               (window-height . 20)
;;               (preserve-size . (nil . t))
;;               (dedicated . t) ,parameters))))

;; (defun sthenno/denote-buffer-on-buttom ()
;;   (let ((buffer (bookmark-bmenu-get-buffer)))
;;     (display-buffer-in-side-window
;;      buffer `((side . left) (slot . 1)
;;               ;; (window-width . fit-window-to-buffer)
;;               ;; (window-width . 0.25)
;;               (preserve-size . (nil . t))
;;               (dedicated . t) ,parameters))))

(defun sthenno/denote-mark-buffer ()
  (interactive)
  (let* ((marked-text "marked")
         (file (buffer-file-name (current-buffer)))
         (buff (buffer-name (current-buffer)))
         (keys (denote-extract-keywords-from-path file))
         (marked-keys (cons marked-text keys))
         (denote-rename-confirmations nil))
    (denote-rename-file file 'keep-current marked-keys 'keep-current nil)))

;; (defun sthenno/bookmark-set-buffer-name ()
;;   (interactive)
;;   (let ((name (buffer-name (current-buffer))))
;;     (bookmark-set name nil)))

;;; windows
;; (setq
;;  display-buffer-alist
;;  `(("\\*Buffer List\\*" display-buffer-in-side-window
;;     (side . top) (slot . 0) (window-height . fit-window-to-buffer)
;;     (preserve-size . (nil . t)) ,parameters)
;;    ("\\*Tags List\\*" display-buffer-in-side-window
;;     (side . right) (slot . 0) (window-width . fit-window-to-buffer)
;;     (preserve-size . (t . nil)) ,parameters)
;;    ("\\*\\(?:help\\|grep\\|Completions\\)\\*"
;;     display-buffer-in-side-window
;;     (side . bottom) (slot . -1) (preserve-size . (nil . t))
;;     ,parameters)
;;    ("\\*\\(?:shell\\|compilation\\)\\*" display-buffer-in-side-window
;;     (side . bottom) (slot . 1) (preserve-size . (nil . t))
;;     ,parameters)))

;; (setq window-sides-slots '(2 0 1 1))

;; (setq display-buffer-alist
;;       '(((lambda (buf _)
;;            (with-current-buffer buf
;;              (and buffer-file-name
;;                   (boundp 'denote-directory)
;;                   (string-prefix-p (expand-file-name denote-directory)
;;                                    (file-name-directory buffer-file-name)))))
;;          (display-buffer-reuse-window
;;           display-buffer-same-window)
;;          (reusable-frames . nil)
;;          (inhibit-same-window . nil)
;;          (frame-predicate . (lambda (frame)
;;                               (string-match-p "Notes" (frame-parameter frame 'name)))))))


;;; Keys

(defun sthenno/get-sorted-note-files (directory)
  "Return a list of note files in DIRECTORY, sorted by name."
  (sort (seq-filter 'denote-file-is-note-p
                    (directory-files directory t "\\`[^.]"))
        'string<))

(defun sthenno/denote-open-adjacent-file (direction)
  "Open the adjacent note file in the given DIRECTION (-1 for previous, +1 for next)."
  (let* ((current-file (buffer-file-name))
         (directory (file-name-directory current-file))
         (sorted-files (sthenno/get-sorted-note-files directory))
         (current-file-index (cl-position current-file sorted-files :test 'string=)))
    (if (null current-file-index)
        (message "Current file is not a note file.")
      (let ((adjacent-index (+ current-file-index direction)))
        (if (or (< adjacent-index 0)
                (>= adjacent-index (length sorted-files)))
            (message "No adjacent note file.")
          (find-file (nth adjacent-index sorted-files)))))))

(defun sthenno/denote-open-previous-file ()
  "Open the previous note file in the current directory."
  (interactive)
  (sthenno/denote-open-adjacent-file -1))

(defun sthenno/denote-open-next-file ()
  "Open the next note file in the current directory."
  (interactive)
  (sthenno/denote-open-adjacent-file +1))

(bind-keys :map org-mode-map
           ("s-<up>"   . sthenno/denote-open-previous-file)
           ("s-<down>" . sthenno/denote-open-next-file))

;;; Load languages for Org Babel

;; Do not ask for confirmation before executing
(setq org-link-elisp-confirm-function nil
      org-link-shell-confirm-function nil)

;; Org code blocks
(setq org-confirm-babel-evaluate nil)

(setq org-src-preserve-indentation t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-src-ask-before-returning-to-edit-buffer nil)

;; In addition to `org-src-fontify-natively'
(add-to-list 'org-src-lang-modes (cons "python" 'python))

(setq org-edit-src-turn-on-auto-save t)

;; Enable these languages for Org-Babel

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (python . t)))

(provide 'init-org)

;;; init-org.el ends here
