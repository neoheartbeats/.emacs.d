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
(setq org-directory "~/Developer/sthenno-notebook/")

;;; Org Mode buffer init behaviors
(setq org-startup-with-inline-images t
      org-startup-with-latex-preview t)

;; Fold titles by default
;; (setq org-startup-folded 'content)

;;; Install AUCTeX
(use-package tex :ensure auctex)

;;; Use CDLaTeX to improve editing experiences

;; This is temporarily disabled and replaced by `tempel'

;; (use-package cdlatex
;;   :ensure t
;;   :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

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
  (org-redisplay-inline-images))
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
                                               "\n" physics2-preamble)))))

(add-hook 'after-init-hook #'sthenno/org-latex-preview-preamble-setup)

(setq org-highlight-latex-and-related '(native)) ; Highlight inline LaTeX code
(setq org-use-sub-superscripts '{})

(let ((factor (- (/ (face-attribute 'default :height)
                    100.0)
                 0.25)))
  (plist-put org-latex-preview-appearance-options :scale factor)
  (plist-put org-latex-preview-appearance-options :zoom  factor))

(setopt org-latex-preview-process-default 'dvisvgm)
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
  (setopt org-modern-star 'fold
          ;; org-modern-fold-stars '(("◉" . "○"))
          org-modern-hide-stars 'leading)

  (setopt org-modern-list '((?- . "•")))
  (setopt org-modern-checkbox '((?X  . "􀃰")
                                (?-  . "􀃞")
                                (?\s . "􀂒")))

  ;; (setopt org-modern-block-name '(("src"    . ("􀃤" "􀂓"))
  ;;                                 ("quote"  . ("􀈎" "􀂓"))
  ;;                                 ("export" . ("􀣙" "􀂓"))))

  ;; (setopt org-modern-todo nil)

  (setopt org-modern-keyword '(("title"   . "􀉛")
                               ("results" . "􂨖")
                               (t . t)))

  (setopt org-modern-timestamp '(" %Y-%m-%d " . " %H:%M "))

  ;; (defun sthenno/org-modern-spacing ()
  ;;   "Adjust line-spacing for `org-modern' to correct svg display."

  ;;   ;; FIXME: This may not set properly
  ;;   (setq-local line-spacing (cond ((eq major-mode #'org-mode) 0.20)
  ;;                                  (t nil))))
  ;; (add-hook 'org-mode-hook #'sthenno/org-modern-spacing)

  ;; (defun sthenno/org-modern-checkbox ()
  ;;   (modus-themes-with-colors
  ;;     (custom-set-faces
  ;;      `(org-checkbox ((t (:foreground ,prose-todo)))))))
  ;; (add-hook 'org-mode-hook #'sthenno/org-modern-checkbox)

  ;; Hooks
  (add-hook 'org-mode-hook #'org-modern-mode))

;; External settings for `org-modern'
(setq org-ellipsis "")                  ; …
;; (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

(setq org-use-property-inheritance t)
(setq org-auto-align-tags nil)
(setq org-tags-column 0)
(setq org-hide-emphasis-markers nil)

;; Always add the checkbox when `org-insert-item' is called
;; (define-advice org-insert-item
;;     (:filter-args (args) sthenno/use-checkbox)
;;   "Force CHECKBOX to t."
;;   (list t))

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

(setopt org-image-align 'center
        org-image-actual-width t
        org-image-max-width 0.4
        org-display-remote-inline-images 'cache)

(defun sthenno/org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these conventions:

  1. Its path is a file with an extension matching return value from
     `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous type. In
     this case, that link must be a well-formed plain or angle link,
     i.e., it must have an explicit \"file\" or \"attachment\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with a text
description part will be inlined. This can be nice for a quick look at
those images, but it does not reflect what exported files will look
like.

When optional argument REFRESH is non-nil, refresh existing images
between BEG and END. This will create new image displays only if
necessary.

BEG and END define the considered part. They default to the buffer
boundaries with possible narrowing."
  (interactive "P")
  (when (display-graphic-p)
    (when refresh
      (org-remove-inline-images beg end)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))
        (let* ((case-fold-search t)
               (file-extension-re (image-file-name-regexp))
               (link-abbrevs (mapcar #'car
                                     (append org-link-abbrev-alist-local
                                             org-link-abbrev-alist)))
               (file-types-re
                (format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]
\\[\\(<?\\(?:file\\|attachment\\):\\)"
                        (if (not link-abbrevs) ""
                          (concat "\\|" (regexp-opt link-abbrevs))))))
          (while (re-search-forward file-types-re end t)
            (let* ((link (org-element-lineage
                          (save-match-data (org-element-context))
                          'link t))
                   (linktype (org-element-property :type link))
                   (inner-start (match-beginning 1))
                   (path
                    (cond
                     ((not link) nil)
                     ((or (not (org-element-contents-begin link))
                          include-linked)
                      (and (or (equal "file" linktype)
                               (equal "attachment" linktype))
                           (org-element-property :path link)))
                     ((not inner-start) nil)
                     (t
                      (org-with-point-at inner-start
                        (and (looking-at
                              (if (char-equal ?< (char-after inner-start))
                                  org-link-angle-re
                                org-link-plain-re))
                             (= (org-element-contents-end link)
                                (match-end 0))
                             (progn
                               (setq linktype (match-string 1))
                               (match-string 2))))))))
              (when (and path (string-match-p file-extension-re path))
                (let ((file (if (equal "attachment" linktype)
                                (progn
                                  (require 'org-attach)
                                  (ignore-errors (org-attach-expand path)))
                              (expand-file-name path))))
                  (when file (setq file (substitute-in-file-name file)))
                  (when (and file (file-exists-p file))
                    (let ((width (org-display-inline-image--width link))
                          (align (org-image--align link))
                          (old (get-char-property-and-overlay
                                (org-element-begin link)
                                'org-image-overlay)))
                      (if (and (car-safe old) refresh)
                          (image-flush (overlay-get (cdr old) 'display))
                        (let ((image (org--create-inline-image file width)))
                          (when image
                            (let ((ov (make-overlay
                                       (org-element-begin link)
                                       (progn
                                         (goto-char
                                          (org-element-end link))
                                         (unless (eolp) (skip-chars-backward " \t"))
                                         (point)))))
                              (image-flush image)
                              (overlay-put ov 'display image)
                              (overlay-put ov 'face 'default)
                              (overlay-put ov 'org-image-overlay t)
                              (overlay-put
                               ov 'modification-hooks
                               (list 'org-display-inline-remove-overlay))
                              (when (boundp 'image-map)
                                (overlay-put ov 'keymap image-map))
                              (when align

                                ;; Use fill-column to indicate the right-edge to display
                                (let ((edge fill-column))
                                  (overlay-put
                                   ov 'before-string
                                   (propertize
                                    " " 'face 'default
                                    'display
                                    (pcase align

                                      ;; Apply the edge for alignments
                                      ("center" `(space :align-to (- (0.50 . ,edge)
                                                                     (0.50 . ,image))))
                                      ("right"  `(space :align-to (- ,edge
                                                                     ,image))))))))
                              (push ov org-inline-image-overlays))))))))))))))))
(advice-add #'org-display-inline-images :override #'sthenno/org-display-inline-images)

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
  :init

  ;; Hooks
  (add-hook 'emacs-startup-hook #'denote-journal-extras-new-or-existing-entry)
  (add-hook 'dired-mode-hook #'denote-dired-mode)

  :config
  (setopt denote-directory org-directory) ; Use `org-directory' as default
  (setopt denote-file-type 'org)
  (setopt denote-known-keywords '("stages" "silo" "research" "production")
          denote-infer-keywords t)
  (setopt denote-prompts '(title keywords))
  (setopt denote-save-buffers t
          denote-kill-buffers 'on-creation)

  ;; Denote for journals
  (setopt denote-journal-extras-directory
          (expand-file-name "stages/" denote-directory)) ; Sub-directory for journal files
  (setopt denote-journal-extras-keyword "stages")        ; Stages are journals
  (setopt denote-journal-extras-title-format "%F")       ; Use ISO 8601 for titles

  ;; Do not include date, tags and ids in note files
  (setopt denote-org-front-matter "#+title: %1$s\n\n")

  ;; Automatically rename Denote buffers when opening them so that instead of their long
  ;; file name they have a literal "[D]" followed by the file's title.  Read the doc
  ;; string of `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)

  (setopt denote-rename-buffer-format "[D] %t%b")

  ;; Do not issue any extra prompts. Always sort by the `title' file name component and
  ;; never do a reverse sort.
  (setq denote-sort-dired-extra-prompts nil)
  (setq denote-sort-dired-default-sort-component 'title)
  (setq denote-sort-dired-default-reverse-sort t)

  ;; Creating and linking notes
  (defun sthenno/denote-link-or-create ()
    "Improved note creation and linking for Denote."
    (interactive)
    (let ((denote-prompts nil)
          (denote-kill-buffers nil))
      (cond
       ((use-region-p)
        (let* ((denote-ignore-region-in-denote-command t)
               (path (buffer-substring-no-properties (region-beginning)
                                                     (region-end)))
               (regexp (format "--%s" path))
               (target (car (denote-directory-files regexp :omit-current))))
          (if target
              (progn
                (denote-link target
                             (denote-filetype-heuristics (buffer-file-name))
                             (denote--link-get-description target))
                (save-buffer)
                (message "Note linked to: %s" target))
            (progn
              (save-window-excursion
                (setq target (denote path '("notes"))))
              (denote-link target
                           (denote-filetype-heuristics (buffer-file-name))
                           (denote--link-get-description target))
              (save-buffer)
              (find-file target)
              (message "Note created at: %s" target)))))
       ((and (not (use-region-p))
             (org-at-heading-p))
        (denote-org-extras-extract-org-subtree)
        (save-buffer))
       (t
        (call-interactively #'denote-link-or-create))))
    (run-hooks 'sthenno/denote-link-or-create-hook))

  ;; The `denote-rename-buffer-mode' can now show if a file has backlinks
  (setopt denote-rename-buffer-backlinks-indicator " 􀄾")

  ;; Org subtrees
  (setopt denote-org-store-link-to-heading 'context)

  :bind ((:map global-map
               ("C-c o" . denote-open-or-create)
               ("C-c d" . denote-journal-extras-new-or-existing-entry))
         (:map org-mode-map
               ("s-l" . sthenno/denote-link-or-create)
               ("s-i" . denote-insert-link)
               ("s-b" . denote-backlinks)
               ("s-r" . denote-region))))

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

;;; _
(provide 'init-org)
