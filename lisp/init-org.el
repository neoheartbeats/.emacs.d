;;; init-org.el --- Org Mode -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This configuration only supports TEC's Org-mode develop branch due
;; to the use of `org-latex-preview.el'. The "Org LaTeX customizations"
;; part is unstable and underdevelopment.

;;; Code:

;; Setup default directory
(setq org-directory "~/Sthenno/")

;; Org Mode buffer init behaviors
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)

;; Install AUCTeX. This is required by TEC's Org
;; (use-package tex
;;   :straight auctex)
(straight-use-package 'auctex)


;; Modern Org Mode theme
(use-package org-modern
  :straight t
  :init
  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X . "􀃰") (?- . "􀃞") (?\s . "􀂒")))
  (setq org-modern-progress '("􀛪" "􀛩" "􀺶" "􀺸" "􀛨"))
  (setq org-modern-table-vertical 2)
  (setq org-modern-tag nil)
  (setq org-modern-block-name nil)
  (setq org-modern-keyword nil)
  (setq org-modern-block-fringe nil)
  :config (global-org-modern-mode 1))

;; External settings for `org-modern'
(setq org-ellipsis " 􀍠")
(setq org-hide-emphasis-markers t)
(setq org-auto-align-tags nil)
(setq org-tags-column 0)

;; Use this with `C-<return>'
(setq org-insert-heading-respect-content t)

;; Use this with `C-S-<return>'
(setq org-treat-insert-todo-heading-as-state-change t)

;; Prevent editing of text within folded subtree
(setq org-catch-invisible-edits 'show-and-error)

;; Better experiences jumping through headlines
(setq org-special-ctrl-a/e t)

;; Using the SF Pro font for symbols
(defun my/iconify-org-buffer ()
  (progn
    (push '("#+title:     " . ?􀈭) prettify-symbols-alist)
    (push '("#+identifier:" . ?􀅷) prettify-symbols-alist)
    (push '("#+date:      " . ?􀧵) prettify-symbols-alist)
    (push '("#+filetags:  " . ?􀋡) prettify-symbols-alist)
    (push '("#+begin_src"   . ?􀃤) prettify-symbols-alist)
    (push '("#+end_src" . ?􀅽) prettify-symbols-alist)
    (push '("#+begin_quote" . ?􀙤) prettify-symbols-alist)
    (push '("#+end_quote" . ?􀅽) prettify-symbols-alist)
    (push '("#+BEGIN: denote-links" .?􀋲) prettify-symbols-alist)
    (push '("#+END:" . ?􀅽) prettify-symbols-alist)
    (push '("#+RESULTS:" . ?􀎚) prettify-symbols-alist)
    (push '("SCHEDULED:" .?􀧞) prettify-symbols-alist)
    (push '("#+attr_org:" . ?􀌞) prettify-symbols-alist))
  (prettify-symbols-mode 1))
(add-hook 'org-mode-hook #'my/iconify-org-buffer)

;; Fold drawers by default
(setq org-cycle-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-fold-hide-drawer-all)

;; Fold titles by default
(setq org-startup-folded 'content)

;; Org fragments and overlays
(setq org-image-actual-width '(420))

;;; Org links
(setq org-return-follows-link t)

;; Open file links in current window
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;; Using shift-<arrow-keys> to select text
(setq org-support-shift-select t)


;; The Zettlekasten note-taking system by Denote
(use-package denote
  :straight t
  :config
  (setq denote-directory org-directory) ; Use `org-directory' as default
  (setq denote-known-keywords '("robots"
				"poem"
				"sciences"
				"dust"
				"business"
				"billings"))
  (setq denote-save-buffer-after-creation t)
  
  ;; Denote for journaling
  (setq denote-journal-extras-directory
        (expand-file-name "stages/" denote-directory)) ; Subdirectory for journal files
  (setq denote-journal-extras-keyword "stages") ; Stages are journals
  (setq denote-journal-extras-title-format 'day-date-month-year)

  ;; Do not include date in notes
  (setq denote-org-front-matter
        "#+title:      %1$s
#+filetags:   %3$s
#+identifier: %4$s
\n")

  ;; No need confirmation using `denote-rename-file'
  (setq denote-rename-no-confirm t)
  :bind
  (:map global-map

        ;; Open today's note
        ("C-c d" . denote-journal-extras-new-or-existing-entry))
  (:map org-mode-map
        ("C-c i" . denote-link-or-create)
	("C-c b" . denote-backlinks)
	("C-c e" . denote-org-extras-extract-org-subtree)
	("C-c k a" . denote-keywords-add)
	("C-c k r" . denote-keywords-remove))
  :hook (after-init . denote-journal-extras-new-or-existing-entry))

;; Extensions for Denote
(use-package denote-menu
  :straight t
  :config
  (setq denote-menu-title-column-width 45)

  ;; Remove denote journal entries from the menu
  (setq denote-menu-initial-regex
	(mapconcat (lambda (keyword) (concat "_" keyword))
		   denote-known-keywords "\\|"))
  :bind (:map org-mode-map
	      ("C-c m" . list-denotes)))

;; Custom functions for Denote
(defun my/denote-insert-links-current-month ()
  (interactive)
  (denote-add-links (format-time-string "%B")))

(defun my/denote-open-previous-file ()
  (interactive)
  (let* ((current-file (buffer-file-name))
         (directory (file-name-directory current-file))
         (files (directory-files directory t "\\`[^.]"))
         (sorted-files (sort files 'string<))
         (current-file-index (cl-position current-file sorted-files :test 'string=)))

    (when (and current-file-index (> current-file-index 0))
      (find-file (nth (1- current-file-index) sorted-files)))))

(defun my/denote-open-next-file ()
  (interactive)
  (let* ((current-file (buffer-file-name))
         (directory (file-name-directory current-file))
         (files (directory-files directory t "\\`[^.]"))
         (sorted-files (sort files 'string<))
         (current-file-index (cl-position current-file sorted-files :test 'string=)))

    (when (and current-file-index (< current-file-index (1- (length sorted-files))))
      (find-file (nth (1+ current-file-index) sorted-files)))))

(bind-keys :map org-mode-map
           ("s-<up>" . my/denote-open-previous-file)
           ("s-<down>" . my/denote-open-next-file))


;; Org LaTeX customizations
;; (setq org-latex-compiler "lualatex")
(setq org-latex-preview-process-default 'dvisvgm)
(setq org-latex-preview-process-alist
      '((dvipng
	 :programs ("latex" "dvipng")
	 :description "dvi > png"
	 :message "you need to install the programs: latex and dvipng."
	 :image-input-type "dvi"
	 :image-output-type "png"
	 :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
	 :latex-precompiler ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
	 :image-converter ("dvipng --follow -D %D -T tight --depth --height -o %B-%%09d.png %f")
	 :transparent-image-converter
	 ("dvipng --follow -D %D -T tight -bg Transparent --depth --height -o %B-%%09d.png %f"))
	(dvisvgm
	 :programs ("latex" "dvisvgm")
	 :description "dvi > svg"
	 :message "you need to install the programs: latex and dvisvgm."
	 :image-input-type "dvi"
	 :image-output-type "svg"
	 :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
	 :latex-precompiler ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
	 :image-converter ; [TODO] Add "libgs" to PATH
	 ("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts
--exact-bbox --bbox=preview
--libgs=/opt/homebrew/Cellar/ghostscript/10.03.0/lib/libgs.10.03.dylib
-v4 -o %B-%%9p.svg %f"))
	(imagemagick
	 :programs ("pdflatex" "convert")
	 :description "pdf > png"
	 :message "you need to install the programs: latex and imagemagick."
	 :image-input-type "pdf"
	 :image-output-type "png"
	 :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
	 :latex-precompiler ("pdftex -output-directory %o -ini -jobname=%b \"&pdflatex\" mylatexformat.ltx %f")
	 :image-converter ("convert -density %D -trim -antialias %f -quality 100 %B-%%09d.png"))))

(setq org-latex-packages-alist
      '(("T1" "fontenc" t)
        ("" "amsmath" t)
        ("" "mathtools" t)
        ("" "siunitx" t)
        ("" "physics2" t)
	("libertine" "newtxmath" t)))

(setq org-latex-preview-preamble
      "\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}
\\usephysicsmodule{ab,ab.braket,diagmat,xmat}%
")

(add-hook 'org-mode-hook #'org-latex-preview-auto-mode)

;; (setq org-latex-preview-live nil) ; Do not generate live previews
(setq org-highlight-latex-and-related '(native)) ; Highlight inline LaTeX code

;; Remove dollars and "begin" as delimiters. This may keep LaTeX source
;; code uniform
(plist-put org-latex-preview-appearance-options :matchers '("\\(" "\\["))
(plist-put org-latex-preview-appearance-options :scale 1.35)
(plist-put org-latex-preview-appearance-options :zoom 1.35)

;; Use CDLaTeX to improve editing experiences
(use-package cdlatex
  :straight t
  :diminish (org-cdlatex-mode)
  :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))


;; Load languages for Org Babel

;; Do not ask for confirmation before executing
(setq org-link-elisp-confirm-function nil)
(setq org-link-shell-confirm-function nil)

;; Org code blocks
(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (python . t)))


;; Org-agenda
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(bind-keys :map global-map
	   ("C-c a" . org-agenda))

;; Org-agenda settings related to `org-modern'
(setq org-agenda-tags-column 0)
(setq org-agenda-block-separator ?─)
(setq org-agenda-time-grid
      '((daily today require-timed)
	(800 1000 1200 1400 1600 1800 2000)
	" ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
(setq org-agenda-current-time-string
      "◀── now ─────────────────────────────────────────────────")


;; Useful functions
(defun my/org-mode-insert-get-button ()
  "Inserts a button that copies a user-defined string to clipboard."
  (interactive)
  (let ((content (read-string "Content: ")))
    (insert (format "[[elisp:(kill-new \"%s\")][GET]]" content))))

(defun my/org-mode-preview-buffer ()
  "Preview current buffer including images and LaTeX fragments."
  (interactive)
  (call-interactively 'org-latex-preview-clear-cache)
  (org-latex-preview 'buffer)
  (org-display-inline-images))

(bind-keys :map org-mode-map
           ("s-p" . my/org-mode-preview-buffer))

(provide 'init-org)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
