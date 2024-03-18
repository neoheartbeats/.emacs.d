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
(use-package latex
  :straight auctex)


;; Modern Org Mode theme
(use-package org-modern
  :straight t
  :init
  (setopt org-modern-star '("􀄩"))
  (setopt org-modern-hide-stars "􀄩")
  (setopt org-modern-list '((?- . "•")))
  (setopt org-modern-checkbox '((?X . "􀃰") (?- . "􀃞") (?\s . "􀂒")))
  (setopt org-modern-progress '("􀛪" "􀛩" "􀺶" "􀺸" "􀛨"))
  (setopt org-modern-table-vertical 2)
  (setopt org-modern-todo nil)
  (setopt org-modern-tag nil)
  (setopt org-modern-block-name nil)
  (setopt org-modern-keyword nil)
  (setopt org-modern-timestamp nil)
  (setopt org-modern-block-fringe nil)
  :config (global-org-modern-mode 1))

;; Using the SF Pro font for symbols
(defun my/iconify-org-buffer ()
  (progn
    (push '("#+title:     " . ?􀈭) prettify-symbols-alist)
    (push '("#+identifier:" . ?􀅷) prettify-symbols-alist)
    (push '("#+date:      " . ?􀧵) prettify-symbols-alist)
    (push '("#+filetags:  " . ?􀋡) prettify-symbols-alist)
    (push '("#+begin_src" . ?􀃤) prettify-symbols-alist)
    (push '("#+end_src" . ?􀅽) prettify-symbols-alist)
    (push '("#+begin_quote" . ?􀙤) prettify-symbols-alist)
    (push '("#+end_quote" . ?􀅽) prettify-symbols-alist)
    (push '("#+RESULTS:" . ?􀎚) prettify-symbols-alist)
    (push '("#+attr_org:" . ?􀌞) prettify-symbols-alist)))
(add-hook 'org-mode-hook #'my/iconify-org-buffer)

(setopt org-ellipsis " 􀍠")
(setopt org-hide-emphasis-markers t)

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
  (setq denote-known-keywords '("robots" "poem" "sciences" "dust"))
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
(setq org-latex-preview-process-default 'dvisvgm)
(setq org-latex-packages-alist
      '(("T1" "fontenc" t)
        ("" "amsmath" t)
        ("" "bm" t) ; Bold math required
        ("" "mathtools" t)
        ("" "siunitx" t)
        ("" "physics2" t)
        ("" "mlmodern" t)))

(setq org-latex-preview-preamble
      "\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}
\\usephysicsmodule{ab,ab.braket,diagmat,xmat}%
")

(add-hook 'org-mode-hook #'org-latex-preview-auto-mode)

(setq org-latex-preview-live nil) ; Do not generate live previews

;; Remove dollars and "begin" as delimiters. This may keep LaTeX source
;; code uniform
(plist-put org-latex-preview-appearance-options :matchers '("\\(" "\\["))
(plist-put org-latex-preview-appearance-options :scale 1.04)
(plist-put org-latex-preview-appearance-options :zoom 1.04)

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
                               (python . t)
			       (java . t)
                               (shell . t)))


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
