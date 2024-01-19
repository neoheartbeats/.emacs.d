;;; init-org.el --- Org Mode -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;
;; Setup default directory
;;
(setq org-directory "~/Upstage/")

;;
;; Org Mode buffer init behaviors
;;
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)

;; Install AUCTeX. This is required by TEC's Org
(use-package latex
  :straight auctex)

;;
;; Modern Org Mode theme
;;
(use-package org-modern
  :straight t
  :init
  (setq org-modern-star '("􀄩"))
  (setq org-modern-hide-stars "􀄩")
  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X . "􀃰") (?- . "􀃞") (?\s . "􀂒")))
  (setq org-modern-progress '("􀛪" "􀛩" "􀺶" "􀺸" "􀛨"))
  (setq org-modern-table-vertical 2)
  (setq org-modern-todo nil)
  (setq org-modern-tag nil)
  (setq org-modern-block-name nil)
  (setq org-modern-keyword nil)
  (setq org-modern-timestamp nil)
  :config (global-org-modern-mode 1))

(defun my-iconify-org-buffer ()
  (progn
    (push '("#+title:     " . ?􀈭) prettify-symbols-alist)
    (push '("#+identifier:" . ?􀅷) prettify-symbols-alist)
    (push '("#+date:      " . ?􀧵) prettify-symbols-alist)
    (push '("#+filetags:  " . ?􀋡) prettify-symbols-alist)
    (push '("#+RESULTS:" . ?􀎚) prettify-symbols-alist)
    (push '("#+attr_org:" . ?􀌞) prettify-symbols-alist)
    (push '("Sunday" . ?􀀸) prettify-symbols-alist)
    (push '("Monday" . ?􀀺) prettify-symbols-alist)
    (push '("Tuesday" . ?􀀼) prettify-symbols-alist)
    (push '("Wednesday" . ?􀀾) prettify-symbols-alist)
    (push '("Thursday" . ?􀁀) prettify-symbols-alist)
    (push '("Friday" . ?􀁂) prettify-symbols-alist)
    (push '("Saturday" . ?􀁄) prettify-symbols-alist)
    (prettify-symbols-mode 1)))
(add-hook 'org-mode-hook #'my-iconify-org-buffer)

(setq org-ellipsis " 􀍠")
(setq org-hide-emphasis-markers t)

;; Draw fringes in Org mode
(defun my-toggle-internal-fringes ()
  (setq left-margin-width 5)
  (setq right-margin-width 5)
  (set-window-buffer nil (current-buffer)))

(add-hook 'org-mode-hook #'my-toggle-internal-fringes)

;; Fold drawers by default
(setq org-cycle-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-fold-hide-drawer-all)

;; Fold titles by default
(setq-default org-startup-folded 'content)

;;
;; Org fragments and overlays
;;
(setq org-image-actual-width '(420))

;;; Org links
(setq org-return-follows-link t)

;; Open file links in current window
(setq org-link-frame-setup '((file . find-file)))


;; Using shift-<arrow-keys> to select text
(setq org-support-shift-select t)

;;
;; The Zettlekasten note-taking system by Denote
;;

(use-package denote
  :straight t
  :config
  (setq denote-directory org-directory) ; Use `org-directory' as default
  (setq denote-known-keywords '("robot" "poem" "science" "dust")) ; dust can be drafts
  
  ;; Denote for journaling
  (setq denote-journal-extras-directory
        (expand-file-name "stages/" denote-directory)) ; Subdirectory for journal files
  (setq denote-journal-extras-keyword "stages") ; Stages are journals

  ;; Do not include date in notes
  (setq denote-org-front-matter
        "#+title:      %1$s
#+filetags:   %3$s
#+identifier: %4$s
\n")
  :bind
  (:map global-map

        ;; Open today's note
        ("C-c d" . denote-journal-extras-new-or-existing-entry))
  (:map org-mode-map
        ("s-i" . denote-link-or-create)))

(defun my/denote-insert-links-current-month ()
  (interactive)
  (denote-add-links (format-time-string "%B")))

;;
;; Org LaTeX customizations
;;
(setq org-latex-preview-default-process 'dvisvgm)
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

(add-hook 'org-mode-hook #'(lambda ()
                             (org-latex-preview-auto-mode 1)))

(setq org-latex-preview-appearance-options
      '(
        :foreground auto
        :background "Transparent"
        :scale 1.04
        :zoom 1.04
        :page-width 0.6
        :matchers ("begin" "\\(" "\\["))) ; Removed dollars as delimiters


;; Use CDLaTeX to improve editing experiences
(use-package cdlatex
  :straight t
  :diminish (org-cdlatex-mode)
  :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

;;
;; Load languages for Org Babel
;;

;; Do not ask confirmation before executing Emacs Lisp links
(setq-default org-link-elisp-confirm-function nil)

(setq-default org-confirm-babel-evaluate nil)
(setq-default org-src-preserve-indentation t)
(setq-default org-src-fontify-natively t)
(setq-default org-src-tab-acts-natively t)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (python . t)
                               (shell . t)))

;;;
;;
;; Useful functions
;;

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
