;;; init-org.el --- Org Mode -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;
;; Setup default directory
;;
(setopt org-directory "~/Upstage/")

;;
;; Org Mode buffer init behaviors
;;
(setopt org-startup-with-inline-images t)
(setopt org-startup-with-latex-preview t)

;; Install AUCTeX. This is required by TEC's Org
(use-package latex
  :straight auctex)

;;
;; Modern Org Mode theme
;;
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

(defun my-iconify-org-buffer ()
  (progn
    (push '("#+title:     " . ?􀈭) prettify-symbols-alist)
    (push '("#+identifier:" . ?􀅷) prettify-symbols-alist)
    (push '("#+date:      " . ?􀧵) prettify-symbols-alist)
    (push '("#+filetags:  " . ?􀋡) prettify-symbols-alist)
    (push '("#+RESULTS:" . ?􀎚) prettify-symbols-alist)
    (push '("#+attr_org:" . ?􀌞) prettify-symbols-alist)))
(add-hook 'org-mode-hook #'my-iconify-org-buffer)

(setopt org-ellipsis " 􀍠")
(setopt org-hide-emphasis-markers t)

;; Fold drawers by default
(setopt org-cycle-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-fold-hide-drawer-all)

;; Fold titles by default
(setopt org-startup-folded 'content)

;;
;; Org fragments and overlays
;;
(setopt org-image-actual-width '(420))

;;; Org links
(setopt org-return-follows-link t)

;; Open file links in current window
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;; Using shift-<arrow-keys> to select text
(setopt org-support-shift-select t)

;;
;; The Zettlekasten note-taking system by Denote
;;

(use-package denote
  :straight t
  :config
  (setopt denote-directory org-directory) ; Use `org-directory' as default
  (setopt denote-known-keywords '("robot" "poem" "science" "dust")) ; dust can be drafts
  
  ;; Denote for journaling
  (setopt denote-journal-extras-directory
        (expand-file-name "stages/" denote-directory)) ; Subdirectory for journal files
  (setopt denote-journal-extras-keyword "stages") ; Stages are journals

  ;; Do not include date in notes
  (setopt denote-org-front-matter
        "#+title:      %1$s
#+filetags:   %3$s
#+identifier: %4$s
\n")
  :bind
  (:map global-map

        ;; Open today's note
        ("C-c d" . denote-journal-extras-new-or-existing-entry))
  (:map org-mode-map
        ("s-i" . denote-link-or-create))
  :hook
  (after-init . denote-journal-extras-new-or-existing-entry))

(defun my/denote-insert-links-current-month ()
  (interactive)
  (denote-add-links (format-time-string "%B")))

;;
;; Org LaTeX customizations
;;
(setopt org-latex-preview-default-process 'dvisvgm)
(setopt org-latex-packages-alist
      '(("T1" "fontenc" t)
        ("" "amsmath" t)
        ("" "bm" t) ; Bold math required
        ("" "mathtools" t)
        ("" "siunitx" t)
        ("" "physics2" t)
        ("" "mlmodern" t)))

(setopt org-latex-preview-preamble
      "\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}
\\usephysicsmodule{ab,ab.braket,diagmat,xmat}%
")

(add-hook 'org-mode-hook #'(lambda ()
                             (org-latex-preview-auto-mode 1)))

(setopt org-latex-preview-live nil) ; Do not generate live previews while editing

(setopt org-latex-preview-appearance-options
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

;; Do not ask for confirmation before executing
(setopt org-link-elisp-confirm-function nil)
(setopt org-link-shell-confirm-function nil)

;; Org code blocks
(setopt org-confirm-babel-evaluate nil)
(setopt org-src-preserve-indentation t)
(setopt org-src-fontify-natively t)
(setopt org-src-tab-acts-natively t)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (python . t)
                               (shell . t)))

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
