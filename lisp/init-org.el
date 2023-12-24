;;; init-org.el --- Org Mode -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;
;; Setup default directory
;;
(setq org-directory "~/.org/")

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
  :after (org)
  :init
  (setq org-modern-star '("􀄩"))
  (setq org-modern-hide-stars "􀄩")
  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X . "􀃰") (?- . "􀃞") (?\s . "􀂒")))
  (setq org-modern-progress '("􀛪" "􀛩" "􀺶" "􀺸" "􀛨"))
  (setq org-modern-table-vertical 2)
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
  (prettify-symbols-mode 1)))
(add-hook 'org-mode-hook #'my-iconify-org-buffer)

(setq org-ellipsis " 􀍠")
(setq org-hide-emphasis-markers t)

;; Fold drawers by default
(setq org-cycle-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-fold-hide-drawer-all)

;;
;; Org fragments and overlays
;;
(setq org-image-actual-width '(420))

;; Org links
(setq org-return-follows-link t)

;; Using shift-<arrow-keys> to select text
(setq org-support-shift-select t)

;;
;; The Zettlekasten note-taking system by Denote
;;
(use-package denote
  :straight t
  :config
  (setq denote-directory org-directory) ; Use `org-directory' as default
  (setq denote-known-keywords '("robot" "poem" "science" "dust"))
  
  ;; Denote for journaling
  (setq denote-journal-extras-directory nil) ; Use `denote-directory' as default
  (setq denote-journal-extras-keyword "entrance") ; The entrance for a day

  ;; Format journal titles as Monday 19 September 2023 08:49 PM
  (setq denote-journal-extras-title-format 'day-date-month-year-12h)

  ;; Use `org-read-date' in date prompts
  (setq denote-date-prompt-use-org-read-date t)
  
  ;; Dependences
  (use-package tmr
    :straight t)
  :hook
  (denote-journal-extras . (lambda () ; Journaling with a timer with 15 minutes
                             (tmr "15" "Entrance")))
  :bind
  (:map global-map

        ;; Open today's note
        ("C-c d" . denote-journal-extras-new-or-existing-entry)))

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
        ("" "mlmodern" t))) ; Draw LaTeX with larger weight for better readability

(setq org-latex-preview-preamble
      "\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}
\\usephysicsmodule{ab,ab.braket,diagmat,xmat}%
")

(plist-put org-latex-preview-options :scale 2.00)
(plist-put org-latex-preview-options :zoom 1.05)

;; Use CDLaTeX to improve editing experiences
(use-package cdlatex
  :straight t
  :diminish (org-cdlatex-mode)
  :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

(add-hook 'org-mode-hook #'(lambda ()
                             (org-latex-preview-auto-mode 1)))

;;
;; Load languages for Org Babel
;;
(setq-default org-confirm-babel-evaluate nil)
(setq-default org-src-preserve-indentation t)
(setq-default org-src-fontify-natively t)
(setq-default org-src-tab-acts-natively t)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (python . t)
                               (java . t)))

(provide 'init-org)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
