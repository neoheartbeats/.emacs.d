;;; init-org.el --- Org Mode -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;
;; Setup default directory
;;
(setq org-directory "/Users/ilyaw39/nexus/nexus-notes/")

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
  (setq org-modern-block-name nil)
  (setq org-modern-keyword nil)
  (setq org-modern-timestamp nil)
  :config (global-org-modern-mode 1))

(defun my-iconify-org-buffer ()
  (progn
    (push '(":PROPERTIES:" . ?􀈭) prettify-symbols-alist)
    (push '(":ID:      " . ?􀐚) prettify-symbols-alist)
    (push '(":ROAM_ALIASES:" . ?􀅷) prettify-symbols-alist)
    (push '(":END:" . ?􀅽) prettify-symbols-alist)
    (push '("#+TITLE:" . ?􀧵) prettify-symbols-alist)
    (push '("#+AUTHOR:" . ?􀉩) prettify-symbols-alist)
    (push '("#+RESULTS:" . ?􀎚) prettify-symbols-alist)
    (push '("#+ATTR_ORG:" . ?􀌞) prettify-symbols-alist)
    (push '("#+STARTUP: " . ?􀖆) prettify-symbols-alist))
  (prettify-symbols-mode 1))
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
;; The Zettlekasten note-taking system by Org Roam
;;
(use-package org-roam
  :straight t
  :after (org)
  :config
  (setq org-roam-directory org-directory)
  (setq org-roam-dailies-directory "dates/")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-db-gc-threshold most-positive-fixnum)

  ;; Capture template for `org-roam-dailies'
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "\n* %?"
           :target (file+head
                    "%<%Y-%m-%d>.org"
                    "#+TITLE: %<%Y-%m-%d %A>\n")
           :empty-lines 1)))

  ;; Default capture template for notes
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head
                    "notes/${slug}.org"
                    "#+TITLE: ${title}\n")
           :empty-lines 1
           :unnarrowed t
           :immediate-finish t)))

  (org-roam-db-autosync-mode 1)
  :bind
  (("s-p" . org-roam-dailies-goto-today)
   :map org-mode-map
   (("s-i" . org-roam-node-insert)
    ("s-<up>" . org-roam-dailies-goto-previous-note)
    ("s-<down>" . org-roam-dailies-goto-next-note)))
  :hook (after-init . org-roam-dailies-goto-today))

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
