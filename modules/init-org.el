;; init-org.el --- Lyrith: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Essentials must be loaded first.
;;
;; Code:

(require 'org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org init
(setq org-directory "~/org/")

;; Open org files with previewing
(setq org-startup-with-inline-images t)
;; (setq org-startup-with-latex-preview t)

;; Use indent mode
(setq org-startup-indented t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org editor
;;
;; Enhance inserting headings
(org-insert-heading-respect-content t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org UI
;;
;; Use unicode symbols
(setq org-ellipsis " …")
(use-package org-bullets
  :diminish org-bullets-mode
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("▼")))

;; Prettify symbols
(defun org-icons ()
  (setq prettify-symbols-alist
	    '(("#+title:" . "T")
          (":PROPERTIES:" . "≡")
	      ("#+BEGIN_SRC" . "»")
	      ("#+END_SRC" . "»")
          ("#+RESULTS:" . ":")
          ("#+ATTR_ORG:" . "⌘")))
  (prettify-symbols-mode))
(add-hook 'org-mode-hook 'org-icons)

;; Org list displayed as dots
(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 ()
	     (compose-region
	      (match-beginning 1)
	      (match-end 1) "•"))))))

;; Pretty entities
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts nil)

;; Hide drawers
(add-hook 'org-mode-hook 'org-hide-drawer-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org images
(setq org-image-actual-width nil)

;; Shortcut to display images
(global-set-key (kbd "C-x p") 'org-display-inline-images)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org links
(setq org-return-follows-link t)
(setq org-confirm-elisp-link-function nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org babel settings
;;
;; Org src blocks
(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; Load languages
(org-babel-do-load-languages 'org-babel-load-languages
			                 '((shell . t)
			                   (octave . t)
			                   (scheme . t)
			                   (emacs-lisp . t)
			                   (python . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org LaTeX
;;
;; Use xenops to replace built-in latex-preview program
(use-package xenops
  :config
  (setq xenops-math-image-scale-factor 1.8)
  (setq xenops-cache-directory "~/.cache/xenops/")
  (setq xenops-math-latex-max-tasks-in-flight 64)
  (setq xenops-tooltip-delay 0)
  :hook
  (org-mode . xenops-mode))

;; Org LaTeX packages
(setq org-latex-packages-alist
      '(("" "physics" t)
	    ("" "mhchem" t)
        ("" "mathtools" t)
	    ("" "gensymb" t)
        ("" "txfonts" t)))

;; Setup CDLaTeX
(use-package cdlatex
  :hook
  (org-mode . org-cdlatex-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org roam
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/")
  (org-roam-dailies-directory "dLs/")
  (org-roam-completion-everywhere t)
  :bind
  ("C-c n n" . org-id-get-create)
  ("C-c n a" . org-roam-alias-add)
  ("C-c n f" . org-roam-node-find)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n j" . org-roam-dailies-goto-today)
  ("<s-up>" . org-roam-dailies-goto-previous-note)
  ("<s-down>" . org-roam-dailies-goto-next-note)
  :config
  (org-roam-setup)
  :hook
  (after-init . org-roam-dailies-goto-today))

(provide 'init-org)

;; init-org.el ends here
