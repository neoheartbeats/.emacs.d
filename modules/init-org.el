;; init-org.el --- Credits: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Org mode setup.
;;
;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org init
(use-package org
  :straight (:type built-in))

(require 'org)

(setq org-directory "~/org/")

;; Open org files with previewing
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)

;; Use org-indent mode
(setq org-startup-indented t)
(setq org-adapt-indentation t)

;; Show section numbers
(add-hook 'org-mode-hook 'org-num-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org editor
;;
;; Enhance inserting headings
(org-insert-heading-respect-content t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support mouse click
(use-package org-mouse
  :straight (:type built-in))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org UI
;;
;; Use unicode symbols
(setq org-ellipsis " ▼")

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("§")))

(defun org-icons ()
  (setq prettify-symbols-alist
	  '(
			 ("#+TITLE:" . "T")
       (":PROPERTIES:" . "☰")
       ("#+BEGIN_SRC" . "⌗")
       ("#+END_SRC" . "-")
       ("#+RESULTS:" . ":")
       ("#+ATTR_ORG:" . "⚙")))
  (prettify-symbols-mode))
(add-hook 'org-mode-hook 'org-icons)

;; Display Org list prefix as dots
(font-lock-add-keywords
	'org-mode
	'(
		 ("^ *\\([-]\\) "
			 (0 (prog1 ()
						(compose-region (match-beginning 1) (match-end 1) "▶︎"))))))

;; Setup pretty entities for unicode math symbols
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts nil)

;; Fold drawers by default
(add-hook 'org-mode-hook 'org-hide-drawer-all)

;; Hide emphasis markers
(setq org-hide-emphasis-markers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org images
(setq org-image-actual-width nil)

;; Shortcut to display images
(global-set-key (kbd "C-x p") 'org-display-inline-images)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org links
(setq org-return-follows-link t)
(setq org-confirm-elisp-link-function nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org src blocks
(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; Load languages
(org-babel-do-load-languages 'org-babel-load-languages
	'(
		 (emacs-lisp . t)
		 (python . t)))

;; Determine Python execution program
(setq org-babel-python-command "python3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org LaTeX
;;
;; Install AUCTeX
(use-package auctex :defer t)

;; Setup `dvisvgm' to preview LaTeX fragments
(setq org-preview-latex-default-process 'dvisvgm)

(setq org-preview-latex-process-alist
  '(
		 (dvisvgm
			 :programs ("xelatex" "dvisvgm")
			 :description "xdv > svg"
			 :image-input-type "xdv"
			 :image-output-type "svg"
			 :image-size-adjust (1.7 . 1.5)
			 :latex-compiler ;; Default `xelatex' as the process previewing LaTeX fragments
			 ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
			 :image-converter ;; Set `dvisvgm' with --exact option
			 ("dvisvgm %f -e -n -b min -c %S -o %O"))))

(setq org-format-latex-options
  '(
		 :foreground default
     :background "Transparent"
     :scale 1.05
     :html-foreground "Black"
     :html-background "Transparent"
     :html-scale 1.05
     :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))

;; Org LaTeX packages
(setq org-latex-packages-alist
  '(
		 ("" "physics" t)
     ("" "mhchem" t)
     ("" "gensymb" t)
     ("" "siunitx" t)
     ("" "isomath" t)
     ("mathrm=sym" "unicode-math" t)
     ("" "firamath-otf" t)))

;; (setq org-latex-packages-alist
;;       '(
;;         ("" "physics" t)
;;         ("" "mhchem" t)
;;         ("" "gensymb" t)
;;         ("" "siunitx" t)
;;         ("" "txfonts" t)))

;; Direct LaTeX preview image files
(setq org-latex-preview-ltxpng-directory "~/.emacs.d/ltximg/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better LaTeX editor for Org mode
;;
;; Setup `CDLaTeX'
(use-package cdlatex
  :hook (org-mode . org-cdlatex-mode))

;; Setup `org-fragtog'
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(setq org-highlight-latex-and-related '(latex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org todo
;; (setq org-todo-keywords '((sequence "TODO" "REVIEW" "DONE")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org roam
(use-package org-roam
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/")
  (org-roam-dailies-directory "dLs/")
  (org-roam-completion-everywhere t)
  :bind
	(
		("C-c n n" . org-id-get-create)
		("C-c n a" . org-roam-alias-add)
		("C-c n f" . org-roam-node-find)
		("C-c n i" . org-roam-node-insert)
		("C-c n l" . org-roam-buffer-toggle)
		("C-c n j" . org-roam-dailies-goto-today)
		("<s-up>" . org-roam-dailies-goto-previous-note)
		("<s-down>" . org-roam-dailies-goto-next-note))
  :config
  (org-roam-setup)
  (setq org-roam-dailies-capture-templates ;; Preferred upper-case title tags
    '(
			 ("d" "default" entry "* %?"
         :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>"))))
  (setq org-roam-capture-templates
    '(
			 ("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}")
         :unnarrowed t)))
  :hook (after-init . org-roam-dailies-goto-today))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mixed pitch mode
(use-package mixed-pitch
  :config
	(setq mixed-pitch-set-height t)
	(setq mixed-pitch-variable-pitch-cursor '(bar . 1))
  :hook (org-mode . mixed-pitch-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org tables
;;
;; Align Org mode tables
(use-package valign
	:hook (org-mode . valign-mode)
	:custom (valign-fancy-bar t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spell checking
(use-package ispell
  :straight (:type built-in)
  :hook (org-mode . flyspell-mode))

(provide 'init-org)
