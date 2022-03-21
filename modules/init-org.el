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
;; Org mode setup.
;;
;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org init
(require 'org)
(setq org-directory "~/org/")

;; Open org files with previewing
(setq org-startup-with-inline-images t)

;; Use indent mode
(setq org-startup-indented t)

;; Show section numbers
(add-hook 'org-mode-hook 'org-num-mode)

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
(setq org-ellipsis " ❡")

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("§")))

;; Prettify symbols
(defun org-icons ()
  (setq prettify-symbols-alist
	    '(("#+TITLE:" . "T")
          (":PROPERTIES:" . "≡")
	      ("#+BEGIN_SRC" . "»")
	      ("#+END_SRC" . "«")
          ("#+RESULTS:" . "⌘")
          ("#+ATTR_ORG:" . "⌘")))
  (prettify-symbols-mode))
(add-hook 'org-mode-hook 'org-icons)

;; Org list prefix displayed as dots
(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 ()
	     (compose-region
	      (match-beginning 1)
	      (match-end 1) "○"))))))

;; Pretty entities
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts nil)

;; Hide drawers
(add-hook 'org-mode-hook 'org-hide-drawer-all)

;; Hide emphasis markers
(setq org-hide-emphasis-markers t)

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
			                   (emacs-lisp . t)
			                   (python . t)))

;; Determine Python execution program
(setq org-babel-python-command "python3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org LaTeX
;;
;; Use `xenops' as the default latex-preview program
(use-package xenops
  :config
  (setq xenops-math-image-scale-factor 1.25)
  (setq xenops-math-image-margin 0)
  (setq xenops-cache-directory "~/.emacs.d/xenops/")
  (setq xenops-math-latex-max-tasks-in-flight 64)
  (setq xenops-tooltip-delay 0)
  (setq xenops-math-latex-process-alist ;; Set `dvisvgm' with --exact option
        '((dvisvgm
           :programs ("latex" "dvisvgm")
           :description "dvi > svg"
           :message "you need to install the programs: latex and dvisvgm."
           :image-input-type "dvi"
           :image-output-type "svg"
           :image-size-adjust (1.7 . 1.5)
           :latex-compiler
           ("latex -interaction nonstopmode -shell-escape -output-format dvi -output-directory %o %f")
           :image-converter ("dvisvgm %f -e -n -b %B -c %S -o %O"))))
  :hook
  (org-mode . xenops-mode))

;; Org LaTeX packages
(setq org-latex-packages-alist
      '(("" "physics" t)
	    ("" "mhchem" t)
        ("" "gensymb" t)
	    ("" "siunitx" t)
        ("T1" "fontenc" t)
        ("" "txfonts" t)))

;; Setup `CDLaTeX'
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
  (setq org-roam-dailies-capture-templates ;; Preferred upper-case title tags
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>"))))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}")
           :unnarrowed t)))
  :hook
  (after-init . org-roam-dailies-goto-today))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mixed pitch mode
(use-package mixed-pitch
  :config
  (setq mixed-pitch-set-height 120) ;; Ensure larger font can be displayed correctly
  :hook
  (org-mode . mixed-pitch-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spell checking
(require 'ispell)
(add-hook 'org-mode-hook 'flyspell-mode)

(provide 'init-org)

;; init-org.el ends here
