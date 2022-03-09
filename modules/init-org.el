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
(setq org-startup-with-latex-preview t)

;; Use indent mode
(setq org-startup-indented t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org editor
;;
;; Enhance inserting headings
(org-insert-heading-respect-content t)

;; Enhance LaTeX editing
(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

;; Searching synonyms
(use-package powerthesaurus
  :bind
  ("M-p" . powerthesaurus-lookup-synonyms-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org ui
;;
;; Use unicode symbols
(setq org-ellipsis " ▼")
(use-package org-bullets
  :diminish t
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("▼")))
(use-package org-fancy-priorities
  :diminish t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("🔴" "🟡" "🟢")))

;; Prettify symbols
(defun org-icons ()
  (setq prettify-symbols-alist
	    '(("lambda" . "λ")
	      (":PROPERTIES:" . "≡")
	      (":ID:" . "i")
	      (":END:" . "-")
	      ("#+TITLE:" . "T")
          ("#+title:" . "T")
	      ("#+begin_src" . "»")
	      ("#+end_src" . "»")
          ("#+RESULTS:" . ":")
          ("#+attr_org:" . "⌘")
	      (":ROAM_ALIASES:" . "@")))
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
;; Set preview caches directory
(setq org-latex-preview-ltxpng-directory "~/.emacs.d/ltximg/")

;; Set preview process
(setq org-preview-latex-default-process 'imagemagick)
(setq org-preview-latex-process-alist
      '((imagemagick
	     :programs
	     ("latex" "convert")
	     :description "pdf > png"
	     :image-input-type "pdf"
	     :image-output-type "png"
	     :image-size-adjust
	     (1.0 . 1.0)
	     :latex-compiler
	     ("xelatex -interaction nonstopmode -output-directory %o %f")
	     :image-converter
	     ("convert -density %D -trim -antialias %f -quality 100 %O"))))
(plist-put org-format-latex-options :scale 1.0)

;; Org LaTeX packages
(setq org-latex-packages-alist
      '(("" "physics" t)
	    ("" "mhchem" t)
        ("" "mathtools" t)
	    ("" "gensymb" t)
	    ("" "notomath" t)))

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
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
		         (display-buffer-in-side-window)
		         (side . right)
		         (slot . 0)
		         (window-width . 0.42)
		         (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  (defun display-backlink-latex (node)
    (org--latex-preview-region (point-min) (point-max)))
  (eval-after-load 'org-roam
    (advice-add 'org-roam-reflinks-section
		        :after #'display-backlink-latex))
  :hook
  (after-init . org-roam-dailies-goto-today)
  (org-roam-backlinks-mode . visual-line-mode))

(provide 'init-org)

;; init-org.el ends here
