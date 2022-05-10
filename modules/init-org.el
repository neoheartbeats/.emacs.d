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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org init
(use-package org
  :straight (:type built-in))

(setq org-directory "~/org/")

;; Open org files with previewing
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)

;; Use org-indent mode
(setq org-startup-indented t)
(setq org-adapt-indentation t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org editor
;;
;; Enhance inserting headings
(org-insert-heading-respect-content t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org UI
;;
;; Use unicode symbols
(setq org-ellipsis " ¶")

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("§")))

(defun org-icons ()
  (setq prettify-symbols-alist
	  '(
			 ("#+TITLE:" . "T")
       (":PROPERTIES:" . "⌗")
       ("#+BEGIN_SRC" . "⌗")
       ("#+END_SRC" . "-")
       ("#+RESULTS:" . ":")
       ("#+ATTR_ORG:" . "⌗")))
  (prettify-symbols-mode))
(add-hook 'org-mode-hook 'org-icons)

;; Display Org list prefix as dots
(font-lock-add-keywords
	'org-mode
	'(
		 ("^ *\\([-]\\) "
			 (0 (prog1 ()
						(compose-region (match-beginning 1) (match-end 1) "•︎"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup pretty entities for unicode math symbols
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hide emphasis markers
(setq org-hide-emphasis-markers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fold drawers by default
(add-hook 'org-mode-hook 'org-hide-drawer-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Display chapter numbers
(add-hook 'org-mode-hook 'org-num-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org images
(setq org-image-actual-width nil)

;; Shortcut to preview images in Org mode
(global-set-key (kbd "C-x p") 'org-display-inline-images)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org links
(setq org-return-follows-link t)
(setq org-confirm-elisp-link-function nil)

;; Open links in new window
(setq org-link-frame-setup
  '(
  	 (vm . vm-visit-folder-other-window)
  	 (vm-imap . vm-visit-imap-folder-other-frame)
  	 (gnus . org-gnus-no-new-news)
  	 (file . find-file) ;; Open link in current window
  	 (wl . wl-other-frame)))

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
		 (shell . t)
		 (python . t)))

;; Determine Python execution program
(setq org-babel-python-command "python3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org LaTeX
;;
;; Install AUCTeX
(use-package latex :straight auctex)

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
  '( ;; Ensure LaTeX fragments can be displayed correctly on dark backgrounds
		 :foreground default
     :background "Transparent"
     :scale 1.2
     :html-foreground "Black"
     :html-background "Transparent"
     :html-scale 1.2
     :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))

;; Org LaTeX packages
(setq org-latex-packages-alist
  '(
		 ("" "physics" t)
     ("" "mhchem" t)
		 ("" "chemfig" t)
     ("" "gensymb" t)
     ("" "siunitx" t)
		 ("" "euler" t)))

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

;; Syntax highlighting for LaTeX in Org mode
(setq org-highlight-latex-and-related '(latex script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Roam
(use-package org-roam
	:straight (:files (:defaults "extensions/*"))
  :custom
  (org-roam-directory "~/org/")
  (org-roam-dailies-directory "book/")
  (org-roam-completion-everywhere t)
  :bind
	(
		("C-c n n" . org-id-get-create)
		("C-c n a" . org-roam-alias-add)
		("C-c n f" . org-roam-node-find)
		("C-c n i" . org-roam-node-insert)
		("C-c n c" . org-roam-capture)
		("C-c n j" . org-roam-dailies-goto-today)
		("C-c n l" . org-roam-buffer-toggle)
		("<s-up>" . org-roam-dailies-goto-previous-note)
		("<s-down>" . org-roam-dailies-goto-next-note))
  :config
	(org-roam-setup)
	(setq org-roam-db-gc-threshold most-positive-fixnum) ;; Optimize performance
	(setq org-roam-dailies-capture-templates ;; Preferred upper case title tags
    '(
			 ("d" "default" entry
         "* %?"
         :target (file+head
									 "%<%Y-%m-%d>.org"
                   "#+TITLE: %<%Y-%m-%d>\n"))))
	(setq org-roam-capture-templates
		'(
			 ("d" "default" plain "%?"
				 :target (file+head
									 "main/${slug}.org"
									 "#+TITLE: ${title}")
				 :immediate-finish t
				 :unnarrowed t)))
	(setq org-roam-mode-sections
    '((org-roam-backlinks-section :unique t)
       org-roam-reflinks-section))

	;; Org Roam buffer configuration
	(add-to-list 'display-buffer-alist
    '("\\*org-roam\\*"
       (display-buffer-in-side-window)
       (side . right)
       (slot . 0)
       (window-width . 0.33)
       (window-parameters . (
															(no-other-window . t)
                              (no-delete-other-windows . nil)))))
	:hook
	(after-init . (lambda ()
									(org-roam-dailies-goto-today)
									(org-roam-buffer-toggle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Roam UI
(use-package org-roam-ui
  :straight
  (
		:host github
		:repo "org-roam/org-roam-ui"
		:branch "main"
		:files ("*.el" "out"))
  :after org-roam
	:hook (after-init . org-roam-ui-mode)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org roam buffer settings
;;
;; (defun org-roam-links ()
;; 	(interactive)
;; 	(progn
;; 		(split-window-horizontally)
;; 		(org-roam-buffer-toggle)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org roam buffer settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org journal
;; (use-package org-journal
;;   :bind
;;   ("C-c n j" . org-journal-new-entry)
;;   :custom
;;   (org-journal-date-prefix "#+TITLE: ")
;;   (org-journal-file-format "%Y-%m-%d.org")
;;   (org-journal-dir "~/org/book/")
;;   (org-journal-date-format "%Y-%m-%d")
;; 	:config ;; Open file in new frame
;; 	(setq org-journal-find-file 'find-file)
;; 	:bind
;; 	(
;; 		("<s-up>" . org-journal-previous-entry)
;; 		("<s-down>" . org-journal-next-entry)
;; 		("C-c n o" . org-journal-open-current-journal-file))
;; 	:hook
;; 	(after-init . (lambda ()
;; 									(interactive)
;; 									(org-journal-open-current-journal-file)
;; 									(org-roam-db-sync))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mixed pitch mode
;; (use-package mixed-pitch
;; 	:custom
;; 	(mixed-pitch-set-height t)
;; 	(mixed-pitch-variable-pitch-cursor '(bar . 1))
;; 	:hook
;; 	(org-mode . mixed-pitch-mode))

(provide 'init-org)
