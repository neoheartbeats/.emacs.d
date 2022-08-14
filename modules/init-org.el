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

(setq org-directory "~/Deusilence/")

;; Open org files with previewing
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)

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

;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode)
;;   :custom (org-bullets-bullet-list '("§")))

;; Display Org list prefix as dots
(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
		(0 (prog1 ()
				 (compose-region (match-beginning 1) (match-end 1) "•︎"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup pretty entities for unicode math symbols
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fold drawers by default
(add-hook 'org-mode-hook 'org-hide-drawer-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org images
(setq org-image-actual-width '150)

;; Shortcut to preview images in Org mode
(global-set-key (kbd "C-x p") 'org-display-inline-images)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org links
(setq org-return-follows-link t)
(setq org-confirm-elisp-link-function nil)

;; Open links in new window
(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-window)
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
	                           '((emacs-lisp . t)
		                           (shell . t)
		                           (python . t)))

;; Hide unwanted shell warning messages
(advice-add 'sh-set-shell :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'message) #'ignore))
                (apply orig-fun args))))

;; Determine Python execution program
(setq org-babel-python-command "python3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Roam
(use-package org-roam
	:straight (:files (:defaults "extensions/*"))
  :custom
  (org-roam-directory "~/Deusilence/")
  (org-roam-dailies-directory "./")
  (org-roam-completion-everywhere t)
  :bind
	(("C-c n n" . org-id-get-create)
	 ("C-c n a" . org-roam-alias-add)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n j" . org-roam-dailies-goto-today)
	 ("C-c n l" . org-roam-buffer-toggle)
	 ("<s-up>" . org-roam-dailies-goto-previous-note)
	 ("<s-down>" . org-roam-dailies-goto-next-note)
   ("C-x o" . (lambda ()
                (interactive)
                (if (equal (prin1-to-string (current-buffer))
                           "#<buffer *org-roam*>")
                    (call-interactively 'other-window)
                  (select-window (get-buffer-window "*org-roam*")))))
   (:map org-roam-mode-map
         ("<mouse-1>" . org-roam-preview-visit)))
  :config
	(org-roam-setup)
	(setq org-roam-db-gc-threshold most-positive-fixnum) ;; Optimize performance
	(setq org-roam-dailies-capture-templates ;; Preferred upper case title tags
        '(("d" "default" entry
           "* %?"
           :target (file+head
		                "%<%Y-%m-%d>.org"
		                "#+TITLE: %<%Y-%m-%d>\n"))))
	(setq org-roam-capture-templates
		    '(("d" "default" plain "%?"
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
                 (window-parameters . ((no-other-window . t)
		                                   (no-delete-other-windows . nil)))))
  ;; Preview LaTeX fragments in Org Roam window
  (add-hook 'org-roam-buffer-postrender-functions
            (lambda ()
              (org--latex-preview-region (point-min) (point-max))
              (org-display-inline-images)))

	:hook
	(after-init . (lambda ()
									(org-roam-dailies-goto-today)
                  (org-roam-buffer-toggle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Todos
(setq org-todo-keywords
	    '((sequence "TODO" "REVIEW" "|" "DONE" "CANCELLED")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emms support
(use-package emms
  :init
  (emms-minimalistic)
  (emms-default-players))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org LaTeX bridge
;;
;; Direct LaTeX preview image files
(setq org-latex-preview-ltxpng-directory "~/.emacs.d/ltximg/")

(provide 'init-org)
