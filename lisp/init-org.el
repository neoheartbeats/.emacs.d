;;; init-org.el --- Org mode configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :straight (:type built-in))

;; Org default directory
(setq org-directory "/Users/ilyaw39/Dropbox/regrets/")

;; Open Org files with previewing
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)


;; Better experiences inserting headings
(org-insert-heading-respect-content t)


(setq org-ellipsis " 􀍠")

;; Org Modern
(use-package org-modern
  :after org
  :custom
  
  ;; Org modern settings
  (org-modern-star '("􀄩" "􀄩􀄩" "􀄩􀄩􀄩" "􀄩􀄩􀄩􀄩" "􀄩􀄩􀄩􀄩􀄩" "􀄩􀄩􀄩􀄩􀄩􀄩"))
  (org-modern-list nil)
  (org-modern-checkbox nil)
  (org-modern-keyword nil)
  (org-modern-label nil)
  (org-modern-tag nil)
  (org-modern-horizontal-rule nil)
  :config
  (global-org-modern-mode 1)
  :hook
  (org-mode . (lambda ()
                (setq prettify-symbols-alist
                  '((":PROPERTIES:" . ?􀈭)
                     ("#+TITLE:" . ?􀈣)
                     ("#+FILETAGS:" . ?􀈭)
                     ("#+IDENTIFIER:" . ?􀈭)
                     ("#+AUTHOR:" . ?􀉚)
                     ("#+RESULTS:" . ?􀎚)
                     ("#+ATTR_ORG:" . ?􀌞)
                     ("[ ]" . ?􀂒)
                     ("[-]" . ?􀃞)
                     ("[X]" . ?􀃲)))
                (prettify-symbols-mode 1))))


;; Display Org list prefix as dots
(font-lock-add-keywords
  'org-mode
  '(("^ *\\([-]\\) "
		  (0 (prog1 ()
				   (compose-region (match-beginning 1) (match-end 1) "􀄫"))))))


;; Enable mouse click events
(use-package org-mouse
  :straight (:type built-in)
  :after org)


;; Setup pretty entities for unicode math symbols
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts nil)


;; Hide emphasis markders
(setq org-hide-emphasis-markers t)


;; Fold drawers by default
(add-hook 'org-mode-hook 'org-hide-drawer-all)


;; Org images
(setq org-image-actual-width nil)
(global-set-key (kbd "s-p") (lambda ()
                              (interactive)
                              (org-latex-preview)
                              (org-display-inline-images)))


;; Org links
(setq org-return-follows-link t)
(setq org-link-elisp-confirm-function nil)

;; Open file links in current window
(setq org-link-frame-setup
  '((vm . vm-visit-folder-other-window)
  	 (vm-imap . vm-visit-imap-folder-other-frame)
  	 (gnus . org-gnus-no-new-news)
  	 (file . find-file)
  	 (wl . wl-other-frame)))


;; Org src blocks
(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; Load languages
(org-babel-do-load-languages 'org-babel-load-languages
	'((emacs-lisp . t)
		 (shell . t)
		 (python . t)
     (latex . t)))

;; Hide unwanted shell warning messages
(advice-add 'sh-set-shell :around
  (lambda (orig-fun &rest args)
    (cl-letf (((symbol-function 'message) #'ignore))
      (apply orig-fun args))))

;; Determine Python execution program
(setq org-babel-python-command "python3")

;; Ignore the warnings
(setq python-indent-guess-indent-offset t)  
(setq python-indent-guess-indent-offset-verbose nil)


(provide 'init-org)
;;; init-org.el ends here