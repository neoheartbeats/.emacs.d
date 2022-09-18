;;; init-org.el --- Org mode configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package org)

;; Org default directory
(setq org-directory "~/.org/")

;; Open Org files with previewing
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)


;; Enhance inserting headings
(org-insert-heading-respect-content t)


;; Org icons
(defun org-icons ()
  (setq prettify-symbols-alist
        '(("lambda" . ?λ)
          (":PROPERTIES:" . ?􀙭)
          ("#+TITLE:" . ?􀎟)
          ("#+AUTHOR:" . ?􀉪)
          ("#+RESULTS:" . ?􀎛)
          ("[ ]" . ?􀂒)
          ("[-]" . ?􀃟)
          ("[X]" . ?􀃳)))
  (prettify-symbols-mode))
(add-hook 'org-mode-hook 'org-icons)
(setq org-ellipsis " 􀰛")

;; Org Modern
(use-package org-modern
  :custom ;; Org modern settings
  (org-modern-star '("􀥳" "􀥳􀥳" "􀥳􀥳􀥳" "􀥳􀥳􀥳􀥳" "􀥳􀥳􀥳􀥳􀥳" "􀥳􀥳􀥳􀥳􀥳􀥳"))
  (org-modern-list nil)
  (org-modern-checkbox nil)
  (org-modern-keyword nil)

  ;; Editor settings
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  :config
  (global-org-modern-mode 1)

  ;; Set frame border width
  (modify-all-frames-parameters
   '((right-divider-width . 5)
     (internal-border-width . 5))))


;; Enable mouse click events
(require 'org-mouse)


;; Setup pretty entities for unicode math symbols
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts nil)


;; Hide emphasis markders
(setq org-hide-emphasis-markers t)


;; Display Org list prefix as dots
(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
		(0 (prog1 ()
				 (compose-region (match-beginning 1) (match-end 1) "􁉃"))))))


;; Fold drawers by default
(add-hook 'org-mode-hook 'org-hide-drawer-all)


;; Org images
(setq org-image-actual-width '240)
(global-set-key (kbd "s-p") (lambda ()
                              (interactive)
                              (org-latex-preview)
                              (org-display-inline-images)))


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


;; Configure Org Roam
(use-package org-roam
  :diminish
  :custom
  (org-roam-directory org-directory)
  (org-roam-dailies-directory "stages/")
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

   ;; Open link from Org Roam window with mouse click
   (:map org-roam-mode-map
         ("<mouse-1>" . org-roam-preview-visit)))
  :config
	(org-roam-setup)
	(setq org-roam-db-gc-threshold most-positive-fixnum) ;; Optimize performance
	(setq org-roam-dailies-capture-templates ;; Preferred upper case title tags
        '(("d" "default" entry "\n* %?"
           :target (file+head
		                "%<%Y-%m-%d>.org"
		                "#+TITLE: %<%Y-%m-%d>\n")
           :empty-lines 1)))

  ;; Collect nodes in determined format
	(setq org-roam-capture-templates
		    '(("d" "default" entry "\n* %?"
			     :target (file+head
			              "main/${slug}.org"
			              "#+TITLE: ${title}\n")
           :empty-lines 1
			     :immediate-finish t
           :kill-buffer t)))

  ;; Objects displayed in Org Roam
  (setq org-roam-mode-sections
        '((org-roam-backlinks-section :unique t)
          org-roam-reflinks-section))

  ;; Org Roam buffer configuration
	(add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  ;; Preview LaTeX & images in Org Roam window
  (add-hook 'org-roam-buffer-postrender-functions
            (lambda ()
              (org--latex-preview-region (point-min) (point-max))
              (org-display-inline-images)))
  :hook
  (after-init . (lambda ()
                  (org-roam-dailies-goto-today))))


;; Org LaTeX bridge
(setq-default org-latex-preview-ltxpng-directory "~/.emacs.d/ltximg/")


(provide 'init-org)
;;; init-org.el ends here
