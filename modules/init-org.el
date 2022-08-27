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
  :straight (:type built-in)
  :hook
  (org-mode . (lambda ()
                (setq mode-line-format
                      '("    "
                        "    [  "
                        mode-line-buffer-identification
                        " ]    [  "
                        mode-name
                        " ]")))))

(setq org-directory "~/fairy-stage/")

;; Open Org files with previewing
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org editor
;;
;; Enhance inserting headings
(org-insert-heading-respect-content t)

;; Enable mouse click events
(require 'org-mouse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup pretty entities for unicode math symbols
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Prettify Org mode (Org mode UI setup)
;;
;; Org modern
(use-package org-modern
  :custom
  ;; Org modern settings
  (org-modern-star '("" "" ""))
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
  :hook
  (org-mode . (lambda ()
                (setq prettify-symbols-alist
                      '(("lambda" . ?λ)
                        (":PROPERTIES:" . ?)
                        (":ID:" . ?)
                        (":ROAM_ALIASES:" . ?)
                        (":END:" . ?)
                        ("#+TITLE:" . ?)
                        ("#+AUTHOR:" . ?)
                        ("#+RESULTS:" . ?)
                        ("[ ]" . ?)
                        ("[-]" . ?)
                        ("[X]" . ?)))
                (prettify-symbols-mode))))

;; Org mode icons
(setq org-ellipsis " ")

;; Hide emphasis markders
(setq org-hide-emphasis-markers t)

;; Display Org list prefix as dots
(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
		(0 (prog1 ()
				 (compose-region (match-beginning 1) (match-end 1) "•︎"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fold drawers by default
(add-hook 'org-mode-hook 'org-hide-drawer-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org images
(setq org-image-actual-width '150)

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

   ;; Select window with `other-window'
   ("C-x o" . (lambda ()
                (interactive)
                (if (equal (prin1-to-string (current-buffer))
                           "#<buffer *org-roam*>")
                    (call-interactively 'other-window)
                  (select-window (get-buffer-window "*org-roam*")))))

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
			              "fairies/${slug}.org"
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

  ;; Open links in other windows
  (defun org-roam-preview-visit (file point &optional other-window)
    (setq other-window t) ;; Always preview in other window
    (interactive (list (org-roam-buffer-file-at-point 'assert)
                       (oref (magit-current-section) point)
                       current-prefix-arg))
    (let ((buf (find-file-noselect file))
          (display-buffer-fn (if other-window
                                 #'switch-to-buffer-other-window
                               #'pop-to-buffer-same-window)))
      (funcall display-buffer-fn buf)
      (with-current-buffer buf
        (widen)
        (goto-char point))
      (when (org-invisible-p) (org-show-context))
      buf))

  (defun org-roam-grep-visit (file &optional other-window row col)
    (setq other-window t) ;; Always preview in other window
    (interactive (list (org-roam-buffer-file-at-point t)
                       current-prefix-arg
                       (oref (magit-current-section) row)
                       (oref (magit-current-section) col)))
    (let ((buf (find-file-noselect file))
          (display-buffer-fn (if other-window
                                 #'switch-to-buffer-other-window
                               #'pop-to-buffer-same-window)))
      (funcall display-buffer-fn buf)
      (with-current-buffer buf
        (widen)
        (goto-char (point-min))
        (when row
          (forward-line (1- row)))
        (when col
          (forward-char (1- col))))
      (when (org-invisible-p) (org-show-context))
      buf))

  ;; Styling Org Roam window
  (cl-defun org-roam-backlinks-section (node &key (unique nil))
    (when-let ((backlinks
                (seq-sort #'org-roam-backlinks-sort
                          (org-roam-backlinks-get node :unique unique))))
      (magit-insert-section (org-roam-backlinks)
        (magit-insert-heading "\n[  LINKED MENTIONS ] ") ;; Use icon instead
        (dolist (backlink backlinks)
          (org-roam-node-insert-section
           :source-node (org-roam-backlink-source-node backlink)
           :point (org-roam-backlink-point backlink)
           :properties (org-roam-backlink-properties backlink)))
        (insert ?\n))))

	:hook
	(after-init . (lambda ()
									(org-roam-dailies-goto-today)
                  (org-roam-buffer-toggle)))
  (org-roam-mode . (lambda ()
                     (setq mode-line-format
                           '("    "
                             "    [  "
                             mode-name
                             " ]")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emms support
(use-package emms
  :init
  (emms-minimalistic)
  (emms-default-players)
  :custom
  (emms-mode-line-format " [  EMMS ] ")
  (emms-mode-line-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org LaTeX bridge
;;
;; Direct LaTeX preview image files
(setq org-latex-preview-ltxpng-directory "~/.emacs.d/ltximg/")

(provide 'init-org)
