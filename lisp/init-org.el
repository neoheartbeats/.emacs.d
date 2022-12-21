;;; init-org.el --- Org mode configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org)

;; Org default directory
(setq-default org-directory (expand-file-name "myProjects/myEden/" my-home-path))
(setq-default bookmark-default-file (expand-file-name ".bookmarks.el" org-directory))

;; Open Org files with previewing
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)


(setq org-ellipsis " 􀍠")

;; Org Modern
(use-package org-modern
  :custom
  (org-modern-star
   '("􀄩" "􀄩􀄩" "􀄩􀄩􀄩" "􀄩􀄩􀄩􀄩" "􀄩􀄩􀄩􀄩􀄩" "􀄩􀄩􀄩􀄩􀄩􀄩"))
  :config
  (global-org-modern-mode 1))


;; Setup pretty entities for unicode math symbols
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts nil)


;; Hide emphasis markders
(setq org-hide-emphasis-markers t)

;; Fold drawers by default
(add-hook 'org-mode-hook 'org-hide-drawer-all)


;; Org images
(setq org-image-actual-width '(350)) ; Fallback to `350'
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "s-p") (lambda ()
                                         (interactive)
                                         (org-latex-preview)
                                         (org-display-inline-images))))


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


;; Org source code blocks
(setq-default org-confirm-babel-evaluate nil)
(setq-default org-src-preserve-indentation t)
(setq-default org-src-fontify-natively t)
(setq-default org-src-tab-acts-natively t)
(setq-default org-edit-src-content-indentation 0) ; No relative indentation for code blocks
(setq-default org-fontify-whole-block-delimiter-line t) ; Fontify whole block

;; Load languages
(org-babel-do-load-languages 'org-babel-load-languages
	                     '((emacs-lisp . t)
                               (shell . t)
                               (python . t)
                               (latex . t)))

;; Using `zsh' as default
(setq-default shell-file-name "/bin/zsh")

;; Language specified settings
(setq-default org-babel-python-command my-python-exec-path)

;; Hide unwanted shell warning messages
(advice-add 'sh-set-shell :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'message) #'ignore))
                (apply orig-fun args))))


;;; Org mode text edition
;; Number of empty lines needed to keep an empty line between collapsed trees
(setq-default org-cycle-separator-lines 2)


;;; Org Export
;; Export with undetermined links
(setq org-export-with-broken-links t)


(use-package emacsql-sqlite-builtin :defer t)

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam")
  :defer t
  :custom
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-location (expand-file-name "org-roam.db" org-directory))
  (org-roam-directory org-directory)
  (org-roam-dailies-directory "dates/")
  (org-roam-completion-everywhere t)
  (org-roam-db-gc-threshold most-positive-fixnum)
  :bind
  (("C-c n n" . org-id-get-create)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-goto-today)
   ("C-c n l" . org-roam-buffer-toggle))

  ;; Key-bindings for `org-roam-dailies'
  (:map org-mode-map
        ("<s-up>" . org-roam-dailies-goto-previous-note)
        ("<s-down>" . org-roam-dailies-goto-next-note))

  ;; Open link from Org Roam window with mouse click
  ;; (:map org-roam-mode-map
  ;;       ("<mouse-1>" . org-roam-preview-visit)))
  :config
  (org-roam-db-autosync-enable)
  (org-roam-complete-everywhere)

  ;; Like `org-roam-completion-everywhere', but
  ;; this function perform the completion in brackets
  (org-roam-complete-link-at-point)

  ;;; Configure `org-roam-capture-templates'
  ;; Capture template for `org-roam-dailies'
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "\n* %?"
           :target (file+head
		    "%<%Y-%m-%d>.org"
		    "#+TITLE: %<%A-%Y-%m-%d>.\n\n\n")
           :empty-lines 1)))

  ;; Default capture template
  (setq org-roam-capture-templates
	'(("d" "default" entry "\n* %?"
           :target (file+head
                    "notes/${slug}.org"
		    "#+TITLE: ${title}\n\n")
           :empty-lines 1
           :immediate-finish t
           :kill-buffer t)))

  ;; Customize the content in `org-roam-buffer' backlinks
  (cl-defun org-roam-backlinks-section (node &key (unique nil))
    (when-let ((backlinks (seq-sort #'org-roam-backlinks-sort
                                    (org-roam-backlinks-get node :unique unique))))
      (magit-insert-section (org-roam-backlinks)
        (magit-insert-heading "\n LINKED REFERENCES")
        (insert "\n")
        (dolist (backlink backlinks)
          (org-roam-node-insert-section
           :source-node (org-roam-backlink-source-node backlink)
           :point (org-roam-backlink-point backlink)
           :properties (org-roam-backlink-properties backlink)))
        (insert ?\n))))

  ;; Org Roam buffer frame setup
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.35)
                 (window-height . fit-window-to-buffer)))

  ;; Preview LaTeX & images in Org Roam window
  ;; Note this function is defined interactivity
  (add-hook 'org-roam-buffer-postrender-functions
            (lambda ()
              (forward-line 5)
              (insert "\n")
              (visual-line-mode 1)
              (org--latex-preview-region (point-min) (point-max))
              (org-display-inline-images)))
  :hook
  ((after-init . (lambda ()
                   (interactive)
                   (org-roam-dailies-goto-today)
                   (goto-char (point-max))
                   (save-buffer)))))


(provide 'init-org)
;;; init-org.el ends here
