;;; init-org.el --- Org mode configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :init
  (setq org-fold-core-style 'overlays)
  :config

  ;; Org default directory
  (setq-default org-directory
                (expand-file-name "TH18-03/" my-dev-path))
  (setq-default bookmark-default-file
                (expand-file-name ".bookmarks.el" org-directory))

  ;; Open Org files with previewing
  (setq org-startup-with-inline-images t)
  (setq org-startup-with-latex-preview t))


;; Org Modern
(use-package org-modern
  :custom
  (org-modern-star
   '("􀄩" "􀄩􀄩" "􀄩􀄩􀄩" "􀄩􀄩􀄩􀄩" "􀄩􀄩􀄩􀄩􀄩" "􀄩􀄩􀄩􀄩􀄩􀄩"))
  :config
  (global-org-modern-mode 1)

  ;; Symbols in Org mode
  (setq org-ellipsis " 􀍠")

  ;; Setup pretty entities for unicode math symbols
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts nil))


;; Fold drawers by default
(setq org-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-hide-drawer-all)


;; Org images
(with-eval-after-load 'org
  (setq org-image-actual-width '(350)) ; Fallback to `350'
  (define-key org-mode-map (kbd "s-p") (lambda ()
                                         (interactive)
                                         (org-latex-preview)
                                         (org-display-inline-images))))


;; Org links
(setq org-return-follows-link t)
(setq org-link-elisp-confirm-function nil)

;; Using shift-cursor to select text
(setq org-support-shift-select t)

;; Load languages
(with-eval-after-load 'org

  ;; Org source code blocks
  (setq-default org-confirm-babel-evaluate nil)
  (setq-default org-src-preserve-indentation t)
  (setq-default org-src-fontify-natively t)
  (setq-default org-src-tab-acts-natively t)
  (setq-default org-edit-src-content-indentation 0)

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (shell . t)
                                 (python . t)
                                 (latex . t))))

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

(use-package org-roam
  :defer t
  :init
  (when (not (version< emacs-version "29.5"))
    (use-package emacsql-sqlite-builtin)
    (setq org-roam-database-connector 'sqlite-builtin))
  (global-unset-key (kbd "s-n"))
  :custom
  (org-roam-db-location (expand-file-name "org-roam.db" org-directory))
  (org-roam-directory org-directory)
  (org-roam-dailies-directory "dates/")
  (org-roam-completion-everywhere t)
  (org-roam-db-gc-threshold most-positive-fixnum)
  :bind
  (("s-n j" . org-roam-dailies-goto-today)
   (:map org-mode-map
         ("s-n n" . org-id-get-create)
         ("s-n a" . org-roam-alias-add)
         ("s-n f" . org-roam-node-find)
         ("s-n i" . org-roam-node-insert)))

  ;; Key-bindings for `org-roam-dailies'
  (:map org-mode-map
        ("<s-up>" . org-roam-dailies-goto-previous-note)
        ("<s-down>" . org-roam-dailies-goto-next-note))
  :config
  (org-roam-db-autosync-enable)
  (org-roam-complete-everywhere)

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
           :kill-buffer t))))

(with-eval-after-load 'org
  (org-roam-dailies-goto-today)
  (goto-char (point-max))
  (save-buffer))


(provide 'init-org)
;;; init-org.el ends here
