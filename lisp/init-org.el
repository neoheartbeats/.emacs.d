;;; init-org.el --- Org mode configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)

(when *const-q*
  (setq org-fold-core-style 'text-properties))

;; Org default directory
(setq-default org-directory
              (expand-file-name "TH18-03/" my-dev-path))

;; Open Org files with previewing
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)


;; Org Modern
(use-package org-modern
  :config
  (setq org-modern-star '("􀄩"))
  (setq org-modern-hide-stars "􀄩")
  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X . "􀃠") (?- . "􀃞") (?\s . "􀂒")))
  (setq org-modern-block-name '(("src" . ("􀐘" "􀅽"))))
  (setq org-modern-keyword nil)
  (set-face-attribute 'org-modern-symbol nil
                      :family "SF Pro")

  (global-org-modern-mode 1))


;; Symbols in Org mode
(setq org-ellipsis " 􀍠")

;; Setup pretty entities for unicode math symbols
;; (setq org-pretty-entities t)
;; (setq org-pretty-entities-include-sub-superscripts nil)


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

(setq-default org-link-frame-setup ; Open files in current frame
              (cl-acons 'file #'find-file org-link-frame-setup))

;; Using shift-<arrow-keys> to select text
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


;; Org mode text edition
;; Number of empty lines needed to keep an empty line between collapsed trees
(setq-default org-cycle-separator-lines 2)


(when *const-q*
  (use-package emacsql-sqlite-builtin))

(use-package org-roam
  :init
  (global-unset-key (kbd "s-n"))
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
  (when *const-q* ; Use the built-in sqlite3
    (setq org-roam-database-connector 'sqlite-builtin))
  (setq org-roam-db-location (expand-file-name "org-roam.db" org-directory))
  (setq org-roam-directory org-directory)
  (setq org-roam-dailies-directory "dates/")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-db-gc-threshold most-positive-fixnum)

  (org-roam-db-autosync-mode 1)

  ;; Capture template for `org-roam-dailies'
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "\n* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+TITLE: %<%Y-%m-%d-%Y>\n")
           :empty-lines 1)))

  ;; Default capture template for notes
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
           :target (file+head "notes/${slug}.org"
                              "#+TITLE: ${title}\n")
           :empty-lines 1
           :unnarrowed t
           :immediate-finish t
           :kill-buffer t))))

;; Open today's note when startup
(add-hook 'after-init-hook #'org-roam-dailies-goto-today)


(provide 'init-org)
;;; init-org.el ends here
