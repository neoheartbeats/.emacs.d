;;; init-org.el --- Org mode configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)

;; Org default directory
(setq-default org-directory my-org-path)

;; Open Org files with previewing
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)


(require-package 'org-cliplink)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Various preferences
(setq org-log-done t)
(setq org-edit-timestamp-down-means-later t)
(setq org-hide-emphasis-markers t)
(setq org-catch-invisible-edits 'show)
(setq org-export-coding-system 'utf-8)
(setq org-fast-tag-selection-single-key 'expert)
(setq org-html-validation-link nil)
(setq org-export-kill-product-buffer-when-displayed t)
(setq org-tags-column 80)


(when (maybe-require-package 'org-modern)
  (setq org-modern-star '("􀄩"))
  (setq org-modern-hide-stars "􀄩")
  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X . "􀃠") (?- . "􀃞") (?\s . "􀂒")))
  (setq org-modern-block-name '(("src" . ("􀓪" "􀅽"))))
  (setq org-modern-table-vertical 1)
  (setq org-modern-keyword nil)
  (setq org-modern-block-fringe nil)

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))


;; Symbols in Org mode
(add-hook
 'org-mode-hook
 #'(lambda ()
     (setq prettify-symbols-alist
           '((":PROPERTIES:" . ?􀈣)
             (":ID:" . ?􀅳)
             (":END:" . ?􀅽)
             ("#+TITLE:" . ?􀎞)
             ("#+RESULTS:" . ?􀆀)
             ("#+ATTR_ORG:" . ?􀣋)
             ("SCHEDULED:" . ?􀧞)
             ("CLOSED:" .?􁜒)))
     (prettify-symbols-mode 1)))

(setq org-ellipsis " 􀍠")
(setq org-hide-emphasis-markers t)

;; Draw fringes in Org mode
(defun pes-toggle-internal-fringes ()
  (setq left-margin-width 5)
  (setq right-margin-width 5)
  (set-window-buffer nil (current-buffer)))

(add-hook 'org-mode-hook #'pes-toggle-internal-fringes)

;; Setup pretty entities for unicode math symbols
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts nil)


(setq org-catch-invisible-edits 'show)
(setq org-insert-heading-respect-content t)


;; Fold drawers by default
(setq org-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-hide-drawer-all)


;; Org images
(with-eval-after-load 'org
  (setq org-image-actual-width '(300)) ; Fallback to `300'
  (define-key
   org-mode-map (kbd "s-p")
   (lambda ()
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

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t) (emacs-lisp . t) (python . t) (latex . t))))


;; Org mode text edition
;; Number of empty lines needed to keep an empty line between collapsed trees
(setq-default org-cycle-separator-lines 2)


(when (require-package 'org-roam)
  (require-package 'emacsql-sqlite-builtin)
  (setq org-roam-database-connector 'sqlite-builtin))

(setq org-roam-db-location (expand-file-name "org-roam.db" org-directory))
(setq org-roam-directory org-directory)
(setq org-roam-dailies-directory "dates/")
(setq org-roam-completion-everywhere t)
(setq org-roam-db-gc-threshold most-positive-fixnum)

;; Capture template for `org-roam-dailies'
(setq org-roam-dailies-capture-templates
      '(("d"
         "default"
         entry
         "\n* %?"
         :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d • %A>\n")
         :empty-lines 1)))

;; Default capture template for notes
(setq
 org-roam-capture-templates
 '(("d"
    "default"
    plain
    "%?"
    :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
    :empty-lines 1
    :unnarrowed t
    :immediate-finish t)))

(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode 1)

  (global-unset-key (kbd "s-n"))
  (define-key global-map (kbd "s-n j") 'org-roam-dailies-goto-today)

  (define-key org-mode-map (kbd "s-n n") 'org-roam-node-insert)
  (define-key org-mode-map (kbd "s-n a") 'org-roam-alias-add)
  (define-key org-mode-map (kbd "s-n f") 'org-roam-node-find)

  (define-key org-mode-map (kbd "s-<up>") 'org-roam-dailies-goto-previous-note)
  (define-key org-mode-map (kbd "s-<down>") 'org-roam-dailies-goto-next-note))

;; Open today's note when startup
(add-hook
 'after-init-hook
 #'(lambda ()
     (interactive)
     (org-roam-dailies-goto-today)
     (save-buffer)
     (goto-char (point-max))))


(provide 'init-org)
;;; init-org.el ends here
