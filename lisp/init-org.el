;; init-org.el --- Org mode configuration  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-export-coding-system 'utf-8)
(setq org-fast-tag-selection-single-key 'expert)
(setq org-export-kill-product-buffer-when-displayed t)
(setq org-fontify-whole-heading-line t)

(setq org-directory "/Users/ilyaw39/Developer/PesBook/")

(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)

(bind-keys :map org-mode-map
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modern Org Mode
(use-package org-modern :ensure t
  :custom
  (org-modern-star '("􀄩"))
  (org-modern-hide-stars "􀄩")
  (org-modern-list '((?- . "•")))
  (org-modern-checkbox '((?X . "􀃠") (?- . "􀃞") (?\s . "􀂒")))
  (org-modern-block-name '(("src" . ("􀓪" "􀅽"))))
  (org-modern-table-vertical 2)
  (org-modern-block-fringe nil)
  (org-modern-keyword nil)
  :config (global-org-modern-mode 1)
  :hook
  (org-mode . (lambda ()
                (setq prettify-symbols-alist
                  '(
                     (":PROPERTIES:" . ?􀈣)
                     (":ID:" . ?􀅳)
                     (":END:" . ?􀅽)
                     ("#+TITLE:" . ?􀎞)
                     ("#+RESULTS:" . ?􀆀)
                     ("#+ATTR_ORG:" . ?􀣋)
                     ("SCHEDULED:" . ?􀧞)
                     ("CLOSED:" .?􁜒)))
                (prettify-symbols-mode 1))))

(setq org-ellipsis " 􀍠")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Draw fringes in Org mode
(defun pes-toggle-internal-fringes ()
  (setq left-margin-width 15)
  (setq right-margin-width 15)
  (set-window-buffer nil (current-buffer)))

(add-hook 'org-mode-hook #'pes-toggle-internal-fringes)

;; Fold drawers by default
(setq org-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-hide-drawer-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org fragments
(setq org-image-actual-width '(300))

(defun pes-preview-org-fragments ()
  (interactive)
  (org-display-inline-images)
  (org-latex-preview))

(bind-keys :map org-mode-map
  ("s-p" . pes-preview-org-fragments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org links
(setq org-return-follows-link t)
(setq org-link-elisp-confirm-function nil)

(setq-default org-link-frame-setup ; Open files in current frame
  (cl-acons 'file #'find-file org-link-frame-setup))

;; Using shift-<arrow-keys> to select text
(setq org-support-shift-select t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load languages
;;
;; Org source code blocks
(setq-default org-confirm-babel-evaluate nil)
(setq-default org-src-preserve-indentation t)
(setq-default org-src-fontify-natively t)
(setq-default org-src-tab-acts-natively t)
(setq-default org-edit-src-content-indentation 0)

(org-babel-do-load-languages 'org-babel-load-languages
  '(
     (shell . t)
     (emacs-lisp . t)
     (python . t)
     (latex . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org mode text edition
(use-package org-roam :ensure t
  :config
  (setq org-roam-db-location (expand-file-name "org-roam.db" org-directory))
  (setq org-roam-directory org-directory)
  (setq org-roam-dailies-directory "dates/")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-node-display-template "${TITLE:*}")
  (setq org-roam-db-gc-threshold most-positive-fixnum)

  ;; Capture template for `org-roam-dailies'
  (setq org-roam-dailies-capture-templates
    '(("d" "default" entry "\n* %?"
        :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d • %A>\n")
        :empty-lines 1)))

  ;; Default capture template for notes
  (setq org-roam-capture-templates
    '(("d" "default" plain "%?"
        :target (file+head "notes/${slug}.org" "#+TITLE: ${title}\n")
        :empty-lines 1
        :unnarrowed t
        :immediate-finish t)))

  (org-roam-db-autosync-mode 1)

  (global-unset-key (kbd "s-n"))
  (define-key global-map (kbd "s-n j") 'org-roam-dailies-goto-today)

  (define-key org-mode-map (kbd "s-n i") 'org-roam-node-insert)
  (define-key org-mode-map (kbd "s-n a") 'org-roam-alias-add)
  (define-key org-mode-map (kbd "s-n f") 'org-roam-node-find)

  (define-key org-mode-map (kbd "s-<up>") 'org-roam-dailies-goto-previous-note)
  (define-key org-mode-map (kbd "s-<down>") 'org-roam-dailies-goto-next-note)
  :hook
  (after-init . (lambda ()
                  (interactive)
                  (org-roam-dailies-goto-today)
                  (save-buffer)
                  (goto-char (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better LaTeX editor for Org mode
(add-hook 'org-mode-hook #'org-latex-preview-auto-mode)

(use-package cdlatex :ensure t
  :hook
  (
    (LaTeX-mode . turn-on-cdlatex)
    (org-mode . turn-on-org-cdlatex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org LaTeX customizations
(setq-default org-preview-latex-default-process 'dvisvgm)
(setq-default org-latex-preview-options
  (progn
    (plist-put org-format-latex-options :background "Transparent")
    (plist-put org-format-latex-options :scale 5.0)
    (plist-put org-format-latex-options :zoom 1.25)))

(setq-default org-latex-default-packages-alist nil)

(setq org-latex-packages-alist
  '(
     ("T1" "fontenc" t)
     ("" "amsmath" t)
     ("" "mathtools" t)
     ("" "siunitx" t)
     ("" "xparse" t)
     ("" "upgreek")
     ("version=4" "mhchem" t)
     ("" "kpfonts" t)))


(setq org-latex-preview-preamble
  "\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}
\\NewDocumentCommand{\\PE}{ O{} }{%
  E_{\\mathrm{p}}%
}%
\\NewDocumentCommand{\\KE}{ O{} }{%
  E_{\\mathrm{k}}%
}%
")

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-org.el ends here
