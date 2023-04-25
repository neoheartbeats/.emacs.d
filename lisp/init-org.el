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
(setq org-directory "~/Shelter/")
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)

(bind-keys :map org-mode-map
           ("C-c l" . org-store-link))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modern Org Mode
(use-package org-modern
  :ensure t
  :init
  (setq org-modern-star '("􀄩"))
  (setq org-modern-hide-stars "􀄩")
  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X . "􀃰") (?- . "􀃞") (?\s . "􀂒")))
  ;; (setq org-modern-block-name '(("src" . ("􀓪" "􀅽"))
  ;;                               ("quote" . ("􀌮" "􀅽"))))
  (setq org-modern-progress '("􀛪" "􀛩" "􀺶" "􀺸" "􀛨"))
  (setq org-modern-table-vertical 2)
  ;; (setq org-modern-block-fringe nil)
  (setq org-modern-keyword nil)
  :config (global-org-modern-mode 1))

(defun pes-iconify-org-buffer ()
  (progn
    (push '(":PROPERTIES:" . ?􀈭) prettify-symbols-alist)
    (push '(":ID:      " . ?􀐚) prettify-symbols-alist)
    (push '(":END:" . ?􀅽) prettify-symbols-alist)
    (push '("#+TITLE:" . ?􀈷) prettify-symbols-alist)
    (push '("#+AUTHOR:" . ?􀉩) prettify-symbols-alist)
    (push '("#+RESULTS:" . ?􀎚) prettify-symbols-alist)
    (push '("#+ATTR_ORG:" . ?􀌞) prettify-symbols-alist))

  ;; 􀅷
  (prettify-symbols-mode 1))

(add-hook 'org-mode-hook #'pes-iconify-org-buffer)

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
                             '((shell . t)
                               (emacs-lisp . t)
                               (python . t)
                               (latex . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org mode text edition
(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory org-directory)
  (setq org-roam-dailies-directory "dates/")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-db-gc-threshold most-positive-fixnum)

  ;; Capture template for `org-roam-dailies'
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "\n* %?"
           :target (file+head
                    "%<%Y-%m-%d>.org"
                    "#+TITLE: %<%Y-%m-%d 􀉉>\n")
           :empty-lines 1)))

  ;; Default capture template for notes
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head
                    "notes/%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+TITLE: ${title}\n")
           :empty-lines 1
           :unnarrowed t
           :immediate-finish t)))

  (org-roam-db-autosync-mode 1)
  :bind
  (("s-n" . org-roam-dailies-goto-today)
   :map org-mode-map
   (("s-i" . org-roam-node-insert)
    ("s-f" . org-roam-node-find)
    ("s-a" . org-roam-alias-add)
    ("s-<up>" . org-roam-dailies-goto-previous-note)
    ("s-<down>" . org-roam-dailies-goto-next-note)))
  :hook
  (org-roam-dailies-find-file . (lambda ()
                                  (save-buffer)
                                  (goto-char (point-max))))
  (after-init . org-roam-dailies-goto-today))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org LaTeX customizations
(setq-default org-preview-latex-default-process 'dvisvgm)
(setq-default org-latex-preview-options
              (progn
                (plist-put org-format-latex-options :background "Transparent")
                (plist-put org-format-latex-options :scale 5.0)
                (plist-put org-format-latex-options :zoom 1.25)))

(setq org-latex-packages-alist
      '(("T1" "fontenc" t)
        ("version=4" "mhchem" t)
        ("" "amsmath" t)
        ("" "mathtools" t)
        ("" "siunitx" t)
        ("" "physics2" t)
        ("" "mlmodern" t)))

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
\\usephysicsmodule{ab,ab.braket}%
")

(setq org-latex-preview-auto-generate 'live)
(add-hook 'org-mode-hook #'org-latex-preview-auto-mode)


(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-org.el ends here
