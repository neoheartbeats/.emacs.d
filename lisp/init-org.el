;; init-org.el --- Org mode configuration  -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
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
(use-package org-modern :ensure t
  :init
  (setq org-modern-star '("􀄩"))
  (setq org-modern-hide-stars "􀄩")
  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X . "􀃰") (?- . "􀃞") (?\s . "􀂒")))
  (setq org-modern-progress '("􀛪" "􀛩" "􀺶" "􀺸" "􀛨"))
  (setq org-modern-table-vertical 2)
  (setq org-modern-keyword nil)
  :config (global-org-modern-mode 1))

(defun my-iconify-org-buffer ()
  (progn
    (push '(":PROPERTIES:" . ?􀈭) prettify-symbols-alist)
    (push '(":ID:      " . ?􀐚) prettify-symbols-alist)
    (push '(":ROAM_ALIASES:" . ?􀅷) prettify-symbols-alist)
    (push '(":END:" . ?􀅽) prettify-symbols-alist)
    (push '("#+TITLE:" . ?􀈷) prettify-symbols-alist)
    (push '("#+AUTHOR:" . ?􀉩) prettify-symbols-alist)
    (push '("#+RESULTS:" . ?􀎚) prettify-symbols-alist)
    (push '("#+ATTR_ORG:" . ?􀌞) prettify-symbols-alist))
  (prettify-symbols-mode 1))
(add-hook 'org-mode-hook #'my-iconify-org-buffer)

(setq org-ellipsis " 􀍠")
(setq org-hide-emphasis-markers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Draw fringes in Org mode
(defun my-toggle-internal-fringes ()
  (setq left-margin-width 15)
  (setq right-margin-width 15)
  (set-window-buffer nil (current-buffer)))

(add-hook 'org-mode-hook #'my-toggle-internal-fringes)

;; Fold drawers by default
(setq org-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-hide-drawer-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org fragments
(setq org-image-actual-width '(300))

(defun my-preview-org-fragments ()
  (interactive)
  (org-display-inline-images)
  (org-latex-preview))

(bind-keys :map org-mode-map
           ("s-p" . my-preview-org-fragments))

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
(use-package org-roam :ensure t
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
  (global-unset-key (kbd "s-o"))
  :bind
  (("s-n" . org-roam-dailies-goto-today)
   :map org-mode-map
   (("s-i" . org-roam-node-insert)
    ("s-f" . org-roam-node-find)
    ("s-o" . org-roam-alias-add)
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
(setq org-latex-preview-default-process 'dvisvgm)
(setq org-latex-packages-alist
      '(("T1" "fontenc" t)
        ("" "amsmath" t)
        ("" "mathtools" t)
        ("" "siunitx" t)
        ("" "physics2" t)
        ("noDcommand" "kpfonts")))

(setq org-latex-preview-process-alist
      '((dvipng
         :programs ("latex" "dvipng")
         :description "dvi > png"
         :message "you need to install the programs: latex and dvipng."
         :image-input-type "dvi"
         :image-output-type "png"
         :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
         :latex-precompiler ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
         :image-converter ("dvipng --follow -D %D -T tight --depth --height -o %B-%%09d.png %f")
         :transparent-image-converter
         ("dvipng --follow -D %D -T tight -bg Transparent --depth --height -o %B-%%09d.png %f"))
        (dvisvgm
         :programs ("latex" "dvisvgm")
         :description "dvi > svg"
         :message "you need to install the programs: latex and dvisvgm."
         :image-input-type "dvi"
         :image-output-type "svg"
         :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
         :latex-precompiler ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
         ;; With dvisvgm the --bbox=preview flag is needed to emit the preview.sty-provided
         ;; height+width+depth information. The --optimise, --clipjoin, and --relative flags
         ;; cause dvisvgm do do some extra work to tidy up the SVG output, but barely add to
         ;; the overall dvisvgm runtime (<1% increace, from testing).
         :image-converter ("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview --exact-bbox -o %B-%%9p.svg %f"))
        (imagemagick
         :programs ("pdflatex" "convert")
         :description "pdf > png"
         :message "you need to install the programs: latex and imagemagick."
         :image-input-type "pdf"
         :image-output-type "png"
         :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
         :latex-precompiler ("pdftex -output-directory %o -ini -jobname=%b \"&pdflatex\" mylatexformat.ltx %f")
         :image-converter
         ("convert -density %D -trim -antialias %f -quality 100 %B-%%09d.png"))))

(setq org-latex-preview-preamble
      "\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}
\\usephysicsmodule{ab,ab.braket}%
")

(plist-put org-latex-preview-options :scale 2.0)
(plist-put org-latex-preview-options :zoom 1.25)

;; Use `CDLaTeX' to improve editing experiences
(use-package cdlatex :ensure t
  :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

(add-hook 'org-mode-hook #'(lambda ()
                             (org-latex-preview-auto-mode 1)))


(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-org.el ends here
