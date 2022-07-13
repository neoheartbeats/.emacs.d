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
;; LaTeX environment setup.
;; This includes LaTeX mode & Org LaTeX.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org LaTeX
;;
;; Install AUCTeX
(use-package latex :straight auctex)

;; Setup `dvisvgm' to preview LaTeX fragments
(setq org-preview-latex-default-process 'dvisvgm)

(setq org-preview-latex-process-alist
      '((dvisvgm
			   :programs ("xelatex" "dvisvgm")
			   :description "xdv > svg"
			   :image-input-type "xdv"
			   :image-output-type "svg"
			   :image-size-adjust (1.7 . 1.5)
			   :latex-compiler ;; Default `xelatex' as the process previewing LaTeX fragments
			   ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
			   :image-converter ;; Set `dvisvgm' with --exact option
			   ("dvisvgm %f -e -n -b min -c %S -o %O"))))

(setq org-format-latex-options
      '( ;; Ensure LaTeX fragments can be displayed correctly on dark backgrounds
		    :foreground default
        :background "Transparent"
        :scale 0.85
        :html-foreground "Black"
        :html-background "Transparent"
        :html-scale 1.2
        :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))

;; Org LaTeX packages
(setq org-latex-packages-alist
      '(("" "mhchem" t)
	      ("" "siunitx" t)
        ("" "pxfonts" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better Org LaTeX editor for Org mode
;;
;; Setup `CDLaTeX'
(use-package cdlatex
  :hook (org-mode . org-cdlatex-mode))

;; Setup `org-fragtog'
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; Syntax highlighting for LaTeX in Org mode
(setq org-highlight-latex-and-related '(latex script entities))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reference management
;;
;; Org-ref setup
(use-package org-ref
  :init
  (setq bibtex-completion-bibliography
        '("~/org/labs/latex/OverleafExample/sample.bib")
	      bibtex-completion-library-path '("~/org/labs/latex/OverleafExample/"
                                         "~/org/works/7BTeX/")
	      ;; bibtex-completion-notes-path "~/org/labs/latex/OverleafExample/"
	      ;; bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
        bibtex-completion-additional-search-fields '(keywords)
	      bibtex-completion-display-formats
	      '((article .
                   "${=has-pdf=:1}${=has-note=:1}${year:4} ${author:36} ${title:*} ${journal:40}")
	        (inbook .
                  "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	        (incollection .
                        "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	        (inproceedings .
                         "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	        (t .
             "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	      bibtex-completion-pdf-open-function
	      (lambda (fpath)
	        (call-process "open" nil 0 nil fpath)))
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5)
  (global-set-key (kbd "C-c ]") 'org-ref-insert-link)
  (add-hook 'org-ref-clean-bibtex-entry-hook 'org-ref-replace-nonascii))

;; BibTeX setup
(setq bibtex-dialect 'biblatex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org LaTeX export to PDF
;;
;; Org LaTeX exportation process setup
(setq org-latex-pdf-process
      ("latexmk -bibtex -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Previewing PDF files
(use-package pdf-tools
  :config
  (setq pdf-view-use-scaling t))

(provide 'init-tex)
