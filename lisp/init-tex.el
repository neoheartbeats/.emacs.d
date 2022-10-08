;;; init-tex.el --- LaTeX configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Install AUCTeX
(use-package latex
  :straight auctex
  :init
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t))


(setq-default org-latex-preview-ltxpng-directory "~/.emacs.d/ltximg/")


;; Setup `dvisvgm' to preview LaTeX fragments
(setq org-preview-latex-default-process 'imagemagick)
(setq org-preview-latex-process-alist
  '((imagemagick
      :programs ("xelatex" "convert")
      :description "pdf > png"
      :image-input-type "pdf"
      :image-output-type "png"
      :image-size-adjust (1.0 . 1.0)
      :latex-compiler
      ("xelatex -interaction nonstopmode -output-directory %o %f")
      :image-converter
      ("convert -density %D -trim -antialias %f -quality 100 %O"))))

(setq org-format-latex-options
  '( ; Ensure LaTeX fragments can be displayed correctly on dark backgrounds
		 :foreground default
     :background "Transparent"
     :scale 1.0
     :html-foreground "Black"
     :html-background "Transparent"
     :html-scale 1.2
     :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))


;; Org LaTeX packages
(setq org-latex-packages-alist
  '(("" "mathtools" t)
     ("" "physics" t)
     ("" "mhchem" t)
	   ("" "siunitx" t)
     ("" "unicode-math" t)
     ("" "upgreek" t)))

;; LaTeX format options
(setq org-format-latex-header
  (concat org-format-latex-header "\n\\setmathfont{TeX Gyre Bonum Math}"))


;;; Better LaTeX editor for Org mode
;; Setup `CDLaTeX'
(use-package cdlatex
  :hook
  (org-mode . org-cdlatex-mode))

;; Syntax highlighting for LaTeX in Org mode
(setq org-highlight-latex-and-related '(latex script entities))


(use-package ox-latex
  :straight (:type built-in))

(setq-default org-latex-compiler "xelatex")


(provide 'init-tex)
;;; init-tex.el ends here
