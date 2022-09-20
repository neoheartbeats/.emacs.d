;;; init.el --- LaTeX configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

;; Align LaTeX fragments
(defun org--make-preview-overlay (beg end image &optional imagetype)
  (let ((ov (make-overlay beg end))
	       (imagetype (or (intern imagetype) 'png)))
    (overlay-put ov 'org-overlay-type 'org-latex-overlay)
    (overlay-put ov 'evaporate t)
    (overlay-put ov
		  'modification-hooks
		  (list (lambda (o _flag _beg _end &optional _l)
			        (delete-overlay o))))
    (overlay-put ov
		  'display
		  (list 'image :type imagetype :file image :ascent '100))))

(setq org-format-latex-options
  '( ;; Ensure LaTeX fragments can be displayed correctly on dark backgrounds
		 :foreground default
     :background "Transparent"
     :scale 3.6
     :html-foreground "Black"
     :html-background "Transparent"
     :html-scale 1.2
     :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))


;; Org LaTeX packages
(setq org-latex-packages-alist
  '(("" "mhchem" t)
	   ("" "siunitx" t)))


;; Better Org LaTeX editor for Org mode
;; Setup `CDLaTeX'
(use-package cdlatex
  :hook (org-mode . org-cdlatex-mode))

;; Syntax highlighting for LaTeX in Org mode
(setq org-highlight-latex-and-related '(latex script entities))


;; Previewing PDF files
(use-package pdf-tools
  :config
  (setq pdf-view-use-scaling t))


(provide 'init-tex)
;;; init-tex.el ends here
