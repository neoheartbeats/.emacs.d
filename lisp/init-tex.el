;;; init-tex.el --- LaTeX configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Install AUCTeX
(use-package latex
  :straight auctex
  :init
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  :hook
  ((LaTeX-mode-hook . LaTeX-math-mode)
   (LaTeX-mode-hook . turn-on-reftex)))


(setq-default org-latex-preview-ltxpng-directory "~/.emacs.d/ltximg/")


;; Setup `imagemagick' to preview LaTeX fragments
(setq org-preview-latex-default-process 'imagemagick)
(setq org-preview-latex-process-alist
      '((imagemagick
         :programs ("xelatex" "convert")
         :description "pdf > png"
         :image-input-type "pdf"
         :image-output-type "png"
         :image-size-adjust (1.0 . 1.0)
         :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
         :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))

(setq org-format-latex-options
      '( ; Ensure LaTeX fragments can be displayed correctly on dark backgrounds
	:foreground default
        :background "Transparent"
        :scale 1.2
        :html-foreground "Black"
        :html-background "Transparent"
        :html-scale 1.2
        :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))


;; Org LaTeX packages
(setq org-latex-packages-alist
      '(("" "mathtools" t)
        ("" "physics" t)
        ("" "mhchem" t)
        ("" "siunitx" t)))


;;; Better LaTeX editor for Org mode
;; Setup `CDLaTeX'
(use-package cdlatex
  :hook
  ((LaTeX-mode . turn-on-cdlatex)
   (org-mode . turn-on-org-cdlatex)))

;; Syntax highlighting for LaTeX in Org mode
(setq org-highlight-latex-and-related '(script))


(use-package ox-latex
  :straight (:type built-in))

(setq-default org-latex-compiler "xelatex")


(defun my/org-latex--get-tex-string ()
  "Return the content of the LaTeX fragment at point."
  (let ((datum (org-element-context)))
    (org-element-property :value datum)))

(defun my/latex-fragment-superscript-p ()
  "Return `t' if '^' in current LaTeX fragment."
  (memq 94 (string-to-list (my/org-latex--get-tex-string))))

(defun my/latex-fragment-subscript-p ()
  "Return `t' if '_' in current LaTeX fragment."
  (memq 95 (string-to-list (my/org-latex--get-tex-string))))

(defun my/latex-fragment-script-p ()
  "Return `t' if both '_' &  '^' in current LaTeX fragment."
  (and (memq 94 (string-to-list (my/org-latex--get-tex-string)))
       (memq 95 (string-to-list (my/org-latex--get-tex-string)))))

(defun org--make-preview-overlay (beg end image &optional imagetype)
  "Build an overlay between BEG and END using IMAGE file.
Argument IMAGETYPE is the extension of the displayed image,
as a string.  It defaults to \"png\"."
  (setq my/position 'center)
  (cond ((my/latex-fragment-script-p)
         (setq my/position 'center))
        ((my/latex-fragment-superscript-p)
         (setq my/position 100))
        ((my/latex-fragment-subscript-p)
         (setq my/position 70)))
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
		 (list 'image :type imagetype :file image :ascent my/position))))


(provide 'init-tex)
;;; init-tex.el ends here
