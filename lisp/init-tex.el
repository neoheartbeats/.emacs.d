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


(setq-default org-latex-preview-ltxpng-directory
              (expand-file-name "ltximg/" user-emacs-directory))


;; Setup `imagemagick' to preview LaTeX fragments
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-preview-latex-process-alist
      '((dvisvgm
         :programs ("latex" "dvisvgm")
         :description "dvi > svg"
         :image-input-type "dvi"
         :image-output-type "svg"
         :image-size-adjust (1.7 . 1.5)
         :post-clean (".dvi" ".tex" ".aux" ".svg" ".png" ".jpg" ".jpeg" ".out")
         :latex-compiler ("latex -interaction nonstopmode -output-directory %o '\\PassOptionsToPackage{active,tightpage,auctex}{preview}\\input{%f}' %f")
         :image-converter ("dvisvgm %f -e -n -b 1 -c %S -o %O"))))

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
        ("version=4" "mhchem" t)
        ("" "siunitx" t)
        ("displaymath,floats,graphics,textmath,footnotes" "preview" t)))


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

(setq temporary-file-directory
      (expand-file-name "tmp" user-emacs-directory))


(require 'preview)

(defun my/org--match-text-baseline-ascent ()
  (let* ((tmp (file-name-with-extension "tmp" "log"))
         (regexp-string "^! Preview:.*\(\\([0-9]*?\\)\\+\\([0-9]*?\\)x\\([0-9]*\\))")
         logfile ascent bbox)
    (if (eq (car (directory-files temporary-file-directory t "orgtex\.\*\\\.log")) nil)
        (setq-local ascent 'center)
      (progn
        (setq logfile (car (directory-files temporary-file-directory t "orgtex\.\*\\\.log"))))
      (with-temp-file tmp
        (insert-file-contents-literally logfile)
        (goto-char (point-max))
        (if (re-search-backward regexp-string nil t)
            (progn
              (setq log (match-string 0))
              (setq bbox (mapcar #'(lambda (x)
                                     (* (preview-get-magnification)
                                        (string-to-number x)))
                                 (list (match-string 1)
                                       (match-string 2)
                                       (match-string 3)))))))
      (delete-file tmp)
      (setq-local ascent (preview-ascent-from-bb (preview-TeX-bb bbox))))))

(defun org--make-preview-overlay (beg end image &optional imagetype)
  "Build an overlay between BEG and END using IMAGE file.
Argument IMAGETYPE is the extension of the displayed image,
as a string.  It defaults to \"png\"."
  (my/org--match-text-baseline-ascent)
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
		 (list 'image :type imagetype :file image :ascent ascent))))


(provide 'init-tex)
;;; init-tex.el ends here
