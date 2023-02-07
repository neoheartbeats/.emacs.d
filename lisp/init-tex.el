;;; init-tex.el --- LaTeX configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Install AUCTeX
(require-package 'auctex)

(setq-default org-latex-preview-ltxpng-directory
              (expand-file-name "ltximg/" user-emacs-directory))

;; Setup `dvisvgm' to preview LaTeX fragments
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-preview-latex-process-alist
      '((dvisvgm
         :programs ("latex" "dvisvgm")
         :description "dvi > svg"
         :image-input-type "dvi"
         :image-output-type "svg"
         :image-size-adjust (1.7 . 1.5)
         :latex-compiler
         ("latex -interaction nonstopmode -output-directory %o %f")
         :image-converter
         ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))))

;; Org LaTeX packages
(setq org-latex-packages-alist
      '(("" "mathtools" t)
        ("retain-explicit-decimal-marker" "siunitx" t)
        ("" "physics" t)
        ("version=4" "mhchem" t)
        ("" "mlmodern" t)))

(setq org-format-latex-options
      '(
        :foreground default
        :background "Transparent"
        :scale 1.70
        :html-foreground default
        :html-background "Transparent"
        :html-scale 1.25
        :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))

;; Match the text baseline of an LaTeX fragment to the surrounding text
(defun pes-org--latex-header-preview (orig &rest args)
  "Setup dedicated `org-format-latex-header'
to `pes-org--match-text-baseline-ascent'."
  (let ((org-format-latex-header "\\documentclass[preview]{standalone}
  \\usepackage[usenames]{color}
  [PACKAGES]
  [DEFAULT-PACKAGES]"))
    (apply orig args)))

(defun pes-org--match-text-baseline-ascent (imagefile)
  "Set `:ascent' to match the text baseline of an image to the surrounding text.
  Calculate `ascent' with the data collected in IMAGEFILE."
  (advice-add 'org-create-formula-image :around #'pes-org--latex-header-preview)
  (let* ((viewbox (split-string
                   (xml-get-attribute (car (xml-parse-file imagefile))
                                      'viewBox)))
         (min-y (string-to-number (nth 1 viewbox)))
         (height (string-to-number (nth 3 viewbox)))
         (ascent (round (* -100 (/ min-y height)))))
    (if (or (< ascent 0) (>= ascent 100))
        'center
      ascent)))

(defun org--make-preview-overlay (beg end image &optional imagetype)
  "Build an overlay between BEG and END using IMAGE file.
  Argument IMAGETYPE is the extension of the displayed image,
  as a string.  It defaults to \"png\"."
  (let ((ov (make-overlay beg end))
	(imagetype (or (intern imagetype) 'png)))
    (let ((ascent (pes-org--match-text-baseline-ascent image)))
      (overlay-put ov 'org-overlay-type 'org-latex-overlay)
      (overlay-put ov 'evaporate t)
      (overlay-put ov
		   'modification-hooks
		   (list (lambda (o _flag _beg _end &optional _l)
			   (delete-overlay o))))
      (overlay-put ov
		   'display
		   (list 'image :type imagetype :file image :ascent ascent)))))


(when (maybe-require-package 'cdlatex)
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (with-eval-after-load 'cdlatex
    (diminish 'org-cdlatex-mode)))


(provide 'init-tex)
;;; init-tex.el ends here
