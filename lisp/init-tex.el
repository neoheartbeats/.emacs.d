;;; init-tex.el --- LaTeX configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-org)

;; Install AUCTeX
(use-package latex :ensure auctex :demand t)


(setq-default org-latex-preview-ltxpng-directory
  (expand-file-name "ltximg/" user-emacs-directory))


;; Setup `dvisvgm' to preview LaTeX fragments
(setq org-preview-latex-default-process 'dvisvgm)

(setq org-preview-latex-process-alist
  '(
     (dvisvgm
       :programs ("latex" "dvisvgm")
       :description "dvi > svg"
       :image-input-type "dvi"
       :image-output-type "svg"
       :image-size-adjust (1.7 . 1.5)
       :latex-compiler
       ("latex -interaction nonstopmode -output-directory %o %f")
       :image-converter
       ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))))


;;; Match the text baseline of an LaTeX fragment to the surrounding text
(defun pes-org--latex-header-preview (orig &rest args)
  "Setup dedicated `org-format-latex-header' to `pes-org--match-text-baseline-ascent'."
  (let ((org-format-latex-header
          "\\documentclass[preview]{standalone}
\\usepackage[usenames]{color}
\\newcommand{\\vect}[1]{\\textit{\\textbf{#1}}}
\\newcommand{\\diff}{\\mathop{}\\!\\mathrm{d}}
\\newcommand{\\Diff}{\\mathop{}\\!\\mathrm{D}}
[DEFAULT-PACKAGES]
[PACKAGES]"))
    (apply orig args)))

(defun pes-org--match-text-baseline-ascent (imagefile)
  "Set `:ascent' to match the text baseline of an image to the surrounding text.
Calculate `ascent' with the data collected in IMAGEFILE."
  (advice-add 'org-create-formula-image :around #'pes-org--latex-header-preview)
  (let* ((viewbox (split-string
                    (xml-get-attribute (car (xml-parse-file imagefile)) 'viewBox)))
          (depth (string-to-number (nth 1 viewbox)))
          (height (string-to-number (nth 3 viewbox)))
          ;; The baseline seems to tend to sit slightly
          ;; lower than it should be, and a very mild
          ;; bias seems to improve the visual result.
          ;; From testing with a collecting of LaTeX
          ;; maths fonts (cm, cmbright, arev, pxfonts,
          ;; notomath, nextxsf, eulervm) decreacing the
          ;; depth measurement by 0.02pt in the baseline
          ;; calculation seems to work well.
          ;; I have yet to come across any situation
          ;; where this results in a negative depth,
          ;; however we may as well ensure that never
          ;; occurs.
          (ascent (round (* -100 (/ (max 0.0 (- depth 0.02) height))))))
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


;; Org LaTeX packages
(setq org-latex-packages-alist
  '(
     ("" "mathtools" t)
     ("" "siunitx" t)
     ("version=4" "mhchem" )
     ("" "mlmodern" t)))


;; Ensure LaTeX fragments can be displayed correctly on dark backgrounds
(setq org-format-latex-options
  '(
     :foreground default
     :background "Transparent"
     :scale 1.65
     :html-foreground default
     :html-background "Transparent"
     :html-scale 1.2
     :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))


;;; Better LaTeX editor for Org mode
;; Setup `CDLaTeX'
(use-package cdlatex :ensure t
  :hook
  (
    (LaTeX-mode . turn-on-cdlatex)
    (org-mode . turn-on-org-cdlatex)))

;; Syntax highlighting for LaTeX in Org mode
(setq org-highlight-latex-and-related '(latex script))


(use-package ox-latex)
(setq-default org-latex-compiler "xelatex")


(provide 'init-tex)
;;; init-tex.el ends here
