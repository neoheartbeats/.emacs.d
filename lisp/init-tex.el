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


;; Setup `dvisvgm' to preview LaTeX fragments
;; (setq org-preview-latex-default-process 'dvisvgm)
;; (setq org-preview-latex-process-alist
;;       '((dvisvgm
;;          :programs ("xelatex" "dvisvgm")
;;          :description "xdv > svg"
;;          :image-input-type "xdv"
;;          :image-output-type "svg"
;;          :image-size-adjust (1.7 . 1.5)
;;          :post-clean (".dvi" ".xdv" ".tex" ".aux" ".svg" ".png" ".jpg" ".jpeg" ".out")
;;          :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o '\\PassOptionsToPackage{active,tightpage,auctex}{preview}\\input{%f}' %f")
;;          :image-converter ("dvisvgm %f -e -n -b 1 -c %S -o %O"))))


;; Org LaTeX packages
(setq org-latex-packages-alist
      '(("" "mathtools" t)
        ("" "physics" t)
        ("version=4" "mhchem" t)
        ("" "siunitx" t)
        ("displaymath,floats,graphics,textmath,footnotes" "preview" t)))


;; (setq org-format-latex-options
;;       '( ;; Ensure LaTeX fragments can be displayed correctly on dark backgrounds
;; 	:foreground default
;;         :background "Transparent"
;;         :scale 3.5
;;         :html-foreground default
;;         :html-background "Transparent"
;;         :html-scale 1.2
;;         :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))


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


;; (require 'preview)

;; (defun my/org--match-text-baseline-ascent ()
;;   (let* ((tmp (file-name-with-extension "tmp" "log"))
;;          (regexp-string "^! Preview:.*\(\\([0-9]*?\\)\\+\\([0-9]*?\\)x\\([0-9]*\\))")
;;          logfile ascent bbox log)
;;     (if (eq (car (directory-files temporary-file-directory t
;;                                   "orgtex\.\*\\\.log"
;;                                   #'file-newer-than-file-p)) nil)
;;         (setq-local ascent 'center)
;;       (progn
;;         (setq logfile (car (directory-files temporary-file-directory t "orgtex\.\*\\\.log"))))
;;       (with-temp-file tmp
;;         (insert-file-contents-literally logfile)
;;         (goto-char (point-max))
;;         (if (re-search-backward regexp-string nil t)
;;             (progn
;;               (setq log (match-string 0))
;;               (setq bbox (mapcar #'(lambda (x)
;;                                      (* (preview-get-magnification)
;;                                         (string-to-number x)))
;;                                  (list (match-string 1)
;;                                        (match-string 2)
;;                                        (match-string 3))))
;;               (erase-buffer)
;;               (insert log))))
;;       ;; (delete-file logfile)
;;       ;; (delete-file tmp)
;;       (setq-local ascent (preview-ascent-from-bb (preview-TeX-bb bbox))))))

;; (defun org--make-preview-overlay (beg end image &optional imagetype)
;;   "Build an overlay between BEG and END using IMAGE file.
;; Argument IMAGETYPE is the extension of the displayed image,
;; as a string.  It defaults to \"png\"."
;;   (my/org--match-text-baseline-ascent)
;;   (let ((ov (make-overlay beg end))
;; 	(imagetype (or (intern imagetype) 'png)))
;;     (overlay-put ov 'org-overlay-type 'org-latex-overlay)
;;     (overlay-put ov 'evaporate t)
;;     (overlay-put ov
;; 		 'modification-hooks
;; 		 (list (lambda (o _flag _beg _end &optional _l)
;; 			 (delete-overlay o))))
;;     (overlay-put ov
;; 		 'display
;; 		 (list 'image :type imagetype :file image :ascent ascent))))


(use-package xenops
  :init
  (setq xenops-math-image-scale-factor 3.5)
  :config
  (setq xenops-math-latex-process-alist
        '((dvisvgm :programs
                   ("latex" "dvisvgm")
                   :description "dvi > svg"
                   :image-input-type "dvi"
                   :image-output-type "svg"
                   :image-size-adjust
                   (1.7 . 1.5)
                   :latex-compiler
                   ("latex -interaction nonstopmode -shell-escape -output-format dvi -output-directory %o \"\\nonstopmode\\nofiles\\PassOptionsToPackage{active,tightpage,auctex}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined\\RequirePackage[displaymath,floats,graphics,textmath,footnotes]{preview}[2004/11/05]\\fi}\\input\\detokenize{\"%f\"}\" %f")
                   :image-converter
                   ("dvisvgm %f -e -n -b 1 -c %S -o %O"))))

  (defun xenops-aio-subprocess (command &optional _ __)
    "Start asynchronous subprocess; return a promise.

COMMAND is the command to run as an asynchronous subprocess.

Resolve the promise when the process exits. The value function
does nothing if the exit is successful, but if the process exits
with an error status, then the value function signals the error."
    (let* ((promise (aio-promise))
           (name (format "xenops-aio-subprocess-%s"
                         (sha1 (prin1-to-string command))))
           (output-buffer (generate-new-buffer name))
           (sentinel (lambda (process event)
                       (unless (process-live-p process)
                         (aio-resolve
                          promise
                          (lambda ()
                            (if (or (eq 0 (process-exit-status process))
                                    (and (eq 1 (process-exit-status process))
                                         (not (string-match-p
                                               "^! [^P]"
                                               (with-current-buffer output-buffer
                                                 (buffer-string))))))
                                (kill-buffer output-buffer)
                              (signal 'error
                                      (prog1 (list :xenops-aio-subprocess-error-data
                                                   (list (s-join " " command)
                                                         event
                                                         (with-current-buffer output-buffer
                                                           (buffer-string))))
                                        (kill-buffer output-buffer))))))))))
      (prog1 promise
        (make-process
         :name name
         :buffer output-buffer
         :command command
         :sentinel sentinel))))
  
  (defun eli/xenops-preview-align-baseline (element &rest _args)
    "Redisplay SVG image resulting from successful LaTeX compilation of ELEMENT.

Use the data in log file (e.g. \"! Preview: Snippet 1 ended.(368640+1505299x1347810).\")
to calculate the decent value of `:ascent'. "
    (let* ((inline-p (eq 'inline-math (plist-get element :type)))
           (ov-beg (plist-get element :begin))
           (ov-end (plist-get element :end))
           (colors (xenops-math-latex-get-colors))
           (latex (buffer-substring-no-properties ov-beg
                                                  ov-end))
           (cache-svg (xenops-math-compute-file-name latex colors))
           (cache-log (file-name-with-extension cache-svg "log"))
           (cache-log-exist-p (file-exists-p cache-log))
           (tmp-log (f-join temporary-file-directory "xenops"
                            (concat (f-base cache-svg) ".log")))
           (ov (car (overlays-at (/ (+ ov-beg ov-end) 2) t)))
           (regexp-string "^! Preview:.*\(\\([0-9]*?\\)\\+\\([0-9]*?\\)x\\([0-9]*\\))")
           img new-img ascent bbox log-text log)
      (when (and ov inline-p)
        (if cache-log-exist-p
            (let ((text (f-read-text cache-log)))
              (string-match regexp-string text)
              (setq log (match-string 0 text))
              (setq bbox (mapcar #'(lambda (x)
                                     (* (preview-get-magnification)
                                        (string-to-number x)))
                                 (list
                                  (match-string 1 text)
                                  (match-string 2 text)
                                  (match-string 3 text)))))
          (with-temp-file cache-log
            (insert-file-contents-literally tmp-log)
            (goto-char (point-max))
            (if (re-search-backward regexp-string nil t)
                (progn
                  (setq log (match-string 0))
                  (setq bbox (mapcar #'(lambda (x)
                                         (* (preview-get-magnification)
                                            (string-to-number x)))
                                     (list
                                      (match-string 1)
                                      (match-string 2)
                                      (match-string 3))))))
            (erase-buffer)
            (insert log)))
        (setq ascent (preview-ascent-from-bb (preview-TeX-bb bbox)))
        (setq img (cdr (overlay-get ov 'display)))
        (setq new-img (plist-put img :ascent ascent))
        (overlay-put ov 'display (cons 'image new-img)))))
  (advice-add 'xenops-math-display-image :after
              #'eli/xenops-preview-align-baseline)
  :hook
  (org-mode . xenops-mode))


(provide 'init-tex)
;;; init-tex.el ends here
