;;; init-tex.el

;;; AUCTeX
(use-package tex
  :ensure auctex
  :defer t
  :hook
  ((LaTeX-mode . TeX-PDF-mode)
   (LaTeX-mode . TeX-source-correlate-mode)
   (LaTeX-mode . turn-on-reftex)
   (LaTeX-mode . (lambda ()
                   (setq-local compilation-scroll-output t)
                   (setq-local TeX-command-extra-options
                               "-interaction=nonstopmode -synctex=1 -file-line-error"))))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil)
  (setq TeX-source-correlate-start-server t
        TeX-source-correlate-method 'synctex)
  (setq TeX-command-default "LateXMk")

  ;; `pdf-tools' for AUCTeX PDF preview
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list
        '(("PDF Tools" TeX-pdf-tools-sync-view))))

;; FIXME: CDLaTeX (may be conflict with `texlab', see `init-eglot')
;; (use-package cdlatex
;;   :ensure t
;;   :hook (LaTeX-mode . turn-on-cdlatex))


(use-package reftex
  :defer t
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-use-multiple-selection-buffers t))

(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-tools-install)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init (pdf-tools-install)
  :config
  (use-package pdf-sync :after pdf-tools)
  (setq pdf-view-display-size 'fit-width)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil))

(defun sthenno/latexindent-buffer ()
  "Format current buffer with latexindent (if available)."
  (interactive)
  (when (and (executable-find "latexindent")
             (derived-mode-p 'latex-mode 'LaTeX-mode))
    (let ((orig-point (point)))
      (shell-command-on-region
       (point-min) (point-max)
       "latexindent -g /tmp/latexindent.log"
       (current-buffer) t)
      (goto-char orig-point))))

(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (add-hook 'before-save-hook #'sthenno/latexindent-buffer nil t)))

(provide 'init-tex)
