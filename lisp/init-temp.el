;;; init-temp.el --- The template system for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Using `abbrev' (built-in)
(use-package abbrev
  :straight (:type built-in)
  :diminish (abbrev-mode)
  :custom
  (save-abbrevs 'silent)
  :hook
  (org-mode . abbrev-mode))


;; Using `dabbrev' (built-in)
(use-package dabbrev
  :straight (:type built-in)
  :custom ;; Other useful Dabbrev configurations
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


;; Using `tempel'
(use-package tempel
  :init ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'org-mode-hook 'tempel-setup-capf)
  :custom
  (tempel-path (expand-file-name "templates.lsp" user-emacs-directory))
  (tempel-trigger-prefix "\\")

  ;; Hook Tempel into the Abbrev mechanism
  (global-tempel-abbrev-mode 1)
  :bind
  ((:map tempel-map
         ("RET" . tempel-done)
         ("<right>" . tempel-next)
         ("<left>" . tempel-previous))))


(provide 'init-temp)
;;; init-temp.el ends here
