;;; init-temp.el --- Configure Tempel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package yasnippet :straight t
  :diminish (yas-minor-mode)
  :config
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets/" user-emacs-directory)))
  (setq yas-triggers-in-field t)
  (setq yas-visit-from-menu t)
  (yas-global-mode 1))


(provide 'init-temp)
;;; init-temp ends here
