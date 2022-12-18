;;; init-snippets.el --- Template system for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :custom

  ;; Visit snippet files from menu
  (yas-visit-from-menu t)

  ;; Allow stacked expansions
  (yas-triggers-in-field t)
  :config
  (yas-global-mode 1))


(provide 'init-snippets)
;;; init-snippets.el ends here
