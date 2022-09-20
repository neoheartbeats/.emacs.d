;;; init-yasnippet.el --- Simple templates for Emacs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :custom
  (yas-triggers-in-field t)
  (yas-visit-from-menu t)
  :config
  (yas-global-mode 1))


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
