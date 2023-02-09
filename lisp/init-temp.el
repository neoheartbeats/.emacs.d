;;; init-temp.el --- Configure Tempel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(when (maybe-require-package 'yasnippet)
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets/" user-emacs-directory)))
  (setq yas-triggers-in-field t)
  (setq yas-visit-from-menu t)

  (yas-global-mode 1)
  (with-eval-after-load 'yasnippet
    (diminish 'yas-minor-mode)))


(provide 'init-temp)
;;; init-temp ends here
