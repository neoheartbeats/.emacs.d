;;; init-temp.el --- Configure Tempel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package tempel
  :init ;; Setup completion at point
  (defun tempel-setup-capf ()
    "Add the Tempel Capf to `completion-at-point-functions'"
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'org-mode-hook 'tempel-setup-capf)
  :custom ;; Require trigger prefix when completing
  (tempel-trigger-prefix "\\")
  (tempel-path (concat user-emacs-directory "templates/*.eld"))
  :bind
  (("s-." . tempel-expand)
   (:map tempel-map
         ("RET" . tempel-done)
         ("<tab>" . tempel-next))))


(provide 'init-temp)
;;; init-temp ends here
