;;; init-temp.el --- Simple templates for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
  :config
  (defun my/tab-maybe-expand ()
    "Try to `org-cycle', `tempel-expand' at current cursor position.
  If all failed, try to complete the common part with `indent-for-tab-command'."
    (interactive)
    (when (featurep 'tempel)
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (func-list
             (if (equal major-mode 'org-mode) '(tempel-expand tempel-next org-cycle)
               '(tempel-expand tempel-next))))
        (catch 'func-suceed
          (dolist (func func-list)
            (ignore-errors (call-interactively func))
            (unless (and (eq old-point (point))
                         (eq old-tick (buffer-chars-modified-tick)))
              (throw 'func-suceed t)))
          (indent-for-tab-command)))))
  :bind
  ((:map org-mode-map
         ("<tab>" . my/tab-maybe-expand)
         ("s-." . tempel-insert))))


(provide 'init-temp)
;;; init-temp.el ends here
