;;; init-minibuff.el --- Config for minibuffer completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'vertico)
  (add-hook 'after-init-hook #'vertico-mode)

  (with-eval-after-load 'vertico
    (setq vertico-count 10)
    (setq vertico-cycle t)

    ;; Correct file path when changed
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

    (define-key vertico-map (kbd "<tab>") 'vertico-insert)
    (define-key vertico-map (kbd "RET") 'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") 'vertico-directory-delete-char)
    (define-key vertico-map (kbd "s-DEL") 'vertico-directory-delete-word)

    ;; Customize Consult buffers
    (set-face-attribute 'vertico-group-title nil :slant 'normal)

    ;; Persist history over Emacs restarts
    (require 'savehist)
    (savehist-mode 1))

  (when (maybe-require-package 'embark)
    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "C-c C-o") 'embark-export)
      (define-key vertico-map (kbd "C-c C-c") 'embark-act)))

  (define-key global-map (kbd "M-.") 'embark-dwim)

  (when (maybe-require-package 'consult)
    (defmacro pes-no-consult-preview (&rest cmds)
      `(with-eval-after-load 'consult
         (consult-customize ,@cmds :preview-key (kbd "M-P"))))

    (pes-no-consult-preview
     consult-ripgrep
     consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref)

    (global-set-key (kbd "s-b") 'switch-to-buffer)

    (global-set-key [remap switch-to-buffer]
                    'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window]
                    'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame]
                    'consult-buffer-other-frame)
    (global-set-key [remap goto-line]
                    'consult-goto-line)

    (define-key global-map (kbd "s-l") 'consult-line)
    (define-key global-map (kbd "M-s") 'consult-ripgrep))

  (when (maybe-require-package 'embark-consult)
    (with-eval-after-load 'embark
      (require 'embark-consult)
      (add-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)))

  (when (and (executable-find "rg") (maybe-require-package 'affe))
    (defun pes-affe-grep-at-point (&optional dir initial)
      (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                      (symbol-name s))))
      (affe-grep dir initial))
    (global-set-key (kbd "M-?") 'pes-affe-grep-at-point)
    (pes-no-consult-preview pes-affe-grep-at-point)
    (with-eval-after-load 'affe
      (pes-no-consult-preview affe-grep))))

(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode))


(provide 'init-minibuff)
;;; init-minibuff.el ends here
