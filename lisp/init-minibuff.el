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
    (global-set-key (kbd "s-b") 'switch-to-buffer)
    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key
     [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key
     [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)

    (define-key global-map (kbd "C-s") 'consult-line)
    (define-key global-map (kbd "M-s") 'consult-ripgrep))

  (when (maybe-require-package 'embark-consult)
    (with-eval-after-load 'embark
      (require 'embark-consult)
      (add-hook
       'embark-collect-mode-hook #'embark-consult-preview-minor-mode))))

(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode))


(provide 'init-minibuff)
;;; init-minibuff.el ends here
