;;; init-minibuff.el --- Config for minibuffer completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init (vertico-mode)
  :config
  
  ;; Load extensions
  (require 'vertico-directory)

  ;; Correct file path when changed
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (setq vertico-count 15)
  (setq vertico-cycle t)

  ;; Do not render italic fonts
  (set-face-attribute 'vertico-group-title nil :slant 'normal)
  :bind
  ((:map vertico-map
     ("<tab>" . vertico-insert)
     ("<return>" . vertico-directory-enter)
     ("<backspace>" . vertico-directory-delete-char))))

(use-package consult :straight t
  :init
  (global-set-key (kbd "s-b") 'switch-to-buffer)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key
    [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key
    [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line)
  :bind
  (
  	("C-s" . consult-line)
    ("M-s" . consult-ripgrep)))

(use-package embark :straight t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (
  	("M-." . embark-dwim)
    ("C-h b" . embark-bindings)))

(use-package embark-consult :straight t
  :after (embark consult)
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enable rich annotations
(use-package marginalia :straight t
  :init (marginalia-mode 1))

(provide 'init-minibuff)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-minibuff.el ends here
