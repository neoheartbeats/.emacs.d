;; init-comp.el ---  Modern completion system -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion for minibuffers
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
  (set-face-attribute 'vertico-group-title nil
    :slant 'normal)
  :bind
  (
    (:map vertico-map
      ("<tab>" . vertico-insert)
      ("<return>" . vertico-directory-enter)
      ("<backspace>" . vertico-directory-delete-char))))

(use-package consult
  :straight t
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

(use-package embark
  :straight t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (
  	("M-." . embark-dwim)
    ("C-h b" . embark-bindings)))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enable rich annotations
(use-package marginalia
  :straight t
  :init (marginalia-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion in buffers
(setq tab-always-indent 'complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add extensions
(use-package cape
  :straight t
  :config (setq cape-dabbrev-min-length 5)
  :hook
  (
    (emacs-lisp-mode . (lambda ()
                         (push 'cape-file completion-at-point-functions)
                         (push 'cape-symbol completion-at-point-functions)
                         (push 'cape-keyword completion-at-point-functions)))
    (org-mode . (lambda ()
                  (push 'cape-file completion-at-point-functions)
                  (push 'cape-dict completion-at-point-functions)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Build the completion framework
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-override nil)
  (setq completion-cycle-threshold 5)
  (setq completion-ignore-case t))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :init
  (add-hook 'after-init-hook #'(lambda ()
                                 (global-corfu-mode 1)))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'first)  
  :config
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  :hook
  (eshell-mode .  (lambda ()
                    (setq-local corfu-auto nil)))
  (org-mode . (lambda ()
                (setq-local corfu-auto-delay 0.15)
                (setq-local corfu-auto-prefix 2)
                (setq-local completion-styles '(basic))))
  :bind
  (:map corfu-map
    ("<down>" . corfu-next)
    ("<up>" . corfu-previous)
    ("<space>" . corfu-quit)
    ("<escape>" . corfu-quit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sorting and filtering with `prescient.el'
(use-package prescient
  :straight t)

(use-package vertico-prescient
  :straight t
  :config
  (vertico-prescient-mode 1))

(use-package corfu-prescient
  :straight t
  :config
  (corfu-prescient-mode 1))


(provide 'init-comp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-comp.el ends here
