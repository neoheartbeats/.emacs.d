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
(use-package vertico :ensure t
;;  :ensure (:files (:defaults "extensions/*"))
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

(use-package savehist
  :init
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (savehist-mode))

(use-package consult :ensure t
  :init
  (global-set-key (kbd "s-b") 'switch-to-buffer)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key
   [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key
   [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line)
  :bind
  (("C-s" . consult-line)
   ("M-s" . consult-ripgrep)))

(use-package embark :ensure t
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (("M-." . embark-dwim)
   ("C-h b" . embark-bindings)))

(use-package embark-consult :ensure t
  :after (embark consult)
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enable rich annotations
(use-package marginalia :ensure t
  :init (marginalia-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion in buffers
(setq tab-always-indent 'complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add extensions
(use-package cape :ensure t
  :config (setq cape-dabbrev-min-length 5)
  :hook
  ((emacs-lisp-mode . (lambda ()
                        (push 'cape-dabbrev completion-at-point-functions)
                        (push 'cape-file completion-at-point-functions)
                        (push 'cape-symbol completion-at-point-functions)
                        (push 'cape-keyword completion-at-point-functions)))
   (org-mode . (lambda ()
                 (push 'cape-dabbrev completion-at-point-functions)
                 (push 'cape-file completion-at-point-functions)
                 (push 'cape-dict completion-at-point-functions)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Build the completion framework
(use-package orderless :ensure t
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-override nil)
  (setq completion-cycle-threshold 5)
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t))

(use-package corfu :ensure t
;;  :ensure (:files (:defaults "extensions/*"))
  :init (add-hook 'after-init-hook #'global-corfu-mode)
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0)
  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t)
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match 'separator)
  (setq corfu-preselect 'first)
  (setq corfu-history-mode 1)
  (setq corfu-popupinfo-mode 1)
  :hook
  (eshell-mode .  (lambda ()
                    (setq-local corfu-auto nil)))
  (org-mode . (lambda ()
                (setq-local completion-styles '(basic))))
  :bind
  (:map corfu-map
        ("<down>" . corfu-next)
        ("<up>" . corfu-previous)
        ("<space>" . corfu-quit)
        ("<escape>" . corfu-quit)))


(provide 'init-comp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-comp.el ends here
