;; init-corfu.el --- Completion in buffers -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
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
                (setq-local corfu-auto-delay 0)
                (setq-local corfu-auto-prefix 1)
                (setq-local completion-styles '(basic))))
  :bind
  (
    :map corfu-map
    ("<down>" . corfu-next)
    ("<up>" . corfu-previous)
    ("<space>" . corfu-quit)
    ("<escape>" . corfu-quit)))


(provide 'init-corfu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-corfu.el ends here
