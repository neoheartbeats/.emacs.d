;;; init-corfu.el --- Completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)


(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(when (maybe-require-package 'orderless)
  (require 'orderless)
  (setq-default completion-styles '(orderless basic)))

(setq completion-category-defaults nil)
(setq completion-category-overrides nil)

(setq completion-cycle-threshold 4)
(setq completion-ignore-case t)


(when (maybe-require-package 'cape)
  (push 'cape-dabbrev completion-at-point-functions)
  (push 'cape-file completion-at-point-functions)
  (push 'cape-dict completion-at-point-functions)

  (with-eval-after-load 'cape
    (setq cape-dabbrev-min-length 3)))


(when (maybe-require-package 'corfu)
  (setq-default corfu-auto t)
  (setq-default corfu-auto-delay 0)
  (setq-default corfu-auto-prefix 2)
  (setq-default corfu-cycle t)
  (setq-default corfu-quit-at-boundary t)
  (setq-default corfu-quit-no-match 'separator)
  (setq-default corfu-preselect 'first)

  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook #'(lambda () (setq-local corfu-auto nil))))

  (with-eval-after-load 'org
    (add-hook
     'org-mode-hook
     #'(lambda ()
         (setq-local corfu-auto-prefix 1)
         (setq-local completion-styles '(basic)))))

  (add-hook 'after-init-hook #'global-corfu-mode)

  (with-eval-after-load 'corfu
    (corfu-popupinfo-mode 1)
    (corfu-history-mode 1)

    (define-key corfu-map (kbd "<down>") 'corfu-next)
    (define-key corfu-map (kbd "<up>") 'corfu-previous)
    (define-key corfu-map (kbd "<space>") 'corfu-quit)
    (define-key corfu-map (kbd "<escape>") 'corfu-quit)))


(provide 'init-corfu)
;;; init-corfu.el ends here
