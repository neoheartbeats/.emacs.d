;;; init-corfu.el --- Completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)


;; Add extensions
(use-package cape
  :init ;; Add `completion-at-point-functions', used by `completion-at-point'
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config ;; Setup `cape-dict-file'
  (setq cape-dict-file
        (expand-file-name "dict/dict-en-utf8.txt" user-emacs-directory)))


(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic))
  (setq completion-category-defaults nil)
  (setq completion-category-override nil))

(use-package corfu
  :init
  (global-corfu-mode 1)
  :config

  ;; Load extensions
  (require 'corfu-history)

  ;; Remember the latest choice
  (corfu-history-mode 1)

  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t)
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match 'separator)
  (setq corfu-preselect 'prompt)

  (with-eval-after-load 'org
    ;; Aggressive completion, cheap prefix filtering
    (add-hook 'org-mode-hook (lambda ()
                               (setq-local corfu-auto-delay 0)
                               (setq-local corfu-auto-prefix 3)
                               (setq-local completion-styles '(basic)))))
  :bind
  (:map corfu-map
        ("<down>" . corfu-next)
        ("<up>" . corfu-previous)
        ("<tab>" . corfu-quit)
        ("<escape>" . corfu-quit)))


(provide 'init-corfu)
;;; init-corfu.el ends here
