;;; init-corfu.el --- Completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :init
  (setq-default corfu-auto t)
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda ()
                                  (setq-local corfu-auto nil))))
  :custom
  (corfu-auto-delay 0)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match 'separator)
  (corfu-cycle t)

  ;; Cycling is used as long as number of candidates is
  ;; less than 5
  (completion-cycle-threshold 5)
  :config ;; Remember the latest choice
  (use-package corfu-history
    :straight nil
    :after corfu
    :config
    (corfu-history-mode 1))
  :bind
  (:map corfu-map
        ("<down>" . corfu-next)
        ("<up>" . corfu-preselect)
        ("<tab>" . corfu-quit)
        ("<escape>" . corfu-quit))
  :hook
  ((prog-mode org-mode) . corfu-mode))


;; Setup `cape-dict-file'
(setq cape-dict-file
      (expand-file-name "dict/dict-en-utf8.txt" user-emacs-directory))


;; Add extensions
(use-package cape
  :init ;; Add `completion-at-point-functions', used by `completion-at-point'
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))


(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  :config
  (setq completion-category-defaults nil)
  (setq completion-category-override nil))


(provide 'init-corfu)
;;; init-corfu.el ends here
