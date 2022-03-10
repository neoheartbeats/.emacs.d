;; init-term.el --- Lyrith: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Emacs-libvterm setup.
;;
;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup vterm
;;
;; vterm config
(use-package vterm
  :commands vterm-mode
  :hook
  (vterm-mode . hide-mode-line-mode)
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 3000))

(provide 'init-term)

;; init-term.el ends here
