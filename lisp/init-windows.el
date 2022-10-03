;;; init-windows.el --- Windows management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Allow redo changes in different windows
(winner-mode 1)


(global-unset-key (kbd "C-x o"))

(global-set-key (kbd "C-x C-n") 'next-window)
(global-set-key (kbd "C-x C-p") 'previous-window)

(global-set-key (kbd "s-e") 'delete-window)


(provide 'init-windows)
;;; init-windows ends here
