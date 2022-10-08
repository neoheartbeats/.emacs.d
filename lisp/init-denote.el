;;; init-denote.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Simple notes for Emacs with an efficient file-naming scheme.
;; Start your life here.

;;; Code:

(use-package denote
  :init
  (setq denote-org-front-matter
    "#+TITLE: %1$s\n#+FILETAGS: %3$s\n#+IDENTIFIER: \(%4$s\)\n\n")
  :custom
  (denote-directory org-directory)
  (denote-known-keywords '("篝火" "图书馆之梦" "里彭大圣堂" "计时塔"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompts '(title keywords subdirectory))
  :config
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  
  (defvar my-date (format-time-string "%Y-%m-%d"))
  (defvar my-file (concat
                    (expand-file-name "repos/" denote-directory)
                    "20221009T042321--圣森__篝火.org"))

  ;; Custom functions
  (defun my/denote-date ()
    "Create an entry tagged '篝火' with the date as its title."
    (interactive)
    (denote
      (concat "(" (format-time-string "%A-") my-date ")") ; Format like "(Tuesday-2004-07-10)"
      '("篝火")
      nil
      (expand-file-name "dates" denote-directory))
    (setq my-date-filename (buffer-file-name)))

  (defun my/denote-file ()
    "Visit your home."
    (interactive)
    (find-file my-file))

  ;; Custom keys
  (global-set-key (kbd "C-c j") 'my/denote-date)
  (global-set-key (kbd "s-d") 'denote)
  (global-set-key (kbd "s-m") 'my/denote-file)
  (global-set-key (kbd "s-j") (lambda ()
                                (interactive)
                                (find-file (expand-file-name "dates/" denote-directory))))
  (global-set-key (kbd "s-k") (lambda ()
                                (interactive)
                                (find-file (expand-file-name "notes/" denote-directory))))
  (global-set-key (kbd "s-l") (lambda ()
                                (interactive)
                                (find-file (expand-file-name "repos/" denote-directory))))
  :hook
  (after-init . my/denote-file))


(provide 'init-denote)
;;; init-denote.el ends here
