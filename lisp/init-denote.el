;;; init-denote.el --- Note-taking implement -*- lexical-binding: t -*-
;;; Commentary:

;; This file configures Denote & its backend `dired' is covered.
;; This package is created by the philosopher Protesilaos.
;; Check out https://protesilaos.com/emacs/denote/.
;; The key concept of this function is based on Zettelkasten Method.

;;; Code:

(use-package denote
  :init
  (setq denote-org-front-matter
    "#+TITLE: %1$s\n#+FILETAGS: %3$s\n#+IDENTIFIER: %4$s\n\n")
  :custom
  (denote-directory "/Users/ilyaw39/Dropbox/大家好/")
  (denote-known-keywords '("篝火" "图书馆之梦" "里彭大圣堂" "项目"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t) ; Pick dates with Org's interface
  (denote-link-fontify-backlinks t)
  (denote-dired-directories
    (list denote-directory
      (thread-last denote-directory (expand-file-name "dates"))
      (thread-last denote-directory (expand-file-name "notes"))
      (thread-last denote-directory (expand-file-name "repos"))
      (thread-last denote-directory (expand-file-name "projs"))))
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (defun my/denote-date ()
    "Create an entry tagged '篝火' with the `date' as its title."
    (interactive)
    (setq date (format-time-string "%Y-%m-%d"))
    (setq title (concat (format-time-string "%A ") date))
    (denote title ; format like "Sunday 2022-07-10"
      '("篝火") nil ; By default the file type is that of Org mode
      (thread-last denote-directory (expand-file-name "dates")) date nil))
  :bind
  (("s-d" . denote)))

(setq initial-buffer-choice
  "/Users/ilyaw39/Dropbox/大家好/dates/20221004T005520--tuesday-2022-10-04__篝火.org")


(provide 'init-denote)
