;;; init-denote.el --- Publishing my blog post -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ox-hugo
  :after ox
  :custom
  (org-hugo-base-dir (expand-file-name "sites" org-directory))
  :bind
  ("M-o h" . org-hugo-export-to-md))

(use-package denote
  :init
  (setq denote-org-front-matter "#+TITLE: %1$s\n#+FILETAGS: %3$s\n")
  :custom
  (denote-directory org-directory)
  (denote-known-keywords '("图书馆之梦" "童话库" "里彭大圣堂" "卡罗尔川"))
  (denote-infer-keywords t)
  (denote-prompts '(title keywords subdirectory))
  (denote-date-format "%Y-%m-%d")
  :config
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  
  ;; My dates
  (defun my/denote-date ()
    "Create an entry tagged '图书馆之梦' with the date as its title."
    (interactive)
    (denote
     (format-time-string "%A [%Y-%m-%d]") ; Format like Sunday [2022-07-10]
     '("图书馆之梦")
     nil
     (expand-file-name "dates/" denote-directory)))
  (defun my/denote-site ()
    "Create an entry tagged '卡罗尔川' with the date as its title."
    (interactive)
    (require 'ox-hugo)
    (denote
     (denote-title-prompt)
     '("卡罗尔川")
     nil
     (expand-file-name "sites/content-org/" denote-directory))
    (insert "#+OPTIONS: tex:dvisvgm\n\n")))

(setq initial-buffer-choice
      (expand-file-name "notes/20221021T182314--比尔和诺登__里彭大圣堂.org" denote-directory))


(provide 'init-denote)
;;; init-denote.el ends here
