;;; init-temp.el --- Modern template system -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; TODO: This file includes:
;; -
;;

;;; Code:
;;


;; TODO: Abbrevs
;;
(use-package abbrev
  :init
  (setq abbrev-file-name (expand-file-name "abbrev-defs.el" user-emacs-directory))

  ;; Do not ask before saving abbrevs
  (setq save-abbrevs 'silently)

  ;; Hooks
  (add-hook 'text-mode-hook #'(lambda ()
                                (abbrev-mode 1)))
  (add-hook 'prog-mode-hook #'(lambda ()
                                (abbrev-mode 1)))
  :bind (:map global-map
              ("C-x a e" . edit-abbrevs)
              ("C-x a a" . add-global-abbrev)))


;; Yasnippet
;;
(use-package yasnippet
  :ensure t
  :defer 2
  :config
  (setq yas-triggers-in-field t)
  (setq yas-visit-from-menu t)
  (yas-global-mode 1))

;; YASnippet functions
;; (defun sthenno/yas-insert-latex-matrix ()
;;   "Insert a LaTeX matrix at the current cursor position."
;;   (interactive)
;;   (let* ((rows (read-number "# Rows: "))
;;          (cols (read-number "# Columns: "))
;;          (counter 1)
;;          (snippet (concat "\\begin{pmatrix}\n")))
;;     (dotimes (_ rows)
;;       (dotimes (j cols)
;;         (when (> j 0)
;;           (setq snippet (concat snippet " & ")))
;;         (setq snippet (concat snippet "${" (number-to-string counter) "}"))
;;         (setq counter (1+ counter)))
;;       (setq snippet (concat snippet "\\" "\\\\"  "\n")))
;;     (setq snippet (concat snippet "\\end{pmatrix}"))
;;     (yas-expand-snippet snippet)))

;; (bind-keys :map org-mode-map
;;            ("s-i m" . sthenno/yas-insert-latex-matrix))

(provide 'init-temp)
