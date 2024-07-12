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
  :config
  (define-abbrev-table 'global-abbrev-table
    '(("ciallo" "Ciallo～(∠・ω< )⌒☆" nil :count 1)))

  (abbrev-mode 1))


;; Yasnippet
;;

(use-package yasnippet
  :straight t
  :diminish (yas-minor-mode)
  :config
  (setq yas-triggers-in-field t)
  (setq yas-visit-from-menu t)
  (yas-global-mode 1))

;; YASnippet functions
(defun my/yas-insert-latex-matrix ()
  "Insert a LaTeX matrix at the current cursor position."
  (interactive)
  (let* ((rows (read-number "# Rows: "))
         (cols (read-number "# Columns: "))
         (counter 1)
         (snippet (concat "\\begin{pmatrix}\n")))
    (dotimes (_ rows)
      (dotimes (j cols)
        (when (> j 0)
          (setq snippet (concat snippet " & ")))
        (setq snippet (concat snippet "${" (number-to-string counter) "}"))
        (setq counter (1+ counter)))
      (setq snippet (concat snippet "\\" "\\\\"  "\n")))
    (setq snippet (concat snippet "\\end{pmatrix}"))
    (yas-expand-snippet snippet)))

(bind-keys :map org-mode-map
           ("s-i m" . my/yas-insert-latex-matrix))

(provide 'init-temp)
