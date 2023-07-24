;; init-comp.el ---  Modern template system -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Despite `Yasnippet' is not been frequently updated, it still got its high
;; compatibilities. (TODO)
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configure Tempel
;; (use-package tempel
;;   :ensure t
;;   :bind
;;   (("s-." . tempel-expand)
;;    ("s-/" . tempel-insert))
;;   :init ; Setup completion at point
;;   (defun tempel-setup-capf ()
;;     "Add the Tempel Capf to `completion-at-point-functions'."
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-expand
;;                       completion-at-point-functions)))
;;
;;   (add-hook 'org-mode-hook #'tempel-setup-capf)
;;   :config
;;   (setq tempel-path "~/.emacs.d/temp.eld"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configure `Yasnippet'
(use-package yasnippet
  :ensure t
  :config
  (setq yas-visit-from-menu t)
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

;; Functional snippets
(defun insert-latex-matrix ()
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
           ("C-c i m" . insert-latex-matrix))

(provide 'init-temp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-temp ends here
