;;; init-org-roam.el --- Org Roam configuration  -*- lexical-binding: t -*-
;;; Commentary:

;; This file must be loaded after `init-org.el'.

;;; Code:

(use-package emacsql-sqlite-builtin
  :defer t)

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam")
  :defer t
  :custom
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-location (expand-file-name "org-roam.db" org-directory))
  (org-roam-directory org-directory)
  (org-roam-dailies-directory "dates/")
  (org-roam-db-gc-threshold most-positive-fixnum)
  :bind
  (("C-c n n" . org-id-get-create)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-goto-today)
   ("C-c n l" . org-roam-buffer-toggle)

   ;; Key-bindings for `org-roam-dailies'
   ("<s-up>" . org-roam-dailies-goto-previous-note)
   ("<s-down>" . org-roam-dailies-goto-next-note)

   ;; Open link from Org Roam window with mouse click
   (:map org-roam-mode-map
         ("<mouse-1>" . org-roam-preview-visit)))
  :config
  (org-roam-db-autosync-enable)
  (setq org-roam-complete-everywhere t)

  ;; Like `org-roam-completion-everywhere', but
  ;; this function perform the completion in brackets
  (org-roam-complete-link-at-point)

  ;;; Configure `org-roam-capture-templates'
  ;; Capture template for `org-roam-dailies'
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "\n* %?"
           :target (file+head
		    "%<%Y-%m-%d>.org"
		    "#+TITLE: %<%A-%Y-%m-%d>.\n\n\n")
           :empty-lines 1)))

  ;; Default capture template
  (setq org-roam-capture-templates
	'(("d" "default" entry "\n* %?"
           :target (file+head
                    "notes/%<%Y%m%d%H%M%S>-${slug}.org"
		    "#+TITLE: ${title}\n\n")
           :empty-lines 1
           :immediate-finish t
           :kill-buffer t)))

  ;; Objects displayed in Org Roam
  (setq org-roam-mode-sections
        '((org-roam-backlinks-section) ; Remove the `:unique' section
          org-roam-reflinks-section))

  ;;; Org Roam buffer configuration
  ;; Overwrite this function to let `org-roam-preview-visit' always
  ;; open in other window
  (defun org-roam-preview-visit (file point &optional other-window)
    (setq other-window t) ; By setting this variable to `t'
    (interactive (list (org-roam-buffer-file-at-point 'assert)
                       (oref (magit-current-section) point)
                       current-prefix-arg))
    (let ((buf (find-file-noselect file))
          (display-buffer-fn (if other-window
                                 #'switch-to-buffer-other-window
                               #'pop-to-buffer-same-window)))
      (funcall display-buffer-fn buf)
      (with-current-buffer buf
        (widen)
        (goto-char point))
      (when (org-invisible-p) (org-show-context))
      buf))

  ;; Customize the content in `org-roam-buffer' backlinks
  (cl-defun org-roam-backlinks-section (node &key (unique nil))
    (when-let ((backlinks (seq-sort #'org-roam-backlinks-sort
                                    (org-roam-backlinks-get node :unique unique))))
      (magit-insert-section (org-roam-backlinks)
        (magit-insert-heading "\n􀉣 LINKED REFERENCES")
        (insert "\n")
        (dolist (backlink backlinks)
          (org-roam-node-insert-section
           :source-node (org-roam-backlink-source-node backlink)
           :point (org-roam-backlink-point backlink)
           :properties (org-roam-backlink-properties backlink)))
        (insert ?\n))))

  ;; Org Roam buffer frame setup
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.35)
                 (window-height . fit-window-to-buffer)))

  ;; Preview LaTeX & images in Org Roam window
  ;; Note this function is defined interactivity
  (add-hook 'org-roam-buffer-postrender-functions
            (lambda ()
              (forward-line 5)
              (insert "\n")
              (visual-line-mode 1)
              (org--latex-preview-region (point-min) (point-max))
              (org-display-inline-images)))
  :hook
  ((after-init . (lambda ()
                   (interactive)
                   (org-roam-dailies-goto-today)
                   (goto-char (point-max))
                   (save-buffer)))))


(provide 'init-org-roam)
;;; init-org-roam.el ends here
