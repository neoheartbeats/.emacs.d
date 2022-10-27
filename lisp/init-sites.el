;;; init-sites.el --- Static site generator -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package htmlize :defer t)
(use-package dom :straight (:type built-in))
(use-package esxml :defer t)


;; Input file: `foo.el'
;; Output file: `foo.el.html'
(defun my/sites-generate-current-buf ()
  (interactive)
  (save-excursion
    (with-current-buffer (htmlfontify-buffer)
      (let ((doc (libxml-parse-xml-region (point-min)
                                          (point-max))))
        (switch-to-buffer (current-buffer)))
      (write-file (buffer-file-name)))))

(defun my/sites-generate-site (input-file output-dir)
  (save-excursion
    (with-current-buffer (find-file-noselect input-file))
    (my/sites-generate-current-buf)))

(defun my/sites-generate-site (input-dir output-dir)
  )


(provide 'init-sites)
;;; init-sites.el ends here
