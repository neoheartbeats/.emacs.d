;;; init-benchmarking.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:

;; This file is adapted from "https://github.com/purcell/emacs.d/
;; blob/master/lisp/init-benchmarking.el"

;;; Code:

(defun my/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar my/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun my/require-times-wrapper (orig feature &rest args)
  "Note in `my/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
          (require-start-time (and (not already-loaded) (current-time))))
    (prog1
      (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (my/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'my/require-times
            (list feature require-start-time time)
            t))))))

(advice-add 'require :around 'my/require-times-wrapper)


(define-derived-mode my/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
    [("Start time (ms)" 20 my/require-times-sort-by-start-time-pred)
      ("Feature" 30 t)
      ("Time (ms)" 12 my/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'my/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun my/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
    (string-to-number (elt (nth 1 entry2) 0))))

(defun my/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
    (string-to-number (elt (nth 1 entry2) 2))))

(defun my/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in my/require-times
    with order = 0
    do (cl-incf order)
    collect (list order
              (vector
                (format "%.3f" (my/time-subtract-millis start-time before-init-time))
                (symbol-name feature)
                (format "%.3f" millis)))))

(defun my/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (my/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))


(defun my/show-init-time ()
  (message "init completed in %.2fms"
    (my/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'my/show-init-time)


(provide 'init-benchmarking)
