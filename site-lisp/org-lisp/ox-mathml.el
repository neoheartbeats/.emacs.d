;;; ox-mathml.el --- Support for MathML exports -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 TEC
;;
;; Author: TEC <contact@tecosaur.net>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: February 27, 2023
;; Modified: February 27, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/tecosaur/ox-mathml
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Support for MathML exports
;;
;;; Code:

(defgroup org-mathml nil
  "Options for generation of MathML representations of LaTeX math."
  :tag "Org MathML export"
  :group 'org-export)

(defcustom org-mathml-converter-jar-file nil
  "Value of\"%j\" in `org-mathml-convert-command'.
Use this to specify additional executable file say a jar file.

When using MathToWeb as the converter, specify the full-path to
your mathtoweb.jar file."
  :group 'org-mathml
  :version "24.1"
  :type '(choice
          (const :tag "None" nil)
          (file :tag "JAR file" :must-match t)))

(defcustom org-mathml-convert-command nil
  "Command to convert LaTeX fragments to MathML.
Replace format-specifiers in the command as noted below and use
`shell-command' to convert LaTeX to MathML.
%j:     Executable file in fully expanded form as specified by
        `org-latex-to-mathml-jar-file'.
%I:     Input LaTeX file in fully expanded form.
%i:     The latex fragment to be converted.
%o:     Output MathML file.

This command is used by `org-mathml-convert-latex'.

When using MathToWeb as the converter, set this option to
\"java -jar %j -unicode -force -df %o %I\".

When using LaTeXML set this option to
\"latexmlmath \"%i\" --preload=amsmath.sty --preload=amssymb.sty --presentationmathml=%o\"."
  :group 'org-mathml
  :version "24.1"
  :type '(choice
          (const :tag "None" nil)
          (string :tag "\nShell command")))

(defun org-mathml-converter-available-p ()
  "Return t if `org-mathml-convert-command' is usable."
  (save-match-data
    (when (and (boundp 'org-mathml-convert-command)
               org-mathml-convert-command)
      (let ((executable (car (split-string
                              org-mathml-convert-command))))
        (when (executable-find executable)
          (if (string-match
               "%j" org-mathml-convert-command)
              (file-readable-p org-mathml-converter-jar-file)
            t))))))

(defun org-mathml-convert-latex (latex-frag &optional mathml-file)
  "Convert LATEX-FRAG to MathML and store it in MATHML-FILE.
Use `org-latex-to-mathml-convert-command'.  If the conversion is
successful, return the portion between \"<math...> </math>\"
elements otherwise return nil.  When MATHML-FILE is specified,
write the results in to that file.  When invoked as an
interactive command, prompt for LATEX-FRAG, with initial value
set to the current active region and echo the results for user
inspection."
  (interactive (list (let ((frag (when (org-region-active-p)
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end)))))
                       (read-string "LaTeX Fragment: " frag nil frag))))
  (unless latex-frag (user-error "Invalid LaTeX fragment"))
  (let* ((tmp-in-file
          (let ((file (file-relative-name
                       (make-temp-name (expand-file-name "ltxmathml-in")))))
            (write-region latex-frag nil file)
            file))
         (tmp-out-file (file-relative-name
                        (make-temp-name (expand-file-name  "ltxmathml-out"))))
         (cmd (format-spec
               org-mathml-convert-command
               `((?j . ,(and org-mathml-converter-jar-file
                             (shell-quote-argument
                              (expand-file-name
                               org-mathml-converter-jar-file))))
                 (?I . ,(shell-quote-argument tmp-in-file))
                 (?i . ,latex-frag)
                 (?o . ,(shell-quote-argument tmp-out-file)))))
         mathml shell-command-output)
    (when (called-interactively-p 'any)
      (unless (org-mathml-converter-available-p)
        (user-error "LaTeX to MathML converter not configured")))
    (message "Running %s" cmd)
    (setq shell-command-output (shell-command-to-string cmd))
    (setq mathml
          (when (file-readable-p tmp-out-file)
            (with-current-buffer (find-file-noselect tmp-out-file t)
              (goto-char (point-min))
              (when (re-search-forward
                     (format "<math[^>]*?%s[^>]*?>\\(.\\|\n\\)*</math>"
                             (regexp-quote
                              "xmlns=\"http://www.w3.org/1998/Math/MathML\""))
                     nil t)
                (prog1 (match-string 0) (kill-buffer))))))
    (cond
     (mathml
      (setq mathml
            (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" mathml))
      (when mathml-file
        (write-region mathml nil mathml-file))
      (when (called-interactively-p 'any)
        (message mathml)))
     ((warn "LaTeX to MathML conversion failed")
      (message shell-command-output)))
    (delete-file tmp-in-file)
    (when (file-exists-p tmp-out-file)
      (delete-file tmp-out-file))
    mathml))

(defun org-mathml-convert-latex-cached (latex-frag)
  "Use `org-mathml-convert-latex' but check local cache first."
  (let ((latex-hash-path
         (expand-file-name
          (concat
           "org-mathml-formula-"
           (sha1
            (prin1-to-string
             (list latex-frag
                   org-mathml-convert-command)))
           ".mathml")
          temporary-file-directory))
        print-length print-level)
    (unless (file-exists-p latex-hash-path)
      (org-mathml-convert-latex latex-frag latex-hash-path))
    (and (file-exists-p latex-hash-path)
         latex-hash-path)))

(provide 'ox-mathml)
;;; ox-mathml.el ends here
