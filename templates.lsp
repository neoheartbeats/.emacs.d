fundamental-mode

(today (format-time-string "%Y-%m-%d"))

prog-mode

latex-mode org-mode

(m "\$" q "\$")
(mm "\\[\n" q "\n\\]")

(bg "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(f "\\frac{" p "}{" q "}")
(enm "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
(itm "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")

(int "\\int" p "\\,\\dd " q)
(dint "\\int_{" p "}^{" p "}" p "\\,\\dd " q)

lisp-mode emacs-lisp-mode

(lambda "(lambda (" p ")" n> r> ")")

emacs-lisp-mode

org-mode

(qt "#+BEGIN_QUOTE" n> r> n> "#+END_QUOTE")
(sc "#+BEGIN_SRC " p n> r> n> "#+END_SRC" :post (org-edit-src-code))
(elisp "#+BEGIN_SRC emacs-lisp" n> r> n "#+END_SRC" :post (org-edit-src-code))

(img "#+ATTR_ORG: :width " q)

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
