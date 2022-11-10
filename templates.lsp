;; templates.lsp --- template file for `tempel'
;; Fundamental mode

fundamental-mode

prog-mode

latex-mode org-mode

;; Environments
(mm "\$" r> "\$")
(mmm "\\[" n> r> n> "\\]")

(beg "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(enm "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
(itm "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")

(fr "\\frac{" p "}{" p "}" q)
(sq "\\sqrt{" p "}" q)
(pp "\\left(" r> "\\right)")

;; Calculus
(dv "\\dv{" p "}{" p "}" q)
(lim "\\lim_{" p "\to " p "}" q)
(sum "\\sum_{" p "=" p "}^{" p "}" q)
(int "\\int " p "\\,\\dd " q)
(dint "\\int_{" p "}^{" p "}" p "\\,\\dd " q)

;; Constants
(ii "\\mathrm{i}")
(ee "\\mathrm{e}")
(oo "\\infty")

lisp-mode emacs-lisp-mode

(lambda "(lambda (" p ")" n> r> ")" q)

emacs-lisp-mode

org-mode

(qt "#+BEGIN_QUOTE" p n> r> n> "#+END_QUOTE")
(sc "#+BEGIN_SRC " p n> r> n> "#+END_SRC")

(img "#+ATTR_ORG: :width " r> n> "[[/Users/ilyaw39/.org/repos/" r> "]]")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
