org-mode

(sc "#+BEGIN_SRC " p n> r> n> "#+END_SRC")
(qt "#+BEGIN_QUOTE " p n> r> n> "#+END_QUOTE")
(m "$" r "$")
(mm "\\[" r "\\]")
(bn "\\binom{" p "}{" r "}")
(qq "\\coloneqq")
(pp "\\left(" r "\\right)")
(beg "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(img "#+ATTR_ORG: :width " p n> "[[/Users/ilyaw39/Developer/TH18-03/repos/" r> "]]")
