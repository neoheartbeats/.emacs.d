text-mode

(bg "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")

(qt "#+BEGIN_QUOTE" n> r> n "#+END_QUOTE")
(sc "#+BEGIN_SRC " q n> r> n "#+END_SRC")
(scp "#+BEGIN_SRC python :session t :results output" n> r> n "#+END_SRC")

python-ts-mode

(bk "###############################################################################")
