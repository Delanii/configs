# -*- mode: snippet -*-
# name: org source block
# key: srcblock
# --
#+latex: \begin{${1:latexcodeenvironment}}
#+begin_src ${2:`(or (+yas/tec/org-last-src-lang) "lang")`}
${0:`%`}
#+end_src
#+latex: \end{$1}
