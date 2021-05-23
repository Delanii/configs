# -*- mode: snippet -*-
# name: org source block
# key: srcblock
# --
#+latex: \begin{${1:latexcodeenvironment}}
#+begin_src ${2:languagename}
${0:`%`}
#+end_src
#+latex: \end{$1}
