;; Loading emacs modules for org-export

(require 'ox)
(require 'ox-html)
(require 'ox-latex)
(require 'ox-odt)

(require 'ob-shell)

;; Defining Custom LaTeX documentclass; removing default packages and declaring place to load custom packages

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("scrbook"
                 "\\documentclass{scrbook}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  ;; Removes default "\hypersetup" from LaTeX export template

  (customize-set-value 'org-latex-with-hyperref nil)

;; Setting default LaTeX compiler as lualatex
;;
(setq org-latex-compiler "lualatex")

;; Definitions of filters for exporting
;;
;; General Filters
;;
(defun my-general-filter-nobreaks (text backend info)
  "Ensure \" \" are properly handled in export."
  (cond
   ((org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string " \\([zuioaskvZUIOASKV]\\) " " \\1~" text))
   ((org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string " \\([zuioaskvZUIOASKV]\\) " " \\1&nbsp" text))))

  (defun my-general-filter-highlightNotes (text backend info)
    "Highligh custom notes markup. Notes are surounded by \"%%\" or \"!!\" delimiters."
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (setq text
            (replace-regexp-in-string "!!\\(.*\\)!!" "\\\\highLight[yellow]{\\1}" text))
      (setq text
            (replace-regexp-in-string "\\\\%\\\\%\\(.*\\)\\\\%\\\\%" "\\\\highLight[red]{\\1}" text)))
     ((org-export-derived-backend-p backend 'html)
      (setq text
            (replace-regexp-in-string "!!\\(.*\\)!!" "<span style=\"background-color:yellow\">\\1</span>" text))
      (setq text
            (replace-regexp-in-string "%%\\(.*\\)%%" "<span style=\"background-color:red\">\\1</span>" text)))
     ((org-export-derived-backend-p backend 'odt)
      (setq text
            (replace-regexp-in-string "!!\\(.*\\)!!" "<text:span text:style-name=\"myHighlightImportant\">\\1</text:span>" text))
      (setq text
           (replace-regexp-in-string "%%\\(.*\\)%%" "<text:span text:style-name=\"myHighlightWarning\">\\1</text:span>" text)))))

  (add-to-list 'org-export-filter-plain-text-functions
               'my-general-filter-nobreaks)

  (add-to-list 'org-export-filter-plain-text-functions
               'my-general-filter-highlightNotes)

;; LaTeX Fitlers

(defun my-latex-filter-inlineCodeHighlight (text backend info)
  "Grey highlighted inline code"
  (when (org-export-derived-backend-p backend 'latex)
    (format "\\highLight{%s}" text)))

  (defun my-latex-export-src-blocks (text backend info)
    "Export src blocks as without verbatim env."
    (when (org-export-derived-backend-p
           backend
           'latex)
      (with-temp-buffer
        (insert text)
        ;; remove verbatim environment altogether -- replace it with nothing
        (goto-char (point-min))
        (replace-string "\\begin{verbatim}" "")
        (replace-string "\\end{verbatim}" "")
        (buffer-substring-no-properties
         (point-min)
         (point-max)))))

  (add-to-list 'org-export-filter-verbatim-functions
               'my-latex-filter-inlineCodeHighlight)

  (add-to-list 'org-export-filter-src-block-functions
               'my-latex-export-src-blocks)

;; Settings for conversion from org-mode to HTML

(defun my-latex-filter-remove-latex (text backend info)
  "Replace \\LaTeX with \"LaTeX\" in HTML output."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "\\\\LaTeX" "LaTeX" text)))

(defun my-latex-filter-remove-latex-alt (text backend info)
  "Replace \\LaTeX with \"LaTeX\" in HTML output."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "LaTeX{}" "LaTeX" text)))

(add-to-list 'org-export-filter-latex-fragment-functions
             'my-latex-filter-remove-latex)

(add-to-list 'org-export-filter-headline-functions
             'my-latex-filter-remove-latex-alt
             'my-latex-filter-remove-latex)

(add-to-list 'org-export-filter-link-functions
             'my-latex-filter-remove-latex-alt
             'my-latex-filter-remove-latex)

;; Settings for code evaluation results export
;;
(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)))
