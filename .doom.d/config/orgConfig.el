;; Nastavení org-directory. Org-agenda-files čerpají ze stejného nastavení

(custom-set-variables
 '(org-directory "~/Documents/org")
 '(org-agenda-files (list org-directory)))

  ;; Nastavení defaultního souboru pro capture

  (setq org-default-notes-file (concat org-directory "/notesDefaultFile.org"))

(after! org

  ;; Modifying org mode defaults
  ;;
  (setq org-use-property-inheritance t ; it's convenient to have properties inherited
        org-list-allow-alphabetical t  ; have a. A. a) A) list bullets
        org-export-with-sub-superscripts '{} ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
        org-startup-indented t ; always start org-mode indented according to header levels and such
        org-adapt-indentation t
        org-hide-emphasis-markers nil ; dont hide emphasis markers. I want to know what my org mode file contains
        org-pretty-entities nil ; that applies also for UTF8 specail characters. Eventhough this could be considered to make `t`
        org-catch-invisible-edits 'smart)

  ;; Nastavení věcí, co se spustí s org-mode
  ;; tedy org-autolist

  (add-hook 'org-mode-hook (
                            lambda () (org-autolist-mode))
            )

  ;; Have list markers change with depth automatically in sequence - -> + -> * -> -
  (setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "*") ("*" . "-") ("1." . "a.")))

  ;; Hook autoload function in `writing.el` to org-mode start
  (add-hook 'org-mode-hook #'thi/org-buffer-config-h)

  ;; Temporary fix to gj, gk and such shodowed by mostly visual-line-mode
  ;; next lines then allows to use gj, gk, gh, gl as defined in evil-org
  ;; Odkaz: https://github.com/hlissner/doom-emacs/issues/4935
  ;;
;;; Unshadow evil-org's bindings, if we expect it to be loaded.
  (when (and (featurep! :lang org) (featurep! :editor evil +everywhere))
    ;; Move visual-line-mode bindings from evil-integration.el out of the way.
    (when (and evil-want-integration evil-respect-visual-line-mode)
      (general-define-key
       :definer 'minor-mode
       :states 'motion
       :keymaps 'visual-line-mode
       "gj" nil
       "gk" nil ;; In the end, `gj` and `gk` can be used to move around the headings, just as should from evil-org
       "gs M-j" #'evil-next-line
       "gs M-k" #'evil-previous-line))
    ;; Move the evil-lion bindings out of the way too.
    (map!
     :nv "gl" nil
     :nv "gL" nil
     :nv "gzl" #'evil-lion-left
     :nv "gzL" #'evil-lion-right))

  ;; Map those operations also to their arrow-key variants
  (map! :map evil-org-mode-map
        :after evil-org
        :n "g <up>" #'org-backward-heading-same-level
        :n "g <down>" #'org-forward-heading-same-level
        :n "g <left>" #'org-up-element
        :n "g <right>" #'org-down-element)

  ;; Evil-motion bindings - visual line vs real line
  (map! :after org
        (:map org-mode-map
         ;; visual-line movements, meaning movements in visible line - one actual line is text between two `\n`. Actual line may have multiple visual lines, depending on window width.
         :nvm "j" #'evil-next-visual-line
         :nvm "k" #'evil-previous-visual-line
         :nv "0" #'evil-beginning-of-visual-line
         :nvm "^" #'evil-first-non-blank-of-visual-line
         :nvm "$" #'evil-end-of-visual-line
         ;; real-line movements
         :nvm "J" #'evil-next-line
         :nvm "K" #'evil-previous-line
         :nvm "g0" #'evil-beginning-of-line
         :nvm "g^" #'evil-first-non-blank
         :nvm "g$" #'evil-end-of-line))
 )

 (use-package! org-appear
   :hook (org-mode . org-appear-mode)
   :config
   (setq org-appear-autolinks t
         org-appear-autosubmarkers t
         org-appear-autoemphasis t
         org-appear-autoentities t)
   (add-hook! 'org-appear-mode-hook
              ;; for proper first-time setup, `org-appear--set-elements'
              ;; needs to be run after other hooks have acted.
              (org-appear--set-elements)
              (add-hook! evil-insert-state-entry :local (org-appear-mode 1))
              (add-hook! evil-insert-state-exit :local (org-appear-mode -1)))) ;; These two hooks are still little buggy -- after first loading of the file they are not active, only after first triggering them (when editing org link), after that then they act as they should. Without the additional `:hook` this for some reason "leaks" to latex mode and pollutes it with warnings when changing insert/normal state.

;; (add-hook! org-mode :append #'org-appear-mode) ;; alternative to previous hook

;; The same for mathematical symbols and formulas
(use-package! org-fragtog
  :defer t
  :init
  (add-hook! evil-insert-state-entry (org-fragtog-mode 1))
  (add-hook! evil-insert-state-exit (org-fragtog-mode -1)))

(after! org

 ;; Alternative link creating function - `counsel-org-link` - and settings for it

  (map! :after counsel :map org-mode-map
        "C-c l l h" #'counsel-org-link)

  (after! counsel
    (setq counsel-outline-display-style 'title))
  ;; link diplays heading title, no full path to title

  (after! org-id
    ;; Do not create ID if a CUSTOM_ID exists
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))
  ;; org-id now uses CUSTOM-ID if it is specified

  ;; This bunch of code should force creating human-readablE CUSTOM_IDs

  (defun zz/make-id-for-title (title)
    "Return an ID based on TITLE."
    (let* ((new-id (replace-regexp-in-string "[^[:alnum:]]" "-" (downcase title))))
      new-id))

  (defun zz/org-custom-id-create ()
    "Create and store CUSTOM_ID for current heading."
    (let* ((title (or (nth 4 (org-heading-components)) ""))
           (new-id (zz/make-id-for-title title)))
      (org-entry-put nil "CUSTOM_ID" new-id)
      (org-id-add-location new-id (buffer-file-name (buffer-base-buffer)))
      new-id))

  (defun zz/org-custom-id-get-create (&optional where force)
    "Get or create CUSTOM_ID for heading at WHERE.

If FORCE is t, always recreate the property."
    (org-with-point-at where
      (let ((old-id (org-entry-get nil "CUSTOM_ID")))
        ;; If CUSTOM_ID exists and FORCE is false, return it
        (if (and (not force) old-id (stringp old-id))
            old-id
          ;; otherwise, create it
          (zz/org-custom-id-create)))))

  ;; Now override counsel-org-link-action
  (after! counsel
    (defun counsel-org-link-action (x)
      "Insert a link to X.

X is expected to be a cons of the form (title . point), as passed
by `counsel-org-link'.

If X does not have a CUSTOM_ID, create it based on the headline
title."
      (let* ((id (zz/org-custom-id-get-create (cdr x))))
        (org-insert-link nil (concat "#" id) (car x)))))

  ;; This should "reformat" org-mode buffer. Dont know what is the difference betwenn `org-mode-restart`, but it is supposedly a "gem," so I take it and test it.

  (defun zz/org-reformat-buffer ()
    (interactive)
    (when (y-or-n-p "Really format current buffer? ")
      (let ((document (org-element-interpret-data (org-element-parse-buffer))))
        (erase-buffer)
        (insert document)
        (goto-char (point-min)))))

  ;;
  ;; Úprava vzhledu org-mode
  ;;

  ;; Nastavení způsobu zvýrazňování vlastní TODO-sekvencí
  ;; =org-emphasis-alist= je proměnná obsahující delimitery pro markup. Seznam delimiterů je bohužel hardcoded; nelze přidat další, ale lze redefinovat způsob zvýraznění
  ;; daných delimiterů. Níže je redefinice =+=; dalo by se redefinovat i =~=; kromě =:foreground= má zabarvení i parametr =:background=

  (setq org-todo-keyword-faces
        '(("IMPORTANT" . (:foreground "red" :weight bold))
          ))
  (add-to-list 'org-emphasis-alist
               '("+" (:foreground "red")))

  ;; defer font-locking when typing to make the experience more responsive
  (defun locally-defer-font-lock ()
    "Set jit-lock defer and stealth, when buffer is over a certain size."
    (when (> (buffer-size) 50000)
      (setq-local jit-lock-defer-time 0.05
                  jit-lock-stealth-time 1)))

  (add-hook 'org-mode-hook #'locally-defer-font-lock)
  ;; Apparently this causes issues with some people, but I haven’t noticed anything problematic beyond the expected slight delay in some fontification, so until I do I’ll use the above.

  ;; Nastavení vlastních delimiterů pro zvýrazňování textu; text mezi =%= a =!= je zvýrazněn.
  (require 'org-habit nil t)

  (defun my/org-add-my-extra-fonts ()
    "Add alert and overdue fonts. =invisible t= způsobí zmizení delimiteru v zobrazeném textu, =nil= jej ponechá zobrazený. Jake delimitery jsou použity =%%= a =!!=. Řešeno dle: https://emacs.stackexchange.com/questions/35626/how-to-make-my-own-org-mode-text-emphasis-work-again/35632#35632"
    (add-to-list 'org-font-lock-extra-keywords '("\\(!!\\)\\([^\n\r\t]+\\)\\(!!\\)"
                                                 (1 '(face org-habit-alert-face invisible nil)) (2 'org-habit-alert-face t) (3 '(face org-habit-alert-face invisible nil))) t)
    (add-to-list 'org-font-lock-extra-keywords '("\\(%%\\)\\([^\n\r\t]+\\)\\(%%\\)"
                                                 (1 '(face org-habit-overdue-face invisible nil)) (2 'org-habit-overdue-face t) (3 '(face org-habit-overdue-face invisible nil))) t))

  (add-hook 'org-font-lock-set-keywords-hook #'my/org-add-my-extra-fonts)

  ;; Customize org-mode heading symbols
  (after! org-superstar
    (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
          ;; org-superstar-headline-bullets-list '("I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X")
          org-superstar-prettify-item-bullets t ))

  ;; Settings for parenthessis completion -- complete target syntax `<< >>`, and also custom syntax defined above: `%% %%`, `!! !!`
  (sp-local-pair
   '(org-mode)
   "<<" ">>"
   :actions '(insert))

  (sp-local-pair
   '(org-mode)
   "%%" "%%"
   :actions '(insert))

  (sp-local-pair
   '(org-mode)
   "!!" "!!"
   :actions '(insert))
  )

 ;; Settings for org-roam-server package
 ;;
 (use-package org-roam-server
   :defer t
   :after (org-roam-server)
   :config
   (setq org-roam-server-host "127.0.0.1"
         org-roam-server-port 8078
         org-roam-server-export-inline-images t
         org-roam-server-authenticate nil
         org-roam-server-network-label-truncate t
         org-roam-server-network-label-truncate-length 60
         org-roam-server-network-label-wrap-length 20)
   (defun org-roam-server-open ()
     "Ensure the server is active, then open the roam graph."
     (interactive)
     (org-roam-server-mode 1)
     (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))

(after! org

  ;;
  ;; LaTeX in org-mode appearance settings
  ;;
  (setq org-highlight-latex-and-related '(native script entities)) ;; Introduces native highlighting to LaTeX code block
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))) ;; remove org-block face, which is added by `native` highlighting

 ;; Make active org-special blocks
 ;;
 (use-package! org-special-block-extras
   :after org
   :hook (org-mode . org-special-block-extras-mode))

(after! org

  ;; Does this have to be here? Until I learn with publishing and emacs --script, YES
  ;;
  (require 'ox)
  (require 'ox-latex)
  (require 'ox-html)
  (require 'ox-odt)

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

  ;; HTML export settings

  (setq org-html-htmlize-output-type 'css)
  (setq org-src-fontify-natively t)
  (autoload #'highlight-numbers--turn-on "highlight-numbers") ;;
  (add-hook 'htmlize-before-hook #'highlight-numbers--turn-on) ;; highlight numbers with htmlize output

  ;; Definitions of filters for exporting
  ;;
  ;; General Filters
  ;;
  (defun my/general-filter-nobreaks (text backend info)
    "Ensure \" \" are properly handled in export."
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string " \\([zuioaskvZUIOASKV]\\) " " \\1~" text))
     ((org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string " \\([zuioaskvZUIOASKV]\\) " " \\1&nbsp" text))))

  (defun my/general-filter-highlightNotes (text backend info)
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
               'my/general-filter-nobreaks)

  (add-to-list 'org-export-filter-plain-text-functions
               'my/general-filter-highlightNotes)

  ;; LaTeX Filters

  (defun my/latex-filter-inlineCodeHighlight (text backend info)
    "Grey highlighted inline code"
    (when (org-export-derived-backend-p backend 'latex)
      (format "\\highLight{%s}" text)))

  (defun my/latex-export-src-blocks (text backend info)
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

  ;; For some projects it is better disabled. Works well only if all referenes are for latex defined in latex commands (ref, vref, cref) and not with org-mode syntax ([[]])
  ;; (defun my/latex-filter-removeOrgAutoLabels (text backend info)
  ;;   "Org-mode automatically generates labels for headings despite explicit use of `#+LABEL`. This filter forcibly removes all automatically generated org-labels in headings."
  ;;   (when (org-export-derived-backend-p backend 'latex)
  ;;     (replace-regexp-in-string "\\\\label{sec:org[a-f0-9]+}\n" "" text)))

  (add-to-list 'org-export-filter-verbatim-functions
               'my/latex-filter-inlineCodeHighlight)

  (add-to-list 'org-export-filter-src-block-functions
               'my/latex-export-src-blocks)

  ;; (add-to-list 'org-export-filter-headline-functions
  ;;              'my/latex-filter-removeOrgAutoLabels)

  ;; Settings for conversion from org-mode to HTML

  (defun my/latex-filter-remove-latex (text backend info)
    "Replace \\LaTeX with \"LaTeX\" in HTML output."
    (when (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "\\\\LaTeX" "LaTeX" text)))

  (defun my/latex-filter-remove-latex-alt (text backend info)
    "Replace \\LaTeX with \"LaTeX\" in HTML output."
    (when (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "LaTeX{}" "LaTeX" text)))

  (add-to-list 'org-export-filter-latex-fragment-functions
               'my/latex-filter-remove-latex)

  (add-to-list 'org-export-filter-headline-functions
               'my/latex-filter-remove-latex-alt
               'my/latex-filter-remove-latex)

  (add-to-list 'org-export-filter-link-functions
               'my/latex-filter-remove-latex-alt
               'my/latex-filter-remove-latex)

  ;; Settings for org-mode import with pandoc
  (use-package! org-pandoc-import
    :after org)

  ;; HTML htmlize export settings

  (setq org-html-doctype "html5"
        org-html-html5-fancy t)

  ;; Nastavení org-babel-clojure-backend pro evaluaci kódu clojure v org
  (setq org-babel-clojure-backend 'cider)

  ;; Experimental packages
  ;;
  ;; Settings for org-transclusion from https://github.com/tecosaur/emacs-config/blob/master/config.org
  (use-package! org-transclusion
    :commands org-transclusion-mode
    :defer t
    :init
    (map! :after org :map org-mode-map
          "<f12>" #'org-transclusion-mode))

  ;; Settings for org-outline-tree ; experimental package
  (use-package! org-ol-tree
    :defer t
    :commands org-ol-tree)

  ;; Another experimental - org-ml: functional parsing of org-mode files
  (use-package! org-ml
    :after org)

  ;; Query language for org -- disabled because of: https://github.com/alphapapa/org-ql/issues/214, fix #216 meantioned in issue des not seem to work
  (use-package! org-ql
    :after org)

  ;; Automatically tangles org-file. In future might be useful, so it is put here, but disabled now.
  (use-package! org-auto-tangle
    :defer t
    :hook (org-mode . org-auto-tangle-mode)
    :config
    (setq org-auto-tangle-default t))

  ;; This function returns a list of all the headings in the given file which have the given tags.

  (defun zz/headings-with-tags (file tags)
    (let ((headings (org-ql-select file
                      `(tags-local ,@tags))))
      (mapconcat
       (lambda (l) (format "- %s" l))
       (mapcar
        (lambda (h)
          (let ((title (car (org-element-property :title h))))
            (org-link-make-string
             (format "file:%s::*%s"
                     file title)
             title)))
        headings) "\n")))
  ) ;; closed `after!` macro from beginning of the org-mode settings