;; Nastavení org-directory. Org-agenda-files čerpají ze stejného nastavení

(custom-set-variables
 '(org-directory "~/Documents/org")
 '(org-agenda-files (list org-directory)))

  ;; Nastavení defaultního souboru pro capture

  (setq org-default-notes-file (concat org-directory "/notesDefaultFile.org")
  ;;     setq org-roam-directory "~/valid/path/to/folder/where/the/org/roam/db/will/be/stored"
        )

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
        org-catch-invisible-edits 'smart
        org-element-use-cache t)

  ;; Nastavení věcí, co se spustí s org-mode
  ;; tedy org-autolist

  (add-hook 'org-mode-hook (
                            lambda () (org-autolist-mode))
            )

  ;; Have list markers change with depth automatically in sequence - - -> + -> - -> + (orig. -> + -> * -> -); the reason forthat is that the "*" sometimes does work a bit funky, because it is also the mark for the headings
  (setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "-") ("1." . "a.")))

  ;; Hook autoload function in `writing.el` to org-mode start
  (add-hook 'org-mode-hook #'thi/org-buffer-config-h)

  ;; Temporary fix to gj, gk and such shodowed by mostly visual-line-mode
  ;; next lines then allows to use gj, gk, gh, gl as defined in evil-org
  ;; Odkaz: https://github.com/hlissner/doom-emacs/issues/4935
  ;;
;;; Unshadow evil-org's bindings, if we expect it to be loaded.
  (when (and (modulep! :lang org) (modulep! :editor evil +everywhere))
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

;; (add-hook! org-mode :append #'org-appear-mode)
;; alternative to previous hook

;; Temporarily disabled, because org-mode seems to be triggering also in makefiles. Sequence `$text$` then triggers `org-mode-latex-preview`, that triggers fragtog
;;   ;; The same for mathematical symbols and formulas

;;   (use-package! org-fragtog
;;     :defer t
;;     :hook (org-mode . org-fragtog-mode)
;;     :config
;;     (add-hook! 'org-fragtog-mode-hook
;;       (add-hook! evil-insert-state-entry (org-fragtog-mode 1))
;;       (add-hook! evil-insert-state-exit (org-fragtog-mode -1))))

;;
;; Setup sidebars in org mode
;;

;; from elken
(use-package! org-ol-tree
  :after org
  :commands org-ol-tree
  :config
  (setq org-ol-tree-ui-window-position 'left)) ;; or 'right , because this collides with treemacs

;; Org-mode triggers for TODO-items
(use-package! org-edna
  :after org
  :hook (org-mode . org-edna-mode))

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

  ;; Nastavení způsobu zvýrazňování vlastních TODO-sekvencí
  (setq org-todo-keywords '((sequence
                             "TODO(t)"  ; This task needs to be done.

                             "NEXT(n)" ; I committed to work on this task next. This can be used to
                                        ; plan my next day, or to indicate in a project the next
                                        ; logical step.

                             "WAIT(w)" ; This task is waiting some external condition to happen,
                                        ; like waiting for an authorization, waiting for account to
                                        ; be created, etc.

                             "HOLD(h)" ; This task is paused. This state can mean many different
                                        ; things. A task can be paused because I simply don't want
                                        ; to continue doing it, or it is paused because I want to
                                        ; make sure I understand I shouldn't be work on this task.
                             "|"
                             "DONE(d)"  ; Task successfully completed

                             "CANCEL(c)" ; Task was cancelled, aborted or is no longer applicable,
                                        ; but I want to keep it as a record instead of simply
                                        ; remove it from the file.
                             ))

        org-todo-keyword-faces '(("IMPORTANT" . (:foreground "red" :weight bold))
                                 ("WAIT" . +org-todo-onhold)
                                 ("HOLD" . +org-todo-onhold))
        )

  ;; =org-emphasis-alist= je proměnná obsahující delimitery pro markup. Seznam delimiterů je bohužel hardcoded; nelze přidat další, ale lze redefinovat způsob zvýraznění daných delimiterů. Níže je redefinice =+=; dalo by se redefinovat i =~=; kromě =:foreground= má zabarvení i parametr =:background=
  (add-to-list 'org-emphasis-alist
               '("+" (:foreground "red")))

  ;; Set the look of pririties cookies

  (setq org-priority-faces '((?A . (:foreground "red" :weight bold))
                             (?B . (:foreground "orange"))
                             (?C . (:foreground "yellow"))
                             (?D . (:foreground "green"))
                             (?1 . (:foreground "magenta" :weight bold))
                             (?2 . (:foreground "red" :weight bold))
                             (?3 . (:foreground "orange red"))
                             (?4 . (:foreground "dark orange"))
                             (?5 . (:foreground "yellow"))
                             (?6 . (:foreground "lime green"))
                             (?7 . (:foreground "dark green"))
                             (?8 . (:foreground "dodger blue"))
                             (?9 . (:foreground "dark slate blue"))))

  ;; defer font-locking when typing to make the experience more responsive
  ;; Disable this in case of issues with displaying fontification or overlays in org-mode. The source of these issues are most probably org-fancying packages, like org-superstar or org-fancy-priorities, which rely on finished font-lock process.

  (defun locally-defer-font-lock ()
    "Set jit-lock defer and stealth, when buffer is over a certain size."
    (when (> (buffer-size) 50000)
      (setq-local jit-lock-defer-time 0.05
                  jit-lock-stealth-time 1)))

  (add-hook 'org-mode-hook #'locally-defer-font-lock)

  ;; Nastavení vlastních delimiterů pro zvýrazňování textu; text mezi =%= a =!= je zvýrazněn.
  (require 'org-habit nil t)

  (defun my/org-add-my-extra-fonts ()
    "Add alert and overdue fonts. =invisible t= způsobí zmizení delimiteru v zobrazeném textu, =nil= jej ponechá zobrazený. Jake delimitery jsou použity =%%= a =!!=. Řešeno dle: https://emacs.stackexchange.com/questions/35626/how-to-make-my-own-org-mode-text-emphasis-work-again/35632#35632"
    (add-to-list 'org-font-lock-extra-keywords '("\\(!!\\)\\([^\n\r\t]+\\)\\(!!\\)"
                                                 (1 '(face org-habit-alert-face invisible nil)) (2 'org-habit-alert-face t) (3 '(face org-habit-alert-face invisible nil))) t)
    (add-to-list 'org-font-lock-extra-keywords '("\\(%%\\)\\([^\n\r\t]+\\)\\(%%\\)"
                                                 (1 '(face org-habit-overdue-face invisible nil)) (2 'org-habit-overdue-face t) (3 '(face org-habit-overdue-face invisible nil))) t)
    )

  ;; Function to add my custom keywords to org-mode.
  ;; Only this approach works
  (defun my/org-add-my-extra-keywords ()
    (push '("MAKE" (0 'my/make-face t)) org-font-lock-extra-keywords)
    (push '("FIXME" (0 'my/warning-face t)) org-font-lock-extra-keywords)
    (push '("CANCELED" (0 'my/warning-face t)) org-font-lock-extra-keywords)
    (push '("BOOKMARK" (0 'my/warning-face t)) org-font-lock-extra-keywords)
    (push '("IMPORTANT" (0 'my/important-face t)) org-font-lock-extra-keywords))

  ;; This actually adds the functions to be executed as hooks
  (add-hook 'org-font-lock-set-keywords-hook #'my/org-add-my-extra-fonts)
  (add-hook 'org-font-lock-set-keywords-hook #'my/org-add-my-extra-keywords)

  ;; Setup the interactive highlighting with the org-remark package
  ;;
  (require 'org-remark-global-tracking)
  (org-remark-global-tracking-mode +1)

  ;; Key-bind `org-remark-mark' to global-map so that you can call it
  ;; globally before the library is loaded.

  (define-key global-map (kbd "C-c n m") #'org-remark-mark)

  ;; The rest of keybidings are done only on loading `org-remark'
  (with-eval-after-load 'org-remark
    (define-key org-remark-mode-map (kbd "C-c n o") #'org-remark-open)
    (define-key org-remark-mode-map (kbd "C-c n ]") #'org-remark-view-next)
    (define-key org-remark-mode-map (kbd "C-c n [") #'org-remark-view-prev)
    (define-key org-remark-mode-map (kbd "C-c n r") #'org-remark-remove)

    ;; Define custom pens -- also probably has to be loaded after the org-remark package is loaded:
    (load! "custom-pens.el")
    )

  ;; Customize org-mode heading symbols
  ;; (after! org-superstar
  ;;   (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
  ;;         ;; org-superstar-headline-bullets-list '("I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X")
  ;;         org-superstar-prettify-item-bullets t ))

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

(use-package! websocket
    :after org-roam)

(after! org

  ;;
  ;; LaTeX in org-mode appearance settings
  ;;
  (setq org-highlight-latex-and-related '(native script entities)) ;; Introduces native highlighting to LaTeX code block
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))) ;; remove org-block face, which is added by `native` highlighting

    ;; Sets ob-http package to make HTTP requests from org-mode
   (use-package! ob-http))




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

  (map! :map org-mode-map
        :after org
        :localleader
        :desc "Outline" "O" #'org-ol-tree)

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

;; Citations in org-mode with org-cite
;;

(use-package! citeproc
  :defer t)

;;; Org-Cite configuration

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Insert citation" "@" #'org-cite-insert)

(use-package! oc
  :after org citar
  :config
  (require 'ox)
  (setq org-cite-global-bibliography
        (let ((paths (or citar-bibliography
                         (bound-and-true-p bibtex-completion-bibliography))))
          ;; Always return bibliography paths as list for org-cite.
          (if (stringp paths) (list paths) paths)))
  ;; setup export processor; default csl/citeproc-el, with biblatex for latex
  (setq org-cite-export-processors
        '((t csl))))

  ;;; Org-cite processors
(use-package! oc-biblatex
  :after oc)

;;;; Third-party

(use-package! citar-org
  :no-require
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-support-shift-select t)
  (when (modulep! :lang org +roam2)
    ;; Include property drawer metadata for 'org-roam' v2.
    (citar-org-note-include '(org-id org-roam-ref)))
  ;; Personal extras
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-silver :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-dsilver :v-adjust 0.01) . " "))))

;; Glossaries in org mode
(use-package! org-glossary :after org)

;; Settings for org super agenda -- stolen from TEC
;;
(use-package! org-super-agenda
  :commands org-super-agenda-mode)

(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

;; Org declarative capture templates settings
;;

(use-package! doct
  :commands doct)

;; Some hooks are notoriously prblematic.
;; Let's ignore them when they are misbehaving
;; (defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
;;   :around #'org-fancy-priorities-mode
;;   :around #'org-superstar-mode
;;   (ignore-errors (apply orig-fn args)))

  ) ;; closed `after!` macro from beginning of the org-mode settings
