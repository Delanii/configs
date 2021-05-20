;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config sources:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - https://tecosaur.github.io/emacs-config/config.html
;; - https://townk.github.io/doom-emacs-private/
;; - https://gitlab.com/zzamboni/dot-doom
;; - https://pages.sachachua.com/.emacs.d/Sacha.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User identification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tomas Krulis"
      user-mail-address "krulis.tomas.tk@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additinal documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setting location of emacs source code to access documentation for functions that are written in C:
;;
(setq source-directory (concat (getenv "HOME") "/emacsSource/emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 14 :weight 'regular))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Modeline changes
;;
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange" :weight bold))  ;; sets filename in orange color to get nicer look at modified buffer

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Directory Setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line Numbers Setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)
(setq-default display-line-numbers-type 'relative) ;; changed from `setq` to `setq-default`. It should be doing the same - setting default line numbering to `relative`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Toto spustí emacs vždy ve word-wrap módu - zalamování řádků na koncích slov (v mezerách)
(global-visual-line-mode t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(setq comp-deferred-compilation t)

;; Start Emacs always maximized

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Application window settings - emacs top tittle bar has following format: `buffer-name` -symbol- project-name (read by projectile, follows git naming)
;;
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  "   ᛗ  %s" "   ✪   %s") project-name))))))

;; Vypnutí automatické indentace tabulátory:
(setq-default indent-tabs-mode nil)
;; And it there are any tabs, set their size to 4 spaces
(setq-default tab-width 4)

;; Sets the ammount of lines showed that are showed when reaching edge of the screen (top or bottom)
(setq scroll-margin 2
      scroll-conservatively 1000)

(setq scroll-preserve-screen-position 'always)          ;; always preserves cursor position after scrolling

(global-subword-mode 1)                           ; Iterate through CamelCase words

;; This set of functions displays buffer preview when `evil-window-vsplit` or `evil-window-split` is used in splitted window (right or bottom).
;; First, we’ll enter the new window
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Then, we’ll pull up ivy
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

;; Oh, and previews are nice
(setq +ivy-buffer-preview t)

;; This add window changing and swapping not only with `hjkl` as in vim directions, but also with arrow keys
(map! :map evil-window-map
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

;; General settings from tecosaur
;; odkaz: https://github.com/tecosaur/emacs-config/blob/master/config.org

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deletion and undo Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default
 delete-by-moving-to-trash t) ; Delete files to trash

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t)                        ; Nobody likes to loose work, I certainly don't

;; Nastavení undo-tree
;;
;; Vypnutí automatického ukládání undo-tree history, ačkoli by mělo být defaultně vypnuté
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;; Nastavení komprese pro soubory historie undo-tree. Prý mohou rychle nabýt na velikosti ...
(defadvice undo-tree-make-history-save-file-name
    (after undo-tree activate)
  (setq ad-return-value (concat ad-return-value ".xz")))

;; Settings for package for dealing with very large files (vlf package)
;;
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spellcheck Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ispell-change-dictionary "czech" t)

;; Nastavení spellchecku pro angličtinu a češtinu současně - funguje ve Spacemacs (`.spacemacs`), ale ne zde

;; (setq ispell-program-name "hunspell")
;; you could set `ispell-dictionary` instead but `ispell-local-dictionary' has higher priority
;; (setq ispell-local-dictionary "en_US")
;; (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,cs_CZ") nil utf-8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil - modal editor settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; except for those specific for org-mode

(setq evil-move-cursor-back nil)        ;; After switch from normal mode to insert mode dont move cursor back on letter but leave it where it was.
(setq evil-kill-on-visual-paste nil)    ;; When pasting over selected text delete replace selected text with pasted one
(after! evil-escape (evil-escape-mode -1)) ;; Disables evil-escape function, that allows to go to emacs mode after pressing jk
(after! evil (setq evil-ex-substitute-global t)) ;; Evil substitution with `:s/.../...`  are always global, opposed to need to write `:%s/.../...`

(setq evil-visual-region-expanded t)    ;; emacs "region" and vim "selection" mean the same.

;; Smarparens (parenthesis completion and protection from unbalanced parenthesis) activate only in programming modes
(use-package! smartparens
  :init (add-hook 'smartparens-strict-mode-hook #'evil-cleverparens-mode)
  :hook ((prog-mode . smartparens-strict-mode)))

;; parenthesis pairs are always highlighted
(after! smartparens
  (show-smartparens-global-mode 1))

;; evil-goggles setting to make them work with evil-cleverparens

(use-package! evil-goggles
  :hook (doom-first-input . evil-goggles-mode)
  :config
  (setq evil-goggles-duration 0.15
        evil-goggles-enable-delete t)

  (pushnew! evil-goggles--commands
            '(evil-cp-delete
              :face evil-goggles-delete-face
              :switch evil-goggles-enable-delete
              :advice evil-goggles--generic-blocking-advice)
            '(evil-cp-delete-line
              :face evil-goggles-delete-face
              :switch evil-goggles-enable-delete
              :advice evil-goggles--delete-line-advice)
            '(evil-cp-delete-sexp
              :face evil-goggles-delete-face
              :switch evil-goggles-enable-delete
              :advice evil-goggles--generic-blocking-advice)
            '(evil-cp-delete-enclosing
              :face evil-goggles-delete-face
              :switch evil-goggles-enable-delete
              :advice evil-goggles--generic-blocking-advice)
            '(evil-cp-yank
              :face evil-goggles-yank-face
              :switch evil-goggles-enable-yank
              :advice evil-goggles--generic-async-advice)
            '(evil-cp-yank-line
              :face evil-goggles-yank-face
              :switch evil-goggles-enable-yank
              :advice evil-goggles--generic-async-advice)
            '(evil-cp-yank-sexp
              :face evil-goggles-yank-face
              :switch evil-goggles-enable-yank
              :advice evil-goggles--generic-async-advice)
            '(evil-cp-yank-enclosing
              :face evil-goggles-yank-face
              :switch evil-goggles-enable-yank
              :advice evil-goggles--generic-async-advice)
            '(evil-cp-change
              :face evil-goggles-change-face
              :switch evil-goggles-enable-change
              :advice evil-goggles--generic-blocking-advice)
            '(evil-cp-change-line
              :face evil-goggles-change-face
              :switch evil-goggles-enable-change
              :advice evil-goggles--generic-blocking-advice)
            '(evil-cp-change-sexp
              :face evil-goggles-change-face
              :switch evil-goggles-enable-change
              :advice evil-goggles--generic-blocking-advice)
            '(evil-cp-change-enclosing
              :face evil-goggles-change-face
              :switch evil-goggles-enable-change
              :advice evil-goggles--generic-blocking-advice)
            '(evil-paste-after
              :face evil-goggles-paste-face
              :switch evil-goggles-enable-paste
              :advice evil-goggles--paste-advice
              :after t)
            '(evil-cp-copy-paste-form
              :face evil-goggles-paste-face
              :switch evil-goggles-enable-paste
              :advice evil-goggles--paste-advice
              :after t)))

;; After any jump caused by evil (searching, for example) center window on cursor
(after! evil
  (add-hook 'evil-jumps-post-jump-hook #'my-center-after-jump-a))

;; evil-matchit settings and initialization
(use-package! evil-matchit
  :hook (doom-first-file . global-evil-matchit-mode)
  :init
  (setq evilmi-quote-chars (string-to-list "'\"/")))

;; Settings for rotate-text: interchange pairs of words
(after! rotate-text
  (add-to-list 'rotate-text-words '("yes" "no"))
  (add-to-list 'rotate-text-words '("true" "false"))
  (add-to-list 'rotate-text-words '("ano" "ne")))

(map!
 (:prefix "g"
   :n "=" nil
   :nv "/" #'rotate-text
   :nv "+" #'evil-numbers/inc-at-pt
   :nv "-" #'evil-numbers/dec-at-pt))

;; evil-inflection settings
(use-package! evil-string-inflection
  :commands (evil-operator-string-inflection)
  :init
  (map! :prefix "g"
        :desc "String inflection" :o "~" #'evil-operator-string-inflection))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modifying org mode defaults
;;
(setq org-use-property-inheritance t ; it's convenient to have properties inherited
      org-list-allow-alphabetical t  ; have a. A. a) A) list bullets
      org-export-with-sub-superscripts '{} ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
      org-startup-indented t ; always start org-mode indented according to header levels and such
      org-adapt-indentation t
      org-hide-emphasis-markers nil ; dont hide emphasis markers. I want to know what my org mode file contains
      org-pretty-entities nil)
                                        ; that applies also for UTF8 specail characters. Eventhough this could be considered to make `t`

;; Nastavení věcí, co se spustí s org-mode
;; tedy org-autolist

(add-hook 'org-mode-hook (
                          lambda () (org-autolist-mode))
          )

;; Hook autoload function in `writing.el` to org-mode start
(add-hook 'org-mode-hook #'my-org-buffer-config-h)

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

;; Make interactively appear org-entities (links, emphasis markers, etc) in insert mode as they are written in file
(use-package! org-appear
  :defer t
  :init
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoemphasis t
        org-appear-autoentities t)
  (add-hook! evil-insert-state-entry (org-appear-mode 1))
  (add-hook! evil-insert-state-exit (org-appear-mode -1)))

;; The same for mathematical symbols and formulas
(use-package! org-fragtog
  :defer t
  :init
  (add-hook! evil-insert-state-entry (org-fragtog-mode 1))
  (add-hook! evil-insert-state-exit (org-fragtog-mode -1)))

;; (add-hook! org-mode :append #'org-appear-mode) ;; alternative to previous hook

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

;; Nastavení org-directory. Org-agenda-files čerpají ze stejného nastavení

(custom-set-variables
 '(org-directory "~/Documents/org")
 '(org-agenda-files (list org-directory)))

;; Nastavení defaultního souboru pro capture

(setq org-default-notes-file (concat org-directory "/notesDefaultFile.org"))

;; Nastavení způsobu zvýrazňování vlastní TODO-sekvencí
;; =org-emphasis-alist= je proměnná obsahující delimitery pro markup. Seznam delimiterů je bohužel hardcoded; nelze přidat další, ale lze redefinovat způsob zvýraznění
;; daných delimiterů. Níže je redefinice =+=; dalo by se redefinovat i =~=; kromě =:foreground= má zabarvení i parametr =:background=

(after! org
  (setq org-todo-keyword-faces
        '(("IMPORTANT" . (:foreground "red" :weight bold))
          ))
  (add-to-list 'org-emphasis-alist
               '("+" (:foreground "red")))
  )

;; Nastavení vlastních delimiterů pro zvýrazňování textu; text mezi =%= a =!= je zvýrazněn.
(require 'org-habit nil t)

(defun org-add-my-extra-fonts ()
  "Add alert and overdue fonts. =invisible t= způsobí zmizení delimiteru v zobrazeném textu, =nil= jej ponechá zobrazený. Jake delimitery jsou použity =%%= a =!!=. Řešeno dle: https://emacs.stackexchange.com/questions/35626/how-to-make-my-own-org-mode-text-emphasis-work-again/35632#35632"
  (add-to-list 'org-font-lock-extra-keywords '("\\(!!\\)\\([^\n\r\t]+\\)\\(!!\\)"
                                               (1 '(face org-habit-alert-face invisible nil)) (2 'org-habit-alert-face t) (3 '(face org-habit-alert-face invisible nil))) t)
  (add-to-list 'org-font-lock-extra-keywords '("\\(%%\\)\\([^\n\r\t]+\\)\\(%%\\)"
                                               (1 '(face org-habit-overdue-face invisible nil)) (2 'org-habit-overdue-face t) (3 '(face org-habit-overdue-face invisible nil))) t))

(add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-fonts)

;; Settings for org-roam-server package
;;
(use-package org-roam-server
  :defer t
  :after (org-roam server)
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

;; Make active org-special blocks
;;
(use-package! org-special-block-extras
  :after org
  :hook (org-mode . org-special-block-extras-mode))

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

;; For some projects it is better disabled. Works well only if all referenes are for latex defined in latex commands (ref, vref, cref) and not with org-mode syntax ([[]])
;; (defun my-latex-filter-removeOrgAutoLabels (text backend info)
;;   "Org-mode automatically generates labels for headings despite explicit use of `#+LABEL`. This filter forcibly removes all automatically generated org-labels in headings."
;;   (when (org-export-derived-backend-p backend 'latex)
;;     (replace-regexp-in-string "\\\\label{sec:org[a-f0-9]+}\n" "" text)))

 (add-to-list 'org-export-filter-verbatim-functions
              'my-latex-filter-inlineCodeHighlight)

 (add-to-list 'org-export-filter-src-block-functions
              'my-latex-export-src-blocks)
			  
 ;; (add-to-list 'org-export-filter-headline-functions
 ;;              'my-latex-filter-removeOrgAutoLabels)

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

;;
;; Writeroom settings
;;
;; Tweaks doom emacs zooming
(setq +zen-text-scale 0.9)

;;
;; Completion Settings
;;

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

;; Increase completion history size
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; allow Ispell in text, markdown and gfm modes
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

;; company-dabbrev-mode in R-coding
(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))

;;
;; Yasnippets settings
;;

;; allows for nested snippets
;;
(setq yas-triggers-in-field t)

;; Treemacs Project management package settings
;;
;; Sets ignored file extensions
(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))

;; This defines specific file extensions that are ignored by treemacs
(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))

;; Přidání dalších zpráv při odchodu z Doom Emacs
;;
(setq +doom-quit-messages '(;;from doom 2
                            "Don't go now, there's a dimensional shambler waiting at the dos prompt!"
                            "Get outta here and go back to your boring programs."
                            "If I were your boss, I'd deathmatch ya in a minute!"
                            "Look, bud. You leave now and you forfeit your body count!"
                            "You're lucky I don't smack you for thinking about leaving."))
