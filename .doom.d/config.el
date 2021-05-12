;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tomas Krulis"
      user-mail-address "krulis.tomas.tk@gmail.com")

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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

;; Nastavení defaultního jazyka na češtinu

(ispell-change-dictionary "czech" t)

;; Nastavení spellchecku pro angličtinu a češtinu současně - funguje ve Spacemacs (`.spacemacs`), ale ne zde

;; (setq ispell-program-name "hunspell")
;; you could set `ispell-dictionary` instead but `ispell-local-dictionary' has higher priority
;; (setq ispell-local-dictionary "en_US")
;; (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,cs_CZ") nil utf-8)))

;; Start Emacs always maximized

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Změna fontu - spíš ukázka:
;; (setq doom-font (font-spec :family "Fira Mono" :size 12))

;; Vypnutí automatické indentace tabulátory:
(setq-default indent-tabs-mode nil)

;; General settings from tecosaur
;; odkaz: https://github.com/tecosaur/emacs-config/blob/master/config.org

(setq-default
 delete-by-moving-to-trash t) ; Delete files to trash

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t)                        ; Nobody likes to loose work, I certainly don't

(global-subword-mode 1)                           ; Iterate through CamelCase words

;;
;; Nastaveí pro balíčky modálních editorů (kromě evil)
;;
;; Nastavení balíčku 'boon
;; (require 'boon-qwertz)
;; alternativně 'boon-qwerty

;; Nastavení balíčku 'xah-fly-keys
;; (require 'xah-fly-keys)
;; (xah-fly-keys-set-layout "qwertz")

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

;;
;; Specifická nastavení pro org-mode
;;

;; Modifying org mode defaults
;;
(setq org-use-property-inheritance t              ; it's convenient to have properties inherited
      org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
      org-export-with-sub-superscripts '{})       ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}

;; Nastavení věcí, co se spustí s org-mode
;; tedy org-autolist

(add-hook 'org-mode-hook (
                          lambda () (org-autolist-mode))
          )

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
     "gk" nil
     "gs M-j" #'evil-next-line
     "gs M-k" #'evil-previous-line))
  ;; Move the evil-lion bindings out of the way too.
  (map!
   :nv "gl" nil
   :nv "gL" nil
   :nv "gzl" #'evil-lion-left
   :nv "gzL" #'evil-lion-right))

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

;; Does this have to be here?
;;
(require 'ox)
(require 'ox-html)
(require 'ox-latex)
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

;; Settings for org-mode import with pandoc
(use-package! org-pandoc-import
  :after org)

;; Nastavení org-babel-clojure-backend pro evaluaci kódu clojure v org
(setq org-babel-clojure-backend 'cider)

;; Umožnění spouštění Jupyter notebooků přes =ein=
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ein . t)
   (shell . t)))

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

;; Writeroom settings
;;
;; Tweaks doom emacs zooming
(setq +zen-text-scale 0.9)

;; Yasnippets settings
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
