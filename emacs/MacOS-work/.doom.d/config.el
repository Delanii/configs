;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config sources:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - https://tecosaur.github.io/emacs-config/config.html -- new functions defined prefixed by `tec`
;; - https://townk.github.io/doom-emacs-private/         -- new functions prefixed by `thi`
;; - https://gitlab.com/zzamboni/dot-doom                -- new functions defined prefixed by `zz`
;; - https://pages.sachachua.com/.emacs.d/Sacha.html

;; For package configurations, all package configuration should be wrapped in `(after! package-name)` macro, except for:
;;
;; - variables containing any paths or directory names
;; - variables specifically mentioned in theyre package documentation to be set before loading the package

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User identification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tomas Krulis"
      user-mail-address "tomas.krulis@make.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting environment variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fixes for stupid Mac
(setenv "DICTIONARY" "en_US")

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
(setq doom-font (font-spec :family "Fira Code" :size 13 :weight 'regular))
;; doesnt break when opening big files
;; (setq doom-font (font-spec :family "jetBrains Mono" :size 13 :weight 'regular))
;; (setq doom-font (font-spec :family "Source Code Pro" :size 14 :weight 'regular))
;; (setq doom-font (font-spec :family "monospace" :size 14 :weight 'regular))
;; In case emacs hangs during startup on white screen (usualy while opening big files), but is able to start with `emacs -q` or `emacs -nw`; it can be a font issue. The white-screen hanging can be nullified by pressing C-g while loading or setting different font, like this default `monospace`. Doom emacs defaults to `Symbola` font when it cannot find specified font, so I should have installed that too. On Arch, it is AUR package `ttf-symbola`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;;
;; Doom theme customization
;;
;; Comment face customization -- I use a lot of comments, so I have to be able to read that
;; (setq doom-one-brighter-comments t
;;       doom-one-comment-bg nil) ;; makes the coments in doom theme in brighter colors, but changes theyre background too without nullifying that

;; Purely lightening comment face colors
(custom-set-faces!
  `(font-lock-comment-face :foreground "#58A304") ;; just a tad-little-brighter than "green" from official doom one theme
  `(font-lock-doc-face     :foreground ,(doom-lighten 'teal .05)))

;; Customize hl-todo mode to run also in org-mode and to have more keywords
;; hl-todo mode doesn't work in org-mode -- org mode is excluded form it's processing
(global-hl-todo-mode)

;; Define my custom faces specific words
(defface my/make-face
  '((t :foreground "#EF25F1"
       :weight bold))
  nil)

(defface my/fixme-face
  '((t :foreground "#E5D822"
       :weight bold))
  nil)

(defface my/important-face
  '((t :foreground "#C70039"
       :weight bold))
  nil)

;; Add custom faces to hl-todo package.
;; Works everywhere except org-mode
(after! hl-todo
  (setq hl-todo-keyword-faces
        `(
          ("MAKE" . my/make-face)
          ("FIXME". my/fixme-face)
          ("IMPORTANT" . my/important-face))))

;; Line number highlighting customizaton
(custom-set-faces!
  '(line-number :foreground "#BE00ED")
  '(line-number-current-line :foreground "#bbc2cf"))

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
;; Version from tecosaur was making a lot of warnings, maybe because of eval from `org-roam`, so I removed it, now it seems working.

(setq frame-title-format
      '("" "%b"
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
(defadvice! tec/prompt-for-buffer (&rest _)
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

;; To make VLF available without delaying startup, we’ll just load it in quiet moments.
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spellcheck Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ispell-change-dictionary "en_US" t)

;; Nastavení spellchecku pro angličtinu a češtinu současně - funguje ve Spacemacs (`.spacemacs`), ale ne zde

;; (setq ispell-program-name "hunspell")
;; you could set `ispell-dictionary` instead but `ispell-local-dictionary' has higher priority
;; (setq ispell-local-dictionary "en_US")
;; (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,cs_CZ") nil utf-8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil - modal editor settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; except for those specific for org-mode

(load! "config/evilEditingConfig.el")

(defun my/god-mode-cursor-color ()
  "When in god-mode-local change cursor color to grey. Outside of god-mode-local restore original doom one cursor color."
  (setq cursor-color (if (bound-and-true-p god-local-mode)
                          (custom-set-faces!
                            '(cursor :background "#e5e5e5"))
                        (custom-set-faces!
                          '(cursor :background "#51afef")))))

(use-package! god-mode
  :defer t
  :config
  (add-hook 'post-command-hook #'my/god-mode-cursor-color))

;; Other writing settings

(use-package! palimpsest-mode
  :defer t)

;; Text rotations definitions
(load! "config/text-rotations.el")

;; Abbrev mode
 (dolist (hook '(org-mode-hook
                    TeX-latex-mode-hook
                    text-mode-hook))
      (add-hook hook #'abbrev-mode))

;; This function should prevent inserting space after expanded abbrev
(defun xah-abbrev-h-f ()
  "Abbrev hook function, used for `define-abbrev'.
 Our use is to prevent inserting the char that triggered expansion. Experimental.
 the “ahf” stand for abbrev hook function.
Version 2016-10-24"
  t)

(put 'xah-abbrev-h-f 'no-self-insert t)

;; Definitions for abbrev mode
(setq abbrev-file-name                   ;; tell emacs where to read abbrev
      "~/.doom.d/config/abbrev_defs.el")    ;; definitions from...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Own hydras and hercules settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load! "config/hydras.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load! "config/orgConfig.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq TeX-save-query nil
      TeX-show-compilation t
      TeX-command-extra-options "-shell-escape")
(after! latex
  (add-to-list 'TeX-command-list '("LuaLaTeX" "%`lualatex%(mode)%' %t" TeX-run-TeX nil t)))

(setq +latex-viewers '(pdf-tools okular evince))

;; SyncTeX ? From TEC
(after! tex
  (add-to-list 'TeX-view-program-list '("Okular" "okular %o"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Okular")))

;; Fix for emacs 28
(when EMACS28+
  (add-hook 'latex-mode-hook #'TeX-latex-mode))

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
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

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

;; allow yasnippets everywhere
;; (yas-global-mode 1) ;; Head Honcho said I should not do that ...

;; allow sharing some snippets in all modes
(add-hook 'yas-minor-mode-hook (lambda ()
                                 (yas-activate-extra-mode 'fundamental-mode)))

;; Helper functions used to generate some snippets
;;
(defun +yas/tec/org-last-src-lang ()
  "Return the language of the last src-block, if it exists."
  (save-excursion
    (beginning-of-line)
    (when (re-search-backward "^[ \t]*#\\+begin_src" nil t)
      (org-element-property :language (org-element-context)))))

;;
;; File searching settings
;;
;; Company settings
;; View with `Find file` (`SPC .`) also hidden files
(after! counsel
  (setq counsel-find-file-ignore-regexp "\\(?:^#\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

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
