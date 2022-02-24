;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;;
;; Packages for modal editing
;;
;; Packages connected to evil-mode
(package! evil-matchit)
(package! evil-tutor)
(package! evil-nerd-commenter)
(package! evil-cleverparens)
(package! string-inflection :pin "fd7926ac17293e9124b31f706a4e8f38f6a9b855")
(package! evil-escape :disable t)

;; Packages connected to other modal editing
(package! god-mode) ;; Již v Doom Emacs - ale při povolení v =init.el= aktivuje god-mode všude, což mě mate ... Možná bych si na to zvykl, testovat. Prozatím zapnuto zde a vypnuto v =init.el=
(package! ryo-modal)
(package! modalka)
(package! kakoune)

;; Package for dealing with hydras -- hydra + which-key
(package! hercules)

;; Organizace bufferů
(package! bufler) ;; temporarily disabled because of error -- recursive require for feature bufler-workspace-tabs

;; Settings for package for dealing with very large files (vlf package, vlfi on github is itůs development version)
;;
(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c" :disable t)

;;
;; Packages connected to writing and `pandoc`
;;
(package! wc-mode)
(package! pandoc-mode)
(package! expand-region)
(package! focus)

;; Package for import files with pandoc
(package! org-pandoc-import :recipe
  (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))

;; Package to remove text from buffer "Active" area to some out-of-text area
;; Provides:
;;
;; Send selected text to the bottom of the buffer -- C-c C-r: Send selected text to bottom of buffer
;; Send selected text to the top of the buffer -- C-c C-s: Send selected text to top of buffer
;; Send selected text to a trash file -- C-c C-q: Send selected text to trash file
;;
(package! palimpsest-mode)

;; Documentation packages
;;
(package! tldr)

;;
;; Packages connected specifically to org-mode
;;

;; Unpin org-mode -- bugs might happen
;; (unpin! org-mode)

;; Make agenda super
(package! org-super-agenda :pin "a5557ea4f51571ee9def3cd9a1ab1c38f1a27af7")

(package! org-appear
  :recipe (:host github
           :repo "awth13/org-appear"))

(package! org-fancy-priorities
  :disable t)

(package! org-fragtog)

(package! org-autolist)

;; Org citations

(package! org-ref)
(package! citar :pin "a6926650114a8091f98bff8c7fd00add82043190")
(package! citeproc :pin "38e70c0a94eeefe86ddefc38dfa8ab2311008774")
(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate") :pin "8f49ccbd337edda01e52da0c75f6a76e2cc976f7")
(package! org-ref) ;; nevyužívám jej a zbytečně instaluje "helm"

;; Settings for org-roam-v2 per =https://github.com/org-roam/org-roam-ui/=
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! org-ql
  :recipe (:host github :repo "cfroehli/org-ql"))
;; (package! org-plus-contrib) ;; rozbije export z org-mode do jiných formátů
(package! org-pdftools)
(package! navi)

;; Package for using =defblock= function and for simultanious export to html, latex or other formats
(package! org-special-block-extras)

;; Declarative org capture tempates
(package! doct
  :recipe (:host github :repo "progfolio/doct")
  :pin "67fc46c8a68989b932bce879fbaa62c6a2456a1f")

;;
;; Experimental packages
;;
;; Org-mode text transclusion via reference from a file
(package! org-transclusion :recipe (:host github :repo "nobiot/org-transclusion"))

;; Outline packages - experimental
(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view"))
(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))
(package! org-sidebar)
(package! org-ml)
;; (package! hyperbole) ;; Not using it at the moment

(package! org-auto-tangle)

;; Souštění kódu v org-mode

(package! ob-julia :recipe (:host github :repo "nico202/ob-julia" :files ("*.el" "julia")))
;; (package! ob-raku) ;; Není na MELPA, ručně jej instalovat nebudu

;; Make HTTP requests from org-mode
(package! ob-http :pin "b1428ea2a63bcb510e7382a1bf5fe82b19c104a7")

;; Spouštění kódu ve Scale
;; (package! ob-scala) ;; Opět není v MELPA

;; Balíčky pro LaTeX které nejsou v distribuci
;;
;; Mimojiné umožňuje folding jako v org-mode
(package! latex-extra)

;; Package for reading =epub= files in emacs
;;
(package! nov)

;; actions + links package
(package! embark)

;; Balíčky pro programování a programovací jazyky
;;
;; Všeobecná úprava programovacích rozhraní
;; (package! outorg) ;; Umožňuje pracovat s kódem jako s org-mode, jen je třeba vkládat do kódu komentáře formátované jako napisy org-mode; zde ale pro změnu dělá interferenci s org-autolist, protože to vypadá, že způsobuje změny formátování při psaní "+" apod ...
(package! prism)                        ;; Mění zvýrazňování syntaxe tak, aby reflektovala i hloubku závorek, cyklů, atp ...

;; Následující balíček by měl zvýšit rychlost parsování zdrojového kódu v souborech
(package! tree-sitter)
(package! tree-sitter-langs)

;; cider je IDE pro clojure. Umožňuje evaluaci clojure v org-mode
(package! cider)

;; Úpravy dired
;;
(package! dired-open)

;; Balíček pro vytváření dalších expanzí a regexů; myslím
;;
(package! ample-regexps)
;; a pro lepší funkcionální programování v elisp

(package! dash)

;; Balček pro rich content v eshell
(package! shx)

;; Balíček exwm - window manager pro celé OS uvnitř Emacs - spustit při loginu do PC
;;
(package! exwm)
(package! dmenu)

;; Balíčky integrace dalších programovacích jazyků do emacs
;;
;; Haskell
(package! haskell-emacs)

;; Profiling doom emacs startup
(package! benchmark-init)

