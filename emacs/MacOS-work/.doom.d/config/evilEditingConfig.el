(setq evil-move-cursor-back nil)        ;; After switch from normal mode to insert mode dont move cursor back on letter but leave it where it was.
(setq evil-kill-on-visual-paste nil)    ;; When pasting over selected text delete replace selected text with pasted one
(after! evil
  (setq evil-ex-substitute-global t)) ;; Evil substitution with `:s/.../...`  are always global, opposed to need to write `:%s/.../...`

(setq evil-visual-region-expanded t)    ;; emacs "region" and vim "selection" mean the same.

;; Smarparens (parenthesis completion and protection from unbalanced parenthesis) activate only in programming modes
(use-package! smartparens
  :init (add-hook 'smartparens-strict-mode-hook #'evil-cleverparens-mode)
  :hook ((prog-mode . smartparens-strict-mode)))

;; parenthesis pairs are always highlighted
(after! smartparens
  (show-smartparens-global-mode 1))

;; After any jump caused by evil (searching, for example) center window on cursor
(after! evil
  (add-hook 'evil-jumps-post-jump-hook #'my-center-after-jump-a))

;; evil-matchit settings and initialization
(use-package! evil-matchit
  :hook (doom-first-file . global-evil-matchit-mode)
  :init
  (setq evilmi-quote-chars (string-to-list "'\"/")))

(map!
 (:prefix "g"
   :n "=" nil
   :nv "/" #'rotate-text
   :nv "+" #'evil-numbers/inc-at-pt
   :nv "-" #'evil-numbers/dec-at-pt))

;; String inflection cycling -- more reliable than evil-string-inflection version
(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)
  (after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One binding I feel is missing on Doom, is the ability to evaluate any sexp on any mode, like a global eval-last-sexp. Gladly, it's easy enough to create my own:
(map! :prefix "g"
      :desc "Eval last sexp" :n ")" #'eval-last-sexp)

;; o avoid this issue, I will define a repeatable map for resizing, and because this is a separate map from the normal window map, I can add couple extra keys there to make the operation even smoother, for instance, we can increase the window height with + or with the = key
(my-repeat-map! my-window-resize-repeat-map
                '((evil-window-increase-height . "+")
                  (evil-window-increase-height . "=")
                  (evil-window-decrease-height . "-")
                  (evil-window-decrease-height . "_")
                  (evil-window-increase-width . ">")
                  (evil-window-decrease-width . "<"))
                "Repeatable map for window resizing")
