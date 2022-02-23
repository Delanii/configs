(setq evil-move-cursor-back nil)        ;; After switch from normal mode to insert mode dont move cursor back on letter but leave it where it was.
(setq evil-kill-on-visual-paste nil)    ;; When pasting over selected text delete replace selected text with pasted one
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
