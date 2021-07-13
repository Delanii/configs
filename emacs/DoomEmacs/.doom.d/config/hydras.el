;; Empty yet, test a smple hydra for `g +`, `g -` and such ...

;; Define my hydras!!

;; Hydra for incrementing numbers
(defhydra my/hydra-numbers ()           ;; This argument can be set to invoke hydra directly by keybinding; for example `evil-normal-state-map "n"` makes the hydra be accessible also directly with pressing `n` while in evil normal state
  "Increment or decrement numbers. Very little hydra just to learn."
  ("+" evil-numbers/inc-at-pt "Increment" :column "Add")
  ("-" evil-numbers/dec-at-pt "Decrement" :column "Decrease")
  ("q" nil "quit" :column "Aux"))

;; Hydra for grouping and conviniently toggling modes that are sometimes nagging me
(defhydra my/hydra-toggle-modes ()
  "Toggle some auto-formatting minor modes that are mostly helpfull, but sometimes drag to deal with."
  ("a" (lambda ()
         (interactive)
         (if (eq major-mode 'org-mode)
             (org-autolist-mode) ;; if in org-mode, toggle org-autolist mode
           (message "Buffer not in org-mode."))) "Org-autolist-mode")
  ("b" (abbrev-mode) "Abbrev mode")
;; This still doesnt work. The main "troublemaker" in parenthessis checking is `smartparens-mode` so I stick with disablign that for now ...
;;  ("p" (progn
;;           (smartparens-mode 'toggle)
;;           (evil-cleverparens-mode 'toggle)) "Toggle parens checking")
  ("p" (smartparens-mode 'toggle) "Toggle parens checking")
  ("g" (god-mode) "God mode")
  )

;; Hydra for more convenient avy invokation
(defhydra my/hydra-avy ()
  "Improve access to Avy functions in Doom Emacs with my custom hydras, to not intrude into Doom Emacs settings."
  ("1" avy-goto-char "Go 2 char" :column "Chars")
  ("2" avy-goto-char-2 "Go 2 2 chars" :column "Chars")
  ("w" avy-goto-word-1 "Go 2 word starting with 2 chars" :column "Words")
  ("l" avy-goto-line "Go 2 line" :column "Lines")
  ("q" nil "quit" :column "Aux"))

;;
;; Setup global entry keybinding for my hydras

(map! :leader ;; Have it bound globally after pressing <leader> key, which is `SPC`
      :prefix ("y" . "hydras") ;; Access hydras with <y> key after <leader>, label it in `which-key` minibuffer as `hydras`
      "n" #'my/hydra-numbers/body
      "m" #'my/hydra-toggle-modes/body
      "a" #'my/hydra-avy/body)      ;; List of hydras with theyre access keys

;; With `general.el` it would be done like so:
;; References:
;;
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/56
;; https://github.com/noctuid/general.el#about
;;
;; (defconst my-leader "SPC y")
;; (general-define-key :prefix my-leader
;;                     "n" #'my-hydra-number/body)
