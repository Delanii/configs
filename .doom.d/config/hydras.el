;; Empty yet, test a smple hydra for `g +`, `g -` and such ...

;; Define my hydras!!

;; Hydra for incrementing numbers
(defhydra my/hydra-numbers (evil-normal-state-map "n")
  "Increment or decrement numbers. Very little hydra just to learn."
  ("+" evil-numbers/inc-at-pt "Increment" :column "Add")
  ("-" evil-numbers/dec-at-pt "Decrement" :column "Decrease")
  ("q" nil "quit" :column "Aux"))

;; Hydra for grouping and conviniently toggling modes that are sometimes nagging me
(defhydra my/hydra-toggle-modes (evil-normal-state-map "m")
  "Toggle some auto-formatting minor modes that are mostly helpfull, but sometimes drag to deal with."
  ("a" (lambda ()
         (interactive)
         (if (eq major-mode 'org-mode)
             (org-autolist-mode) ;; if in org-mode, toggle org-autolist mode
           (message "Buffer not in org-mode."))) "Org-autolist-mode")
  ("b" (abbrev-mode) "Abbrev mode")
  ("p" (smartparens-mode) "Smartparens-mode")
  ("g" (god-mode) "God mode")
  )

;;
;; Setup global entry keybinding for my hydras

(map! :leader                           ;; Have it bound globally after pressing <leader> key, which is `SPC`
      :prefix ("y" . "hydras")          ;; Access hydras with <y> key after <leader>, label it in `which-key` minibuffer as `hydras`
      "n" #'my/hydra-numbers/body
      "m" #'my/hydra-toggle-modes/body)      ;; List of hydras with theyre access keys

;; With `general.el` it would be done like so:
;; References:
;;
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/56
;; https://github.com/noctuid/general.el#about
;;
;; (defconst my-leader "SPC y")
;; (general-define-key :prefix my-leader
;;                     "n" #'my-hydra-number/body)
