;; Empty yet, test a smple hydra for `g +`, `g -` and such ...

;; Define my hydras!!

(defhydra my-hydra-numbers (evil-normal-state-map "n")
  "Increment or decrement numbers. Very little hydra just to learn."
  ("+" evil-numbers/inc-at-pt "Increment" :column "Add")
  ("-" evil-numbers/dec-at-pt "Decrement" :column "Decrease")
  ("q" nil "quit" :column "Aux"))

;;
;; Setup global entry keybinding for my hydras

(map! :leader                           ;; Have it bound globally after pressing <leader> key, which is `SPC`
      :prefix ("y" . "hydras")          ;; Access hydras with <y> key after <leader>, label it in `which-key` minibuffer as `hydras`
      "n" #'my-hydra-numbers/body)      ;; List of hydras with they access keys

;; With `general.el` it would be done like so:
;;
;; (defconst my-leader "SPC y")
;; (general-define-key :prefix my-leader
;;                     "n" #'my-hydra-number/body)
