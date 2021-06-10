;; Settings for rotate-text: interchange pairs of words
(after! rotate-text
  ;; English rotations
  (add-to-list 'rotate-text-words '("yes" "no"))
  (add-to-list 'rotate-text-words '("true" "false"))
  ;; Czech rotations
  (add-to-list 'rotate-text-words '("ano" "ne")))