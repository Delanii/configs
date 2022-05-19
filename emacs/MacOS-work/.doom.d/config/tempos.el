;;; config/tempos.el -*- lexical-binding: t; -*-

;; test tempo to insert a todo heading in org-mode
;;
(tempo-define-template "my/tempo/todo-heading"
                       '((p "Level: " level)
                         " TODO"
                         " [#" (p "Priority: " priority) "] "
                         (p "Heading text: " heading)
                         " [/]"
                         n)
                       nil
                       "Insert a TODO heading in org mode.")
