;;; config/skeletons.el -*- lexical-binding: t; -*-

;; test skeleton to insert a todo heading in org-mode
;;
(define-skeleton my/skeleton/todo-heading
  "Insert a TODO heading in org mode."
  "Heading text: "
  (setq v1 (skeleton-read "Level: "))
  " TODO"
  " [#" (setq v2 (skeleton-read "Priority: ")) "] "
  str
  " [/]" \n)
