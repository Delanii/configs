(in-package #:nyxt-user)

;; Other parts of the configuration
;;
(dolist (file (list (nyxt-init-file "config/keybinds.lisp")
                    (nyxt-init-file "config/status.lisp")
                    (nyxt-init-file "config/style.lisp")))
  (load file))

;; Always restore previous session
(define-configuration browser
    ((session-restore-prompt :always-restore)))

;; Always be in vi-mode
(define-configuration (buffer internal-buffer editor-buffer prompt-buffer)
  ((default-modes `(vi-mode ,@%slot-default%))))

;; Have prompt buffer always in vi-insert mode
(define-configuration (prompt-buffer)
  ((default-modes (append
                   '(vi-insert-mode)
                   %slot-default%))))

;; Specify letters which are used for targeting objects on screen
(define-configuration nyxt/web-mode:web-mode
    ;; QWERTY home row.
    ((nyxt/web-mode:hints-alphabet "DSJKHLFAGNMXCWEIO")))

;;; This make auto-mode to prompt me about remembering this or that
;;; mode when I toggle it.
(define-configuration nyxt/auto-mode:auto-mode
  ((nyxt/auto-mode:prompt-on-mode-toggle t)))

(define-command-global eval-expression ()
  "Prompt for the expression and evaluate it, echoing result to the `message-area'."
  (let ((expression-string
          ;; Read an arbitrary expression. No error checking, though.
          (first (prompt :prompt "Expression to evaluate"
                         :sources (list (make-instance 'prompter:raw-source))))))
    ;; Message the evaluation result to the message-area down below.
    (echo "~S" (eval (read-from-string expression-string)))))
