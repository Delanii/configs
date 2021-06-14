(in-package #:nyxt-user)

;; Other part of the configuration
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
