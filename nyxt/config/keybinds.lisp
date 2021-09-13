(in-package #:nyxt-user)

;;; Add basic keybindings.
;;;
;;; If you want to have VI bindings overriden, just use `scheme:vi'
;;; instead of `scheme:emacs'.
;;;
;;; `keymap-scheme' hosts several schemes inside a has-table, thus the
;;; `gethash' business
;; (define-configuration nyxt/web-mode:web-mode
;;   ((keymap-scheme (let ((scheme %slot-default%))
;;                     (keymap:define-key (gethash scheme:vi scheme)
;;                      "M-:" 'eval-expression)
;;                     scheme))))
