;;;###autoload
(defun thi/org-buffer-config-h ()
  "Configure all aspects of an Org buffer right before we display it to the user."
  ;; Enable minor modes
  ;; (+zen/toggle)              ;; dont want that
  ;; (variable-pitch-mode 1)    ;; also not using this
  ;; (visual-line-mode 1)       ;; already set up
  ;; Call org configuration functions
  (org-display-inline-images)
  ;; Ignore flycheck errors on source blocks
  ;; (my-noflycheck-h)          ;; turn this off for the time being
  ;; Force a buffer refresh to guarantee all setup is in use
  (set-window-buffer nil (current-buffer)))

;; From Thiago Alvez
;; In order to use it, you have to define a keymap for the repeatable keys, and associate that keymap to the command that will start the repeat.

;;;###autoload
(defmacro my-repeat-map! (map-name keys-alist &optional docstring)
  "A helper macro to create keymaps for repeatable actions.

MAP-NAME is the variable name for the sparse keymap created, and KEYS-ALIST, is
an association list of functions to keys, where each function is called after
the associated key is pressed after the repeatable action is triggered."
  `(defvar ,map-name
     (let ((map (make-sparse-keymap)))
       (dolist (cmd ,keys-alist)
         (define-key map (cdr cmd) (car cmd))
         (put (car cmd) 'repeat-map ',map-name))
       map)
     ,docstring))

;; In order to be able to use repeat-mode everywhere, I will turn it on after Emacs initializes:

(add-hook 'after-init-hook 'repeat-mode)
