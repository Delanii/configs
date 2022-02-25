;; Aggresive autosave
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs allows you to define some advices to any function you want. This allows you to hook some code when the user, or the system calls such functions. On the case of aggressive auto-save, I want Emacs to save any content when I try to close the buffer associated with the content, of if I'm trying to quit Emacs. To do so, I'll create an autoload advice to perform such work:

;;;###autoload
(defadvice! my-save-all-buffers-a (&rest _)
  "Advice to run `save-some-buffers' before a function.

This is a part of the full auto-save feature."
  :before (list #'save-buffers-kill-emacs #'my-tabs-close-buffer-tab)
  (save-some-buffers t nil))

;; And to wrap up this configuration, I enable the auto-save mode:

(auto-save-visited-mode 1)

;; Call the same advice as a normal function when Emacs executes the auto-save hook:

(add-hook! 'auto-save-hook #'my-save-all-buffers-a)

;; And add the advice function to the after-focus-change-function as well, just to make sure Emacs save any outstanding buffer as soon as possible if it loses focus:

(add-function :after after-focus-change-function #'my-save-all-buffers-a)

;; Another core behavior that does not play well with my aggressive auto-save configuration is how Doom configures ws-butler. While I understand the rationale behind the choice of always remove blank spaces regardless the cursor position, I rather the plugin to not remove blank spaces before the cursor.
;; To adjust that, let's make ws-butler behave how I expect it:

(after! ws-butler
  (setq ws-butler-keep-whitespace-before-point t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Recenter cursor on window after performing search

;;;###autoload
(defadvice! thi/center-after-jump-a (&rest _)
  :after 'evil-ex-search
  (evil-scroll-line-to-center nil))
