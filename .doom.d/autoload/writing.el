;;;###autoload
(defun my-org-buffer-config-h ()
  "Configure all aspects of an Org buffer right before we display it to the user."
  ;; Enable minor modes
  ;; (+zen/toggle)              ;; dont want that
  (valign-mode 1)
  ;; (variable-pitch-mode 1)    ;; also not using this
  ;; (visual-line-mode 1)       ;; already set up
  ;; Call org configuration functions
  (org-display-inline-images)
  ;; Ignore flycheck errors on source blocks
  ;; (my-noflycheck-h)          ;; turn this off for the time being
  ;; Force a buffer refresh to guarantee all setup is in use
  (set-window-buffer nil (current-buffer)))
