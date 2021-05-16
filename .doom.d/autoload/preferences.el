;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Recnter cursor on window after erforming search

;;;###autoload
(defadvice! my-center-after-jump-a (&rest _)
  :after 'evil-ex-search
  (evil-scroll-line-to-center nil))
