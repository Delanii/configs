;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Recenter cursor on window after performing search

;;;###autoload
(defadvice! thi/center-after-jump-a (&rest _)
  :after 'evil-ex-search
  (evil-scroll-line-to-center nil))
