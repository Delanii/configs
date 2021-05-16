;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil mode autoload functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From https://townk.github.io/doom-emacs-private/
;; with editor state like this:
;; (
;;
;;)
;;
;; Does this:
;; ()
;;
;; Instead of this:
;; (
;; )

;;;###autoload
(defadvice! my-super-backward-delete-a (&rest _)
  "Special function to super-delete things.

If the line content before cursor contains only blank characters, this function
will delete all the blank characters, and then, join with the previous line. I
there is any non-blank character before cursor, this function will delete the
entire line, but keep the correct indentation on it."
  :before '+default--delete-backward-char-a
  (let* ((line-pos (- (point) (point-at-bol)))
         (prev-indent (save-excursion
                        (forward-line -1)
                        (current-indentation)))
         (prev-line-bol (point-at-bol 0))
         (next-line-eol (point-at-eol 2))
         (smart-bs-p (or (save-excursion
                           (and (re-search-backward "{[ \t]*\n[ \t]*" prev-line-bol t)
                                (re-search-forward "[ \t]*\n[ \t]*}" next-line-eol t)))
                         (save-excursion
                           (and (re-search-backward "\\[[ \t]*\n[ \t]*" prev-line-bol t)
                                (re-search-forward "[ \t]*\n[ \t]*\\]" next-line-eol t)))
                         (save-excursion
                           (and (re-search-backward "([ \t]*\n[ \t]*" prev-line-bol t)
                                (re-search-forward "[ \t]*\n[ \t]*)" next-line-eol t))))))
    (when (and smart-bs-p
               (<= line-pos (+ prev-indent standard-indent)))
      (delete-char (- line-pos)))))
