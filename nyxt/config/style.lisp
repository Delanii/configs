(in-package #:nyxt-user)

;; Sets message-buffer style -- line in bottom of the screen where all messages are displayed (same as echo-area in emacs)
(define-configuration window
    ((message-buffer-style
      (str:concat
       %slot-default%
       (cl-css:css
        '((body
           :background-color "#2D343B"
           :color "white")))))))


;;; Color config for prompt-buffer (minibuffer in Emacs parlance).
(define-configuration prompt-buffer
    ((style (str:concat
             %slot-default%
             (cl-css:css
              '((body
                 :background-color "#2D343B"
                 :color "white")
                ("#prompt-area"
                 :background-color "#2D343B")
                ;; The area you input text in.
                ("#input"
                 :background-color "#2D343B")
                (".source-name"
                 :color "black"
                 :background-color "#34B9E1")
                (".source-content"
                 :background-color "#2D343B")
                (".source-content th"
                 :border "1px solid #34B9E1"
                 :background-color "#2D343B")
                ;; The currently highlighted option.
                ("#selection"
                 :background-color "#BE7AEA"
                 :color "white")
                (.marked :background-color "#8B3A3A"
                 :font-weight "bold"
                 :color "white")
                (.selected :background-color "#19C8EB"
                 :color "black")))))))

;;; Internal (i.e. help, info, describe-* buffers).
;;;
;;; Panel buffers are the same in regards to style.
(define-configuration internal-buffer
    ((style
      (str:concat
       %slot-default%
       (cl-css:css
        '((title
           :color "#BE7AEA")
          (body
           :background-color "#2D343B"
           :color "white")
          (hr
           :color "white")
          (a
           :color "#34B9E1")
          (.button
           :color "white"
           :background-color "#34B9E1")))))))
;;; History-tree-mode is a mode used in `history-tree' and
;;; `buffer-history-tree' buffers. It's not enough to customize
;;; `internal-buffer' to cover it, thus I'm customizing it
;;; specifically.
;;;
(define-configuration nyxt/history-tree-mode:history-tree-mode
    ((nyxt/history-tree-mode::style
      (str:concat
       %slot-default%
       (cl-css:css
        '((body
           :background-color "darkgray"
           :color "lightgray")
          (hr
           :color "darkgray")
          (a
           :color "#34B9E1")
          ("ul li::before"
           :background-color "white")
          ("ul li::after"
           :background-color "white")
          ("ul li:only-child::before"
           :background-color "white")))))))

(define-configuration nyxt/web-mode:web-mode
    ;; The style of highlighted boxes, e.g. link hints.
    ((nyxt/web-mode:highlighted-box-style
      (cl-css:css
       '((".nyxt-hint.nyxt-highlight-hint"
          :background "#BE7AEA")))
      :documentation "The style of highlighted boxes, e.g. link hints.")))

;;; Status buffer is the strip above the message buffer/echo area.
;;; Modeline in Emacs parlance.
(define-configuration status-buffer
    ((style (str:concat
             %slot-default%
             (cl-css:css
               ;; Arrows on the left.
              '(("#controls"
                 :border-top "1px solid black")
                ;; To the right of the arrows.
                ("#url"
                 :background-color "#2D343B"
                 :color "white"
                 :border-top "1px solid black")
                ;; Far to the right.
                ("#modes"
                 :background-color "#2D343B"
                 :border-top "1px solid black")
                ;; The center segment.
                ("#tabs"
                 :background-color "#8C05F0"
                 :color "white"
                 :border-top "1px solid black")))))))

;;; Dark is a simple mode for simple HTML pages to color those in a
;;; darker palette. I have overriden some colors just to make them more simillar to other editor/application window backgrounds I am using.
(define-configuration nyxt/style-mode:dark-mode
    ((style #.(cl-css:css
               '((*
                  :background-color "#2D343B !important"
                  :background-image "none !important"
                  :color "white")
                 (a
                  :background-color "#2D343B !important"
                  :background-image "none !important"
                  :color "#34B9E1 !important"))))))
