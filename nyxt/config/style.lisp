(in-package #:nyxt-user)

(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#2D343B"
         :color "white")))))))

(define-configuration prompt-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '((body
               :background-color "#2D343B"
               :color "white")
              ("#prompt-area"
               :background-color "#2D343B")
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
              ("#selection"
               :background-color "#BE7AEA"
               :color "white")
              (.marked :background-color "#8B3A3A"
                       :font-weight "bold"
                       :color "white")
              (.selected :background-color "#19C8EB"
                         :color "black")))))))

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
  ((nyxt/web-mode:highlighted-box-style
    (cl-css:css
     '((".nyxt-hint.nyxt-highlight-hint"
        :background "#BE7AEA")))
    :documentation "The style of highlighted boxes, e.g. link hints.")))

(define-configuration status-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '(("#controls"
               :border-top "1px solid white")
              ("#url"
               :background-color "#2D343B"
               :color "white"
               :border-top "1px solid darkgray")
              ("#modes"
               :background-color "#2D343B"
               :border-top "1px solid darkgray")
              ("#tabs"
               :background-color "#8C05F0"
               :color "white"
               :border-top "1px solid white")))))))

(define-configuration nyxt/style-mode:dark-mode
  ((style #.(cl-css:css
             '((*
                :background-color "darkgray !important"
                :background-image "none !important"
                :color "white")
               (a
                :background-color "darkgray !important"
                :background-image "none !important"
                :color "#34B9E1 !important"))))))
