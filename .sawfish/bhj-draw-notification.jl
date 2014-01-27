
(define-structure bhj-draw-notification

    (export bhj-draw-notification)

    (open rep
          rep.system
          rep.regexp
          rep.data.records
          sawfish.wm.colors
          sawfish.wm.events
          sawfish.wm.fonts
          sawfish.wm.images
          sawfish.wm.misc
          sawfish.wm.util.x
          sawfish.wm.windows)

;;; variables

  ;; margins
  (defconst x-margin 10)
  (defconst y-margin 10)

  (defconst icon-size 96)

  ;; window currently displayed, or nil
  (define info-window nil)
  (define Kill-image (make-image "~/.sawfish/Kill.png"))

;;; utilities

  ;; type used to encapsulate a vertically stacked set of string, the
  ;; font used to draw them, and the screen space they would take when
  ;; drawn


  ;; Calculates where to put an info window associated with W with size
  ;; DIMS
  (define (get-window-pos w dims)
    (let ((head (current-head w)))
      (if head
          (cons (+ (quotient (- (car (head-dimensions head)) (car dims)) 2)
                   (car (head-offset head)))
                (+ (quotient (- (cdr (head-dimensions head)) (cdr dims)) 2)
                   (cdr (head-offset head))))
        (cons (quotient (- (screen-width) (car dims)) 2)
              (quotient (- (screen-height) (cdr dims)) 2)))))

;;; entry point

  ;; What must be shown?
  ;;  * The window icon at left.
  ;;  * At right, the window's title and (maybe) its class.

  (define line-height (+ icon-size 4))
  (define (bhj-draw-notification #!optional close icon text-head text-content)
    "Shows the icon, the head, and the content"

    ;; if there's an old window, destroy it
    (when info-window
      (x-destroy-window info-window)
      (setq info-window nil))

    (unless close
      (let* ((2x-margin (* 2 x-margin))
             (2y-margin (* 2 y-margin))
             (win-size (cons (+ (* 3 x-margin)
                                icon-size
                                (max (text-width text-head default-font)
                                     (apply max
                                            (mapcar (lambda (line) (text-width line default-font))
                                                    (string-split "\n" text-content)))))
                             (+ (* 3 y-margin)
                                (* (1+ (length (string-split "\n" text-content)))
                                   line-height)))))

        (define (event-handler type xw)
          ;; XW is the handle of the X drawable to draw in
          (case type
            ((expose)
             (x-clear-window xw)

             (when icon
               (x-draw-image icon xw
                             (cons x-margin
                                   y-margin)
                             (cons icon-size icon-size)))

             ;; draw lines of text one at a time
             (let ((gc (x-create-gc xw
                                    `((foreground . ,(get-color "black"))
                                      (background . ,(get-color "white")))))
                   (x (+ (* 2 x-margin) icon-size)))

               (x-draw-string xw gc (cons x
                                          (+ y-margin
                                             (font-ascent default-font)
                                             (quotient (- line-height
                                                          (font-height default-font)) 2)))
                              text-head)
               (let* ((y (+ 2y-margin
                            line-height
                            (font-ascent default-font)
                            (quotient (- line-height
                                         (font-height default-font)) 2))))
                 (x-draw-string xw gc (cons x y) text-content))
               (x-destroy-gc gc)))))

        ;; create new window
        (setq info-window (x-create-window
                           (get-window-pos (car (managed-windows)) win-size) win-size 1
                           `((background . ,(get-color "white"))
                             (border-color . ,(get-color "black")))
                           event-handler))
        (x-map-window info-window)))))
