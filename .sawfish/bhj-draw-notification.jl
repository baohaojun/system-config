
(define-structure bhj-draw-notification

    (export bhj-draw-notification)

    (open rep
          rep.data.tables
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

  (define-record-type :notification
    (new-notif title text icon window y)
    notif?
    (title notif-title notif-set-title!)
    (icon notif-icon notif-set-icon!)
    (text notif-text notif-set-text!)
    (window notif-window notif-set-window!)
    (y notif-y notif-set-y!))

  (define notification-table (make-table string-hash string=))

  ;; window currently displayed, or nil
  (define Kill-image (make-image "~/.sawfish/Kill.png"))

;;; utilities

  ;; type used to encapsulate a vertically stacked set of string, the
  ;; font used to draw them, and the screen space they would take when
  ;; drawn


  ;; Calculates where to put an info window associated with W with size
  ;; DIMS
  (define (get-window-pos w dims)
    (let ((head (current-head w))
          (current-mapped-notif-height 0))
      (table-walk (lambda (title a-notif)
                    (setq current-mapped-notif-height
                          (+ current-mapped-notif-height
                             (x-drawable-height (notif-window a-notif)))))
                  notification-table)
      (if head
          (cons (+ (quotient (- (car (head-dimensions head)) (car dims)) 1)
                   (car (head-offset head)))
                (+ (quotient (- (cdr (head-dimensions head)) (cdr dims)) 1)
                   (cdr (head-offset head))
                   (- current-mapped-notif-height)))
        (cons (quotient (- (screen-width) (car dims)) 1)
              (- (quotient (- (screen-height) (cdr dims)) 1)
                 current-mapped-notif-height)))))

;;; entry point

  ;; What must be shown?
  ;;  * The window icon at left.
  ;;  * At right, the window's title and (maybe) its class.

  (define line-height (+ (font-height default-font) 4))

  (define (bhj-close-notification title)
    (if title
        (let* ((notif (table-ref notification-table title))
               (window (and notif (notif-window notif)))
               (height-of-window 0)
               (y-of-window 0))
          (when window
            (setq height-of-window (x-drawable-height window)
                  y-of-window (notif-y notif))
            (x-destroy-window window)
            (table-unset notification-table title)
            (table-walk (lambda (title a-notif)
                          (format t "y-of-window is %d, current y is %d" y-of-window (notif-y a-notif))
                          (when (> y-of-window (notif-y a-notif))
                            (let ((new-y (+ (notif-y a-notif) height-of-window)))
                              (x-configure-window (notif-window a-notif)
                                                  `((y . ,new-y)))
                              (notif-set-y! a-notif new-y))))
                        notification-table)))
      (let ((titles))
        (table-walk (lambda (title a-notif)
                      (setq titles (cons title titles)))
                    notification-table)
        (format t "titles are %s" titles)
        (while titles
          (bhj-close-notification (or (car titles) "t"))
          (setq titles (cdr titles))))))

  (define (bhj-draw-notification #!optional close icon text-head text-content)
    "Shows the icon, the head, and the content"
    (if close
        (bhj-close-notification text-head)
      (when (table-bound-p notification-table text-head)
        (bhj-close-notification text-head))
      (let* ((2x-margin (* 2 x-margin))
             (2y-margin (* 2 y-margin))
             info-window
             win-xy
             (win-size (cons (+ (* 3 x-margin)
                                icon-size
                                (max (text-width text-head default-font)
                                     (apply max
                                            (mapcar (lambda (line) (text-width line default-font))
                                                    (string-split "\n" text-content)))))
                             (+ (* 3 y-margin)
                                (max icon-size (* (1+ (length (string-split "\n" text-content)))
                                                  line-height))))))

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
                                         (font-height default-font)) 2)))
                      (text-lines (string-split "\n" text-content))
                      line)
                 (while text-lines
                   (setq line (car text-lines) text-lines (cdr text-lines))
                   (x-draw-string xw gc (cons x y) line)
                   (format t "%s" (format nil "drawn %s with y: %d" line y))
                   (setq y (+ y line-height))))
               (x-destroy-gc gc)))))

        ;; create new window
        (setq info-window (x-create-window
                           (setq win-xy (get-window-pos (car (managed-windows)) win-size))
                           win-size
                           1
                           `((background . ,(get-color "white"))
                             (border-color . ,(get-color "black")))
                           event-handler))
        (x-map-window info-window)
        (table-set notification-table text-head (new-notif text-head text-content icon info-window (cdr win-xy)))))))
