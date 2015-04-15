
(define-structure bhj-draw-wininfo

    (export bhj-draw-wininfo)

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

  (defconst icon-size 32)

  ;; window currently displayed, or nil
  (define info-window nil)
  (define Kill-image (make-image "~/system-config/.sawfish/Kill.png"))

;;; utilities

  ;; type used to encapsulate a vertically stacked set of string, the
  ;; font used to draw them, and the screen space they would take when
  ;; drawn
  (define-record-type :text-item
    (make-text-item-1 string font width height)
    ;; [no predicate]
    (string ti-string)
    (font ti-font)
    (width ti-width)
    (height ti-height))

  (define (make-text-item string font)
    (make-text-item-1 string font
		      (text-width string font)
		      (font-height font)))

  (define-record-type :window-info
    (make-window-info-1 window text icon selected height width font)
    (window wi-get-window)
    (text wi-get-text)
    (icon wi-get-icon wi-set-icon)
    (selected wi-get-selected wi-set-selected)
    (font wi-get-font)
    (height wi-get-height wi-set-height)
    (width wi-get-width wi-set-width))

  (define (make-window-info w font)
    (let* ((text (window-info w))
           (wi (make-window-info-1 w ; window
                                   text ; text
                                   (let ((i (window-icon-image w)))
                                     (or (and i (scale-image i icon-size icon-size))
                                         Kill-image)) ; icon
                                   nil ; selected
                                   (+ 4 (max icon-size (font-height font))) ; height
                                   (+ (* 3 x-margin)
                                      icon-size
                                      (text-width text font)) ; width
                                   font)))
      wi))

  (define (make-window-list-info wlist font)
    (do ((wlist wlist (cdr wlist))
         (wi-list nil))
        ((null wlist ) wi-list)
      (setq wi-list (append wi-list (list (make-window-info (car wlist) font))))))

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

  ;; Returns a list of string describing window W in some way
  (define (window-info w)
    (concat (and (window-get w 'iconified) ?[)
                 (window-name w)
                 (and (window-get w 'iconified) ?])

            (let ((class-name (get-x-text-property w 'WM_CLASS)))
              (when class-name
                  (concat "  <" 
                          (aref class-name 1)
                          ">")))))

;;; entry point

  ;; What must be shown?
  ;;  * The window icon at left.
  ;;  * At right, the window's title and (maybe) its class.


  (define (bhj-draw-wininfo wlist #!optional close match active)
    (setq active (or active 0)
          match (or match ""))
    (let* ((wi-list (make-window-list-info wlist default-font))
           (match-list (delete "" (string-split "\\s+" match))))
      
      (bhj-draw-wi-list close (if match-list
                            (filter (lambda (wi)
                                      (if (= (length (filter (lambda (match-item)
                                                    (string-match match-item (wi-get-text wi) 0 t))
                                                  match-list))
                                             (length match-list))
                                          t
                                        nil))
                                    wi-list)
                          wi-list) active (car wlist) (concat match "_"))))

  (define line-height (+ icon-size 4))

  (define (bhj-draw-wi-list close wi-list active w match)
    "Shows window information about those windows in wlist,
the active window ACTIVE should be highlighted"

    ;; if there's an old window, destroy it
        
    (when info-window
      (x-destroy-window info-window)
      (setq info-window nil))

    (unless wi-list ;; there will be errow down below
      (setq wi-list (cons (make-window-info-1
                           nil 
                           "no more matching windows!" 
                           Kill-image
                           nil 
                           line-height 
                           (+ (* 3 x-margin) icon-size (text-width "no more matching windows!" default-font))
                           default-font))))
	
    (unless close
      (let* ((dealing 0)
             (max-display (length wi-list))
             (active (if (and (>= active 0) (< active max-display))
                         active
                       (% active max-display)))
             (active (if (< active 0)
                         (+ active max-display)
                       active))
             icon
             text
             (2x-margin (* 2 x-margin))
             (2y-margin (* 2 y-margin))
             (win-size (cons (max (apply max
                                         (mapcar (lambda (wi) (wi-get-width wi))
                                                 wi-list))
                                  (+ 2x-margin (text-width match default-font)))

                             (+ (* 3 y-margin)
                                (* (1+ max-display)
                                   line-height)))))

	(define (event-handler type xw)
	  ;; XW is the handle of the X drawable to draw in
	  (case type
	    ((expose)
	     (x-clear-window xw)

	     ;; draw the icon
             (setq dealing 0)
	     (while (< dealing max-display)
               (setq icon (wi-get-icon (nth dealing wi-list)))
               (when icon
                 (x-draw-image icon xw
                               (cons x-margin
                                     (+ 2y-margin
                                        line-height
                                        (* dealing
                                           line-height)
                                        2))))
               (setq dealing (1+ dealing)))
             
	     ;; draw lines of text one at a time
	     (let ((gc (x-create-gc xw
				    `((foreground . ,(get-color "black"))
				      (background . ,(get-color "white")))))
		   (x (+ (* 2 x-margin) icon-size)))

               (x-draw-string xw gc (cons (quotient (- (car win-size) (text-width match default-font)) 2)
                                          (+ y-margin
                                             (font-ascent default-font)
                                             (quotient (- line-height
                                                          (font-height default-font)) 2)))
                              match)
               (let* ((rest wi-list)
                      (y (+ 2y-margin
                            line-height
                            (font-ascent default-font)
                            (quotient (- line-height
                                         (font-height default-font)) 2))))
                 (while rest
                   (x-draw-string xw gc (cons x y)
                                  (if (car rest)
                                      (wi-get-text (car rest))
                                    "") (wi-get-font (car rest)))
                   (setq rest (cdr rest)
                         y (+ y line-height))))

	       (x-destroy-gc gc))
             
             (let ((gc (x-create-gc xw
                                    `((foreground . ,(get-color "yellow"))
                                      (background . ,(get-color "green"))
                                      (function . ,'xor)))))
               (x-fill-rectangle xw 
                                 gc 
                                 (cons (+ 2x-margin icon-size) (+ 2y-margin
                                            line-height
                                            (* active line-height)))
                                 (cons (- (car win-size)
                                          2x-margin
                                          x-margin
                                          icon-size
                                          ) 
                                       line-height))
               (x-destroy-gc gc))

             )))

	;; create new window
	(setq info-window (x-create-window
			   (get-window-pos w win-size) win-size 1
			   `((background . ,(get-color "white"))
			     (border-color . ,(get-color "black")))
			   event-handler))
	(x-map-window info-window)
        (wi-get-window (nth active wi-list))))))
