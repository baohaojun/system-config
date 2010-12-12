(define-structure bhj-draw-wininfo

    (export bhj-draw-wininfo)

    (open rep
	  rep.system
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
  (defconst max-display 12)

  (defconst icon-size (32 . 32))

  ;; window currently displayed, or nil
  (define info-window nil)

;;; utilities

  ;; type used to encapsulate a vertically stacked set of strings, the
  ;; font used to draw them, and the screen space they would take when
  ;; drawn
  (define-record-type :text-item
    (make-text-item-1 strings font width height)
    ;; [no predicate]
    (strings ti-strings)
    (font ti-font)
    (width ti-width)
    (height ti-height))

  (define (make-text-item strings font)
    (make-text-item-1 strings font
		      (apply max (mapcar (lambda (s)
					   (text-width s font)) strings))
		      (* (length strings) (font-height font))))

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

  ;; Returns a list of strings describing window W in some way
  (define (window-info w)
    (list (concat (and (window-get w 'iconified) ?[)
		  (window-name w)
		  (and (window-get w 'iconified) ?]))))

;;; entry point

  ;; What must be shown?
  ;;  * The window icon at left.
  ;;  * At right, the window's title and (maybe) its class.

  (define (bhj-draw-wininfo wlist start active max-display)
    "Shows window information about those windows in wlist,
starting from START, the active window ACTIVE should be
highlighted, and there can be at most MAX-DISPLAY windows being displayed. 
This relation should hold: START <= ACTIVE < START + MAX-DISPLAY"

    ;; if there's an old window, destroy it
    (when info-window
      (x-destroy-window info-window)
      (setq info-window nil))
	
    (when wlist
      (let ((icons (make-list max-display))
            (texts (make-list max-display))
            (dealing 0)
            (wlist-save wlist)
            w
            line-height
            win-size
            )

        (while (and wlist (< dealing start))
          (setq wlist (cdr wlist)))
        
        (while (and wlist (< dealing (+ start max-display)))
          (setq w (car wlist)
                wlist (cdr wlist))

          (rplaca (nthcdr (- dealing start) texts) (make-text-item (window-info w) default-font))
          (rplaca (nthcdr (- dealing start) icons) (let ((i (window-icon-image w)))
                                                     (and i (scale-image i (car icon-size) (cdr icon-size)))))
          (format standard-error "text width for %d is %d\n" dealing (ti-width (nth (- dealing start) texts)))
          (setq dealing (1+ dealing)))

        (format standard-error "text is %s\n" (nth 0 texts))
        
        (setq 
         line-height (+ 4
                        (max (car icon-size)
                             (apply max (mapcar (lambda (t) (if (car t) 
                                                          (ti-height (car t))
                                                        0))
                                  texts)))))

        (setq
         win-size (cons (+ (car icon-size)
                           (apply max (mapcar (lambda (t) (if t
                                                              (ti-width t)
                                                            0))
                                              texts))
                           (* x-margin 3))
                        (+ (* 2 y-margin)
                           (* max-display
                              line-height))))
        (format standard-error "winsize is set to %s\n" win-size)
	(define (event-handler type xw)
	  ;; XW is the handle of the X drawable to draw in
	  (case type
	    ((expose)
	     (x-clear-window xw)

	     ;; draw the icon
             (setq dealing start)
	     (while (< dealing (+ start max-display))
               (format standard-error "dealing is now %d\n" dealing)
               (when (nth (- dealing start) icons)
                 (x-draw-image (nth (- dealing start) icons) xw
                               (cons x-margin
                                     (+ y-margin
                                        (* (- dealing start)
                                           line-height)
                                        2))))
               (setq dealing (1+ dealing)))
	       
	     ;; draw lines of text one at a time
	     (let ((gc (x-create-gc xw
				    `((foreground . ,(get-color "black"))
				      (background . ,(get-color "white")))))
		   (x (+ (* 2 x-margin) (car icon-size))))

               (let* ((rest texts)
                      (y (+ (font-ascent (ti-font (car rest)))
                            (quotient (- line-height
                                         (ti-height (car rest))) 2))))
                 (while rest
                   (x-draw-string xw gc (cons x y)
                                  (if (car rest)
                                      (car (ti-strings (car rest)))
                                    "") (ti-font (car rest)))
                   (setq rest (cdr rest)
                         y (+ y line-height))))

	       (x-destroy-gc gc))

)))

	;; create new window
	(setq info-window (x-create-window
			   (get-window-pos (car wlist-save) win-size) win-size 1
			   `((background . ,(get-color "white"))
			     (border-color . ,(get-color "black")))
			   event-handler))
	(x-map-window info-window)))))
