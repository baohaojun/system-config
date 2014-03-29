(bind-keys global-keymap "F1" '(synthesize-multiple-events "ESC" "g" "f" "RET"))
(bind-keys global-keymap "F2" '(synthesize-multiple-events "ESC" "g" "r" "RET"))
(bind-keys global-keymap "F3" '(synthesize-multiple-events "ESC" "." "RET"))
(bind-keys global-keymap "Left" '(synthesize-multiple-events "ESC" "C-,"))
(bind-keys global-keymap "Right" '(synthesize-multiple-events "ESC" "C-."))
(bind-keys global-keymap "Up" '(synthesize-multiple-events "ESC" "C-a"))
(bind-keys global-keymap "Down" '(synthesize-multiple-events "ESC" "C-e"))
(bind-keys global-keymap "F11"  '(progn
                                   (maximize-window (input-focus))
                                   (synthesize-multiple-events "C-x" "1")))
(bind-keys global-keymap "Prior" '(synthesize-multiple-events "ESC" "g" "p"))
(bind-keys global-keymap "Next" '(synthesize-multiple-events "ESC" "g" "n"))

(bind-keys global-keymap "F4"
           '(progn
              (read-event
 "
                ^



<     Click on the screen     >



                V" )
              (let* ((xy (query-pointer))
                     (x (car xy))
                     (y (cdr xy))
                     (w (screen-width))
                     (h (screen-height)))
                (message (format nil "x is %d, y is %d, w is %d, h is %d" x y w h))
                (cond
                 ((< x (/ w 3)) ; left
                  (message "going left")
                  (synthesize-multiple-events "ESC" "C-,"))
                 ((> x (- w (/ w 3))) ; right
                  (message "going right")
                  (synthesize-multiple-events "ESC" "C-."))
                 ((< y (/ h 3))
                  (message "going up")
                  (synthesize-multiple-events "ESC" "C-a"))
                 ((> y (- h (/ h 3)))
                  (message "going down")
                  (synthesize-multiple-events "ESC" "C-e"))))))
