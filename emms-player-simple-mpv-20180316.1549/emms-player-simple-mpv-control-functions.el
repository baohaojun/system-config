;;; emms-player-simple-mpv-control-functions.el --- functions to control mpv via emms-player-simple-mpv.el -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 momomo5717

;; Author: momomo5717
;; URL: https://github.com/momomo5717/emms-player-simple-mpv

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by

;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides functions to control mpv via emms-player-simple-mpv.el.

;;; Code:
(require 'emms-player-simple-mpv)

;;;###autoload
(defun emms-player-simple-mpv-cycle (property)
  "Cycle PROPERTY."
  (emms-player-simple-mpv-tq-enqueue
   (list "cycle" property)
   property
   (lambda (property ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (emms-player-simple-mpv-tq-enqueue
          (list "get_property_string" property)
          nil
          (emms-player-simple-mpv-tq-data-message
           (concat "mpv " property " : %s")))
       (message "mpv %s : error" property)))))

;;;###autoload
(defun emms-player-simple-mpv-seek-to-% (per)
  "Seek to PER(percent position)."
  (interactive "nmpv seek to (%%) : ")
  (setq per (cond ((< per 0) 0) ((> per 100) 100) (t per)))
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "duration")
   per
   (lambda (per ans-ls)
     (let ((successp (emms-player-simple-mpv-tq-success-p ans-ls) )
           (data (emms-player-simple-mpv-tq-assq-v 'data ans-ls)))
       (if (and successp (numberp data) (> data 0.0))
           (let* ((total-time (emms-player-simple-mpv--time-string data))
                  (pos  (floor (* per data) 100))
                  (time (emms-player-simple-mpv--time-string pos)))
             (emms-player-simple-mpv-tq-enqueue
              (list "seek" per "absolute-percent")
              (format "mpv seek to (%%%%) : %.1f (%s / %s)" per time total-time)
              (lambda (form ans-ls)
                (if (emms-player-simple-mpv-tq-success-p ans-ls)
                    (message form)
                  (message "mpv seek to (%%) : error")))))
         (message "mpv seek to (%%) : error"))))))

;;;###autoload
(defun emms-player-simple-mpv-volume-to (v)
  "Set volume to V."
  (interactive "nmpv volume to : ")
  (emms-player-simple-mpv-set_property "volume" v))

;;;###autoload
(defun emms-player-simple-mpv-mute-on ()
  "Mute on."
  (emms-player-simple-mpv-set_property_string
   "mute" "yes" :spec "success" :msg "mute on" :err-msg "mute on"))

;;;###autoload
(defun emms-player-simple-mpv-mute-off ()
  "Mute off."
  (emms-player-simple-mpv-set_property_string
   "mute" "no" :spec "success" :msg "mute off" :err-msg "mute off"))

;;;###autoload
(defun emms-player-simple-mpv-mute ()
  "Cycle mute."
  (interactive)
  (emms-player-simple-mpv-cycle "mute"))

;;;###autoload
(defun emms-player-simple-mpv-time-pos ()
  "Display position in current file."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "time-pos")
   nil
   (emms-player-simple-mpv-tq-data-message
    "mpv time position : %s" :err-form "mpv time position : error"
    :fn #'emms-player-simple-mpv--time-string)))

(defun emms-player-simple-mpv-time-pos-%-1 (form duration)
  "Helper function for `emms-player-simple-mpv-time-pos-%'."
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "percent-pos")
   nil
   (emms-player-simple-mpv-tq-data-message
    "%s" :err-form "mpv time position (%%) : error"
    :fn (lambda (data)
          (format form data (emms-player-simple-mpv--time-string (/ (* data duration) 100.0)))))))

;;;###autoload
(defun emms-player-simple-mpv-time-pos-% ()
  "Display position (0-100) in current file."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "duration")
   nil
   (lambda (_ ans-ls)
     (let ((successp (emms-player-simple-mpv-tq-success-p ans-ls))
           (data (emms-player-simple-mpv-tq-assq-v 'data ans-ls)))
       (if (and successp (numberp data) (> data 0.0))
           (let* ((form "mpv time position (%%%%) : %%.1f (%%s / %s)")
                  (form (format form (emms-player-simple-mpv--time-string data))))
             (emms-player-simple-mpv-time-pos-%-1 form data))
         (message "mpv time position (%%) : error"))))))

(defmacro emms-player-simple-mpv--playlist-change-1 (str)
  "Helper macro for emms-player-simple-mpv--playlist-next/prev."
  (let ((n (if (string= str "next") 1  -1)))
    `(emms-player-simple-mpv-tq-enqueue
      '("get_property" "playlist-pos") nil
      (lambda (_ ans-ls)
        (if (emms-player-simple-mpv-tq-success-p ans-ls)
            (let* ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
                   (form (format "mpv playlist_%s position %s : %%s"
                                 ,str (+ data ,n))))
              (emms-player-simple-mpv-tq-enqueue
               '(,(format "playlist_%s" str)) nil
               (emms-player-simple-mpv-tq-error-message form)))
          (message ,(format "mpv playlist_%s : error" str)))))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-next ()
  "Go to the next entry on the playlist."
  (interactive)
  (emms-player-simple-mpv--playlist-change-1 "next"))

;;;###autoload
(defun emms-player-simple-mpv-playlist-prev ()
  "Go to the previous entry on the playlist."
  (interactive)
  (emms-player-simple-mpv--playlist-change-1 "prev"))

(defun emms-player-simple-mpv--playlist-to-1 (n)
  "Helper function for `emms-player-simple-mpv-playlist-to'.
Set playlist-pos to N."
  (emms-player-simple-mpv-set_property
   "playlist-pos" n :msg "playlist position" :err-msg "playlist to"))

(defun emms-player-simple-mpv--playlist-to-2 ()
  "Helper function for `emms-player-simple-mpv-playlist-to'."
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "playlist-count")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (let* ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
                (n (read-number
                    (format "mpv playlist to (0 - %s) : " (1- data)))))
          (emms-player-simple-mpv--playlist-to-1 n))
       (message "mpv playlist to : error")))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-to (&optional n)
  "Go to the Nth entry on the playlist."
  (interactive)
  (if (called-interactively-p 'any)
      (emms-player-simple-mpv--playlist-to-2)
    (emms-player-simple-mpv--playlist-to-1 n)))

;;;###autoload
(defun emms-player-simple-mpv-playlist-pos ()
  "Display current position on the playlist."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "playlist-count")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (emms-player-simple-mpv-tq-enqueue
          '("get_property" "playlist-pos")
          nil
          (emms-player-simple-mpv-tq-data-message
           (format "mpv playlist position : %%s (total %s)"
                   (emms-player-simple-mpv-tq-assq-v 'data ans-ls))))
       (message "mpv playlist position : error")))))

(defun emms-player-simple-mpv--with-playlist-pos (fn)
  "Run FN with playlist-pos data."
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "playlist-pos")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (funcall fn (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
       (message "mpv playlist-pos : %s"
                (emms-player-simple-mpv-tq-assq-v 'error ans-ls))))))

(defun emms-player-simple-mpv--playlist (&optional p1 p2 fn)
  "Display playlist/P1/P2.
Run FN with data, if non-nil."
  (let ((com (mapconcat (lambda (p) (format "%s" p))
                        (delete nil (list "playlist" p1 p2))
                        "/")))
    (emms-player-simple-mpv-tq-enqueue
     `("get_property" ,com)
     com
     (lambda (com ans-ls)
       (if (emms-player-simple-mpv-tq-success-p ans-ls)
           (if fn (funcall fn (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
             (message "mpv %s : %s" com (emms-player-simple-mpv-tq-assq-v 'data ans-ls)))
         (message "mpv %s : %s" com (emms-player-simple-mpv-tq-assq-v 'error ans-ls)))))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-nth-title (n)
  "Display title of N th entry.
N is 0-base"
  (interactive "nInput playlist position: ")
  (emms-player-simple-mpv--playlist n "title"))

;;;###autoload
(defun emms-player-simple-mpv-playlist-current-title ()
  "Display title of the current entry."
  (interactive)
  (emms-player-simple-mpv--with-playlist-pos
   (lambda (n) (emms-player-simple-mpv--playlist n "title"))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-nth-filename (n)
  "Display filename of N th entry.
N is 0-base."
  (interactive "nInput playlist position: ")
  (emms-player-simple-mpv--playlist n "filename"))

;;;###autoload
(defun emms-player-simple-mpv-playlist-current-filename ()
  "Display title current entry."
  (interactive)
  (emms-player-simple-mpv--with-playlist-pos
   (lambda (n) (emms-player-simple-mpv--playlist n "filename"))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-move (index1 index2)
  "Run playlist-move INDEX1 INDEX2."
  (interactive "nInput index1: \nnInput index2: ")
  (emms-player-simple-mpv-tq-enqueue
   `("playlist-move" ,index1 ,index2) nil
   (emms-player-simple-mpv-tq-error-message
    (format "mpv playlist-move: %s %s : %%s" index1 index2))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-shuffle ()
  "Run playlist-shuffle."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("playlist-shuffle") nil
   (emms-player-simple-mpv-tq-error-message "mpv playlist-shuffle: %s")))

;;;###autoload
(defun emms-player-simple-mpv-playlist-remove-current ()
  "Run playlist-remove current."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("playlist-remove" "current") nil
   (emms-player-simple-mpv-tq-error-message "mpv playlist-remove current: %s")))

;;;###autoload
(defun emms-player-simple-mpv-playlist-remove-index (index)
  "Run playlist-remove INDEX."
  (interactive "nInput index: ")
  (emms-player-simple-mpv-tq-enqueue
   `("playlist-remove" ,index) nil
   (emms-player-simple-mpv-tq-error-message
    (format "mpv playlist-remove %s : %%s" index))))

;;;###autoload
(defun emms-player-simple-mpv-speed-to (v)
  "Set speed to V."
  (interactive "nmpv speed to (0.01 - 100): ")
  (setq v (cond ((< v 0.01) 0.01)
                ((> v 100) 100)
                (t v)))
  (emms-player-simple-mpv-set_property "speed" v :spec "%.2f"))

;;;###autoload
(defun emms-player-simple-mpv-speed-normal ()
  "Change speed to normal."
  (interactive)
  (emms-player-simple-mpv-speed-to 1))

(defun emms-player-simple-mpv--speed-1 (v ans-ls)
  "Helper function for `emms-player-simple-mpv-speed'."
  (if (emms-player-simple-mpv-tq-success-p ans-ls)
      (let* ((speed (+ (emms-player-simple-mpv-tq-assq-v 'data ans-ls) v)))
        (emms-player-simple-mpv-speed-to speed))
    (message "mpv speed : error")))

;;;###autoload
(defun emms-player-simple-mpv-speed (v)
  "Change speed by V."
  (interactive "nmpv speed : ")
  (emms-player-simple-mpv-tq-enqueue
   (list "get_property" "speed")
   v 'emms-player-simple-mpv--speed-1))

(defun emms-player-simple-mpv--speed-n% (n ans-ls)
  "Helper function for `emms-player-simple-mpv-speed-%'."
  (if (emms-player-simple-mpv-tq-success-p ans-ls)
      (let* ((speed (/ (* (emms-player-simple-mpv-tq-assq-v 'data ans-ls) n)
                       100.0)))
        (emms-player-simple-mpv-speed-to speed))
    (message "mpv speed : error")))

;;;###autoload
(defun emms-player-simple-mpv-speed-% (n)
  "N % times speed."
  (emms-player-simple-mpv-tq-enqueue
   (list "get_property" "speed")
   n 'emms-player-simple-mpv--speed-n%))

;;;###autoload
(defun emms-player-simple-mpv-speed-increase ()
  "Increase speed by 10%."
  (interactive)
  (emms-player-simple-mpv-speed-% 110))

;;;###autoload
(defun emms-player-simple-mpv-speed-decrease ()
  "Decrease speed by 10%."
  (interactive)
  (emms-player-simple-mpv-speed-% 90))

;;;###autoload
(defun emms-player-simple-mpv-speed-double ()
  "Double speed."
  (interactive)
  (emms-player-simple-mpv-speed-% 200))

;;;###autoload
(defun emms-player-simple-mpv-speed-halve ()
  "Halve speed."
  (interactive)
  (emms-player-simple-mpv-speed-% 50))

(defun emms-player-simple-mpv--ab-loop-1 ()
  "Helper function for `emms-player-simple-mpv-ab-loop'."
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "ab-loop-a")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (emms-player-simple-mpv-tq-enqueue
          '("get_property" "ab-loop-b")
          (emms-player-simple-mpv-tq-assq-v 'data ans-ls)
          #'emms-player-simple-mpv-ab-loop-2)
       (message "mpv ab-loop : success")))))

(defun emms-player-simple-mpv-ab-loop-2 (loop-a ans-ls)
  "Helper function for `emms-player-simple-mpv--ab-loop-1'."
  (let ((loop-b (emms-player-simple-mpv-tq-assq-v 'data ans-ls)))
    (cond ((numberp loop-b)
           (message "mpv ab-loop : point B %s"
                    (emms-player-simple-mpv--time-string loop-b)))
          ((numberp loop-a)
           (message "mpv ab-loop : point A %s"
                    (emms-player-simple-mpv--time-string loop-a)))
          ((equal loop-a "no") (message "mpv ab-loop : clear"))
          (t (message "mpv ab-loop : success")))))

;;;###autoload
(defun emms-player-simple-mpv-ab-loop ()
  "Cycle ab-loop."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("ab_loop") nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (emms-player-simple-mpv--ab-loop-1)
       (message "mpv ab-loop : error")))))

;;;###autoload
(defun emms-player-simple-mpv-loop-to (n)
  "Set loop to N.
If N is less than 1, set loop to \"inf\"."
  (interactive "nmpv loop to : ")
  (emms-player-simple-mpv-set_property
   "loop" (cond ((< n 1) "inf")
                ((= n 1) "no")
                (t n))
   :fn (lambda (v) (if (numberp v) (format "%s times" v) v))))

;;;###autoload
(defun emms-player-simple-mpv-loop-file-to (n)
  "Set loop-file to N.
If N is less than 0, set loop-file to \"inf\"."
  (interactive "nmpv loop-file to : ")
  (emms-player-simple-mpv-set_property
   "loop-file" (if (< n 0) "inf" n)
   :fn (lambda (v) (if (numberp v) (format "%s times" v) v))))

;;;###autoload
(defun emms-player-simple-mpv-ontop ()
  "Cycle ontop."
  (interactive)
  (emms-player-simple-mpv-cycle "ontop"))

;;;###autoload
(defun emms-player-simple-mpv-fullscreen ()
  "Cycle fullscreen."
  (interactive)
  (emms-player-simple-mpv-cycle "fullscreen"))

(defun emms-player-simple-mpv--metadata-1 (name)
  "Helper function for `emms-player-simple-mpv-metadata'.
Display NAME in minibuffer."
  (emms-player-simple-mpv-tq-enqueue
   `("get_property" ,name) nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (let ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
               (str (list (format "mpv %s:\n" name))))
           (cl-loop for (n . v) in (nreverse data) do
                    (push (format "\n    %s: %s" n v) str)
                    finally do (push "\n" str))
           (let ((max-mini-window-height 0.9)
                 (minibuffer-message-timeout nil)
                 (resize-mini-windows 'grow-only))
             (minibuffer-message (apply #'concat (nreverse str)))))
       (message "mpv %s : error" name)))))

;;;###autoload
(defun emms-player-simple-mpv-metadata ()
  "Display metadata."
  (interactive)
  (emms-player-simple-mpv--metadata-1 "metadata"))

;;;###autoload
(defun emms-player-simple-mpv-filtered-metadata ()
  "Display filtered-metadata."
  (interactive)
  (emms-player-simple-mpv--metadata-1 "filtered-metadata"))

(defvar emms-player-simple-mpv--property-list nil
  "List of property names.")

(defvar emms-player-simple-mpv--property-list-history nil
  "`minibuffer-history' for `emms-player-simple-mpv-display-property'.")

(defun emms-player-simple-mpv--property-list-1 ()
  "Helper function for `emms-player-simple-mpv-property-list'."
  (let ((property
         (completing-read "mpv property: "
                          emms-player-simple-mpv--property-list
                          nil nil nil
                          'emms-player-simple-mpv--property-list-history)))
    (when property
      (emms-player-simple-mpv-tq-enqueue
       `("get_property_string" ,property) nil
       (lambda (_ ans-ls)
         (if (and (listp ans-ls)
                  (emms-player-simple-mpv-tq-success-p ans-ls))
             (let ((data (cdr (assq 'data ans-ls))))
               (message "mpv %s : %s" property data))
           (message "mpv %s : %s" property
                    (if (and (listp ans-ls) (assq 'error ans-ls))
                        "error"
                      "Failed to get data"))))))))

;;;###autoload
(defun emms-player-simple-mpv-property-list ()
  "Display the current value of a property via get_property_string."
  (interactive)
  (if emms-player-simple-mpv--property-list
      (emms-player-simple-mpv--property-list-1)
    (emms-player-simple-mpv-tq-enqueue
     '("get_property" "property-list") nil
     (lambda (_ ans-ls)
       (if (emms-player-simple-mpv-tq-success-p ans-ls)
           (let ((data (cdr (assq 'data ans-ls))))
             (setq emms-player-simple-mpv--property-list
                   (sort (append data nil) #'string<))
             (run-with-idle-timer
              0 nil
              (lambda ()
                (let ((this-command 'emms-player-simple-mpv-property-list))
                  (emms-player-simple-mpv--property-list-1)))))
         (message "mpv property-list : error"))))))

(provide 'emms-player-simple-mpv-control-functions)
;;; emms-player-simple-mpv-control-functions.el ends here
