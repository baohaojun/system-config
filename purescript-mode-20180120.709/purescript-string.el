;;;###autoload
(defun purescript-trim (string)
  (replace-regexp-in-string
   "^[ \t\n]+" ""
   (replace-regexp-in-string
    "[ \t\n]+$" ""
    string)))

;;;###autoload
(defun purescript-string-take (string n)
  "Take n chars from string."
  (substring string
             0
             (min (length string) n)))

;;;###autoload
(defun purescript-is-prefix-of (x y)
  "Is x string a prefix of y string?"
  (string= x (substring y 0 (min (length y) (length x)))))

(defun purescript-string ())

(provide 'purescript-string)
