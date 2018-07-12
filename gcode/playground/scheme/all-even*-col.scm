(define even?
  (lambda (n)
    (= (mod n 2) 0)))

(define all-even*-col
  (lambda (list col)
    (cond
     ((null? list)
      (col '() 1))
     ((and (atom? (car list))
           (even? (car list)))
      (all-even*-col
         (cdr list)
         (lambda (new-list product)
           (col (cons (car list) new-list) (* (car list) product)))))
     ((atom? (car list))
      (all-even*-col
       (cdr list)
       (lambda (new-list product)
         (col new-list product))))
     ((list? (car list))
      (all-even*-col
       (car list)
       (lambda (al ap)
         (all-even*-col (cdr list)
                        (lambda (dl dp)
                          (col
                           (cons al dl)
                           (* ap dp))))))))))

(define col (lambda (l p) (cons p l)))
(all-even*-col '(1 2 3 (2 3 5 2) 5 2) col)
(all-even*-col '(1 2 3 5 6) col)
