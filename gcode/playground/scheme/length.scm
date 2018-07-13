(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else
      (+ 1 (length (cdr l)))))))

(length '(1 2 3 5))

(define eternity
  (lambda (x) (eternity x)))

(eternity 1)

(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
        (+ 1 (length (cdr l)))))))
  eternity)
 '(1))



(((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else
       (+ 1 (length (cdr l)))))))
  ((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else
       (+ 1 (length (cdr l)))))))
 ((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else
       (+ 1 (length (cdr l)))))))
  eternity)))
 '(1 2))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
        (+ 1 ((mk-length mk-length) (cdr l))))))))
 '(2 3 6 6 6))
