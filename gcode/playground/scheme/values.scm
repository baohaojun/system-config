(define first
  (lambda (l)
    (car l)))

(define second
  (lambda (l)
    (car (cdr l))))

(define lookup-entries
  (lambda (name entry entry-f)
    (lookup-entries-help
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-entries-help
  (lambda (name names values entry-f)
    (cond
     ((null? names)
      (entry-f name))
     ((eq? name (car names))
      (car values))
     (else
      (lookup-entries-help
       name
       (cdr names)
       (cdr values)
       entry-f)))))

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table)
      (table-f name))
     (else
      (lookup-entries
       name
       (car table)
       (lambda (name)
         (lookup-in-table name (cdr table) table-f)))))))



(lookup-entries 'hello '((hello world) (fuck you)) 'nil)

(lookup-in-table 'world
                 '(
                   ((hello worldx) (fuck you))
                   ((hello world) (fuck again))) 'nil)
