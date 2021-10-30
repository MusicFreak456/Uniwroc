#lang racket

(define test-list (list 6 3 2 4 1))

(define (select-min xs)
  (define (is-min? cand lis)
    (cond [(empty? lis) #t]
          [(< (car lis) cand) #f]
          [else (is-min? cand (cdr lis))]))     
  (define (select-min-aux min checked to-be-checked)
    (if (is-min? min (append checked to-be-checked))
        (append (list min)(append checked to-be-checked))
        (select-min-aux (car to-be-checked) (append checked (list min))
                        (cdr to-be-checked))))
  (select-min-aux (car xs) '() (cdr xs)))


(define (sort xs)
  (if (empty? xs) '()
  (let*
    ((iter (select-min xs))
    (min (car iter))
    (to-be-sorted (cdr iter)))
    (append (list min) (sort to-be-sorted)))))

(sort test-list)


      

