#lang racket

(define (insert xs n)
  (cond
    [(empty? xs) (list n)]
    [(> (car xs) n) (append (list n) xs)]
    [else  (append (list (car xs)) (insert (cdr xs) n))]))

(define (sort list)
  (define (sort-aux sorted to-be-sorted)
    (if (empty? to-be-sorted) sorted
        (sort-aux (insert sorted (car to-be-sorted)) (cdr to-be-sorted))))
  (sort-aux '() list))

(define sort-test (list 3 2 7 8))

(sort sort-test)