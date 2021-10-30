#lang typed/racket

(: fold-right (All (a b) (-> (-> a b b) b (Listof a) b)))
(define (fold-right f acc xs)
  (if (null? xs)
    acc
    (f (car xs) (fold-right f acc (cdr xs)))))

(fold-right (lambda (a b) (cons a b)) null '(1 2 3))
