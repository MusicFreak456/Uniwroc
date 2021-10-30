#lang racket

(require racket/contract)

(define/contract (fold-right f acc xs)
  (parametric->/c [a b] (-> (-> a b b) b (listof a) b))
  (if (null? xs)
    acc
    (f (car xs) (fold-right f acc (cdr xs)))))

(fold-right (lambda (a b) (cons a b)) null '(1 2 3))
