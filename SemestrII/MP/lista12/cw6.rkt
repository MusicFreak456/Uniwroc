#lang racket

(require "cw5.rkt")

(define-values/invoke-unit/infer intset-ordered-list@)

(require quickcheck)

(define (intlist->intset xs)  ;niezależna od implementacji
   (if (null? xs)
       empty-set
       (union (singleton (car xs)) (intlist->intset (cdr xs)))))
(define test (intlist->intset '(3 2 1)))