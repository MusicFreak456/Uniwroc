#lang racket

(define-signature monoid^
  ((contracted
     [elem? (-> any/c boolean?)]
     [neutral elem?]
     [oper (-> elem? elem? elem?)])))

(define-unit list-unit@
  (import)
  (export monoid^)

  (define elem? list?)
  (define neutral '())
  (define (oper xs ys) (append xs ys)))


(define-values/invoke-unit/infer list-unit@)

(elem? (list 1 2 3))
neutral
(oper (list 1 2 3) neutral)

