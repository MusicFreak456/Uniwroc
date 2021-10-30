#lang racket

(define-signature monoid^
  ((contracted
     [elem? (-> any/c boolean?)]
     [neutral elem?]
     [oper (-> elem? elem? elem?)])))

(define-unit int-unit@
  (import)
  (export monoid^)

  (define elem? number?)
  (define neutral 0)
  (define (oper x y) (+ x y)))


(define-values/invoke-unit/infer int-unit@)

(elem? 2)
neutral
(oper 4 2)




