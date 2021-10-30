#lang racket

(require quickcheck)

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

(quickcheck
 (property ([x arbitrary-integer])
           (and (eq? (oper x neutral) x)
                (eq? (oper neutral x) x))))

(quickcheck
 (property ([x arbitrary-integer]
            [y arbitrary-integer]
            [z arbitrary-integer])
           (eq? (oper x (oper y z)) (oper (oper x y) z))))









