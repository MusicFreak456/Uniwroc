#lang racket

(require quickcheck)

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


(quickcheck
 (property ([x (arbitrary-list arbitrary-integer)])
           (and (equal? (oper x neutral) x)
                (equal? (oper neutral x) x))))

(quickcheck
 (property ([x (arbitrary-list arbitrary-integer)]
            [y (arbitrary-list arbitrary-integer)]
            [z (arbitrary-list arbitrary-integer)])
           (equal? (oper x (oper y z)) (oper (oper x y) z))))