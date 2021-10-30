#lang typed/racket

(require "k1-sqrt.rkt")

(struct vector2 ([x : Real ] [ y : Real ]) #:transparent)
(struct vector3 ([x : Real ] [ y : Real ] [ z : Real ]) #:transparent)

(define-type Vector (U vector2 vector3))

(define-predicate vector? Vector)

(: vector-length (-> Vector Number))
(define (vector-length v)
  (match v
    [(vector2 x y) (sqrt (+ (* x x) (* y y)))]
    [(vector3 x y z) (sqrt (+ (* x x) (* y y) (* z z)))]))
