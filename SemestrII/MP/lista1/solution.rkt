#lang racket

;;Napisane na podstawie kodu z wykładu udostępnionego na skosie

(provide cube-root)

(define (cube x)
  (* x x x))

(define (cube-root x)
  (define (good-enough a) (< (abs (- (cube a) x)) 0.00001))
  (define (improve y) (/ (+ (/ x (* y y)) (* 2 y)) 3))
  (define (iter a) (if (good-enough a) a (iter (improve a))))
 (iter 1.0))

;;testy
(cube-root 42)
(cube-root (cube 42))
(cube-root 8)
(cube-root 64)
(cube-root 9890)
(cube-root (cube 9890))