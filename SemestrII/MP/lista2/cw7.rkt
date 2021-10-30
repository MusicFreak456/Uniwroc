#lang racket

(define (cont-frac-iter iter num den k)  (if (= k iter) 0 (/ (num iter) (+ (den iter) (cont-frac-iter (+ iter 1) num den k)))))

(define (cont-frac num den k) (cont-frac-iter 1 num den k))

(define (num x) (+ 1 (* 2 (- x 1))))

(define pi (+ 3 (cont-frac (lambda (x) (sqr (num x))) (lambda (x) 6.0) 10)))

pi




