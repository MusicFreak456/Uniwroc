#lang racket

(define (cont-frac-rec-iter iter num den k)  (if (< k iter) 0 (/ (num iter) (+ (den iter) (cont-frac-rec-iter (+ iter 1) num den k)))))

(define (cont-frac num den k) (cont-frac-rec-iter 1 num den k))

(define (cont-frac-iter-iter res num den k) (if (= k 0) res (cont-frac-iter-iter (/ (num k) (+ (den k) res)) num den (- k 1))))

;;(define (cont-frac num den k) (cont-frac-iter-iter 0 num den k))

(define (num x) (sqr ( + (* 2 (- x 1) ) 1)))

(define (den x) 6.0)

(+ 3 (cont-frac num den 33))