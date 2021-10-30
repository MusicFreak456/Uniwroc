#lang racket

(define (cont-frac-rec-iter iter num den k)  (if (< k iter) 0 (/ (num iter) (+ (den iter) (cont-frac-rec-iter (+ iter 1) num den k)))))

(define (cont-frac num den k) (cont-frac-rec-iter 1 num den k))

(define (cont-frac-iter-iter res num den k) (if (= k 0) res (cont-frac-iter-iter (/ (num k) (+ (den k) res)) num den (- k 1))))

;;(define (cont-frac num den k) (cont-frac-iter-iter 0 num den k))

(define (den i) ( + (* (- i 1) 2) 1))

(define (atan-cf x k) (cont-frac (lambda (i) (if (= i 1) x (sqr (* (- i 1.0)x)))) den 10.0))  