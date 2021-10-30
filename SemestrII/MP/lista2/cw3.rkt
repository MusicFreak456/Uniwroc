#lang racket





(define (repeated p n)
  (define (compose f g) (lambda (x) (f (g x))))
  (define (identity x) x )
  (if (= n 0) identity (compose p (repeated p (- n 1)))))

((repeated sqr 1) 2)