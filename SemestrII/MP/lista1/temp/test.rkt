#lang racket

(define (power-close-to b n)
  (define(is-good guess)(> (expt b guess) n))
  (define(iter guess)(if (is-good guess) guess (iter (+ guess 1))))
  (iter 1))