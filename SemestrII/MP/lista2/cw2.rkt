#lang racket

(define (squere x) (* x x))

(define (inc x) (+ x 1))

(define (compose f g) (lambda (x) (f (g x))))

((compose squere inc) 5)

((compose inc squere) 5)