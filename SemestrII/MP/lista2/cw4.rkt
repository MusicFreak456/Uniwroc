#lang racket

(define (sum val next start end)(if (> start end) 0 (+ (val start) (sum val next (next start) end))))

(define (produkt-rek val next start end) (if (> start end) 1 (* (val start) (produkt-rek val next (next start) end))))

(define (produkt-iter val next res start end) (if (> start end) res (produkt-iter val next (* res (val start)) (next start) end)))

(define (produkt-it val next start end) (produkt-iter val next (val start) (next start) end))












(produkt-rek (lambda (x) (/ 1 x)) (lambda (x) (+ 1 x)) 1 3)

(produkt-it (lambda (x) (/ 1 x)) (lambda (x) (+ 1 x)) 1 3)







(* 4 (produkt-it (lambda (x) (if (even? x) (/ x (+ x 1)) (/ (+ x 1) x))) (lambda (x) (+ x 1)) 2.0 200))