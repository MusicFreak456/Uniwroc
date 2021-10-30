#lang racket

(struct const (val)    #:transparent)
(struct binop (op l r) #:transparent)

; Co to są wyrażenia?
(define (expr? e)
  (match e
    [(const n) (number? n)]
    [(binop op l r) (and (symbol? op) (expr? l) (expr? r))]
    [_ false]))

; Co to są wartości?
(define (value? v)
  (number? v))

(define (op->proc op)
  (match op ['+ +] ['- -] ['* *] ['/ /]))

(define (eval e)
  (match e
    [(const n) n]
    [(binop op l r) ((op->proc op) (eval l) (eval r))]))


(define (cont-frac-iter iter num den k)  (if (= k iter) (const 0) (binop '/ (const (num iter)) (binop '+ (const (den iter)) (cont-frac-iter (+ iter 1) num den k)))))

(define (cont-frac num den k) (cont-frac-iter 1 num den k))

(define (num x) (+ 1 (* 2 (- x 1))))

(define pi (binop '+ (const 3) (cont-frac (lambda (x) (sqr (num x))) (lambda (x) 6.0) 3)))

pi
(eval pi)




