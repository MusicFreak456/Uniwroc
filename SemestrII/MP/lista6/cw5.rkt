#lang racket

(struct const (val)    #:transparent)
(struct binop (op l r) #:transparent)
(struct variable ()    #:transparent)

; 2 + 2 * x
(define 2+2*x (binop '+ (const 2)
                        (binop '* (const 2)
                                  (variable))))

(define (expr? e)
  (match e
    [(variable)     true]
    [(const n)      (number? n)]
    [(binop op l r) (and (symbol? op) (expr? l) (expr? r))]
    [_              false]))

; Wartosci
(define (value? v)
  (number? v))

(define (op->proc op)
  (match op ['+ +] ['- -] ['* *] ['/ /]))

(define (eval e)
  (match e
    [(const n) n]
    [(binop op l r) ((op->proc op) (eval l) (eval r))]))

; ------------------------- ;
; Trochę składni konkretnej ;
; ------------------------- ;

(define (parse q)
  (cond [(number? q) (const q)]
        [(eq? q 'x) (variable)]
        [(and (list? q) (eq? (length q) 3) (symbol? (first q)))
         (binop (first q) (parse (second q)) (parse (third q)))]))

(define (test-parse) (parse '(+ (* 2 x) (* x x))))

(define (D function)
    (match function
    [(const n) (const 0)]
    [(variable) (const 1)]
    [(binop op l r) 
      (cond 
      [(eq? op '+) (binop op (D l) (D r))]
      [(eq? op '*) (binop '+ (binop op (D l) r) (binop op l (D r)))]
      [(and (eq? l r) (eq? op '*) (variable? l)) (binop '* (const 2) l)])]))

(pretty-print (D (parse '(+ (* 2 x) (* x x)))))

(pretty-print (D (parse '(* x (+ 2 x)))))


                                                

