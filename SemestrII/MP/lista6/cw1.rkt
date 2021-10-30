#lang racket


(struct const (val)    #:transparent)
(struct binop (op l r) #:transparent)

; 2 + 2 * 2
(define 2+2*2 (binop '+ (const 2)
                        (binop '* (const 2)
                                  (const 2))))
;; 8/(2+3)+10+1
(define wyr1 (binop '+ (binop '/ (const 8) (binop '+ (const 2) (const 3)))
                            (binop '+ (const 10) (const 1))))
;;1+2+3*4+5
(define wyr2 (binop '+ (const 1)
                    (binop '+ (const 2)
                           (binop '+ (binop '* (const 3) (const 4))
                                  (const 5)))))


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

; ------------------------- ;
; Trochę składni konkretnej ;
; ------------------------- ;

(define (parse q)
  (cond [(number? q) (const q)]
        [(and (list? q) (eq? (length q) 3) (symbol? (first q)))
         (binop (first q) (parse (second q)) (parse (third q)))]))

(define (test) (eval (parse '(+ (* 2 3) (* 4 5)))))

(eval wyr1)
(eval wyr2)

