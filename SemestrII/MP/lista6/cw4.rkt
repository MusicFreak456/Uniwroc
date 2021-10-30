#lang racket

(struct const (val)    #:transparent)
(struct binop (op l r) #:transparent)
(struct abso (subexp)   #:transparent)

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
    [(abso subexp) (expr? subexp)]
    [_ false]))

; Co to są wartości?
(define (value? v)
  (number? v))

(define (op->proc op)
  (match op ['+ +] ['- -] ['* *] ['/ /] ['^ expt]))

(define (eval e)
  (match e
    [(const n) n]
    [(binop op l r) ((op->proc op) (eval l) (eval r))]
    [(abso subexp) (let ([x (eval subexp)])
                    (if (< x 0) (- x) x))]))

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
(define wyr3 (binop '^ (const 2) (const 2)))
(eval wyr3)

(define wyr4 (binop '- (const 1) (const 3)))
(define wyr5 (abso wyr4))
(expr? wyr4)
(eval wyr4)
(eval wyr5)

(define (pretty-print e)
  (match e
    [(const n) (number->string n)]
    [(binop op l r) (string-append "("  (pretty-print l) (symbol->string op) (pretty-print r) ")")]
    [(abso subexp) (string-append "|" (pretty-print subexp) "|")]))

(pretty-print wyr1)


