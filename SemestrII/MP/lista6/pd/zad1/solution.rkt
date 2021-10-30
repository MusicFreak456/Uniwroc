#lang racket

(provide (struct-out complex) parse eval)

(struct const (val)    #:transparent)
(struct binop (op l r) #:transparent)
(struct i()            #:transparent)
(struct complex (re im)#:transparent)

(define (expr? e)
  (match e
    [(const n) (number? n)]
    [(i) #t]
    [(binop op l r) (and (symbol? op) (expr? l) (expr? r))]
    [_ false]))

(define value? complex?)

(define (op->proc op)
  (match op
    ['+ (lambda (x y) (complex (+ (complex-re x) (complex-re y)) (+ (complex-im x) (complex-im y))))]
    ['- (lambda (x y) (complex (- (complex-re x) (complex-re y)) (- (complex-im x) (complex-im y))))]
    ;;Mnożenie każdy z każdym
    ['* (lambda (x y) (complex (+
                                (* (complex-re x) (complex-re y))
                                (* -1 (complex-im x) (complex-im y)))
                               (+
                                (* (complex-re x) (complex-im y))
                                (* (complex-im x) (complex-re y)))))]
    ;;Mnożenie przez sprzężenie, a następnie dzielenie przez liczbę rzeczywistą
    ['/ (lambda (x y) (let* [(conjugate (complex (complex-re y)(- (complex-im y))))
                             (counter ((op->proc '*) x conjugate))
                             (denominator ((op->proc '*) y conjugate))]
                        (complex (/ (complex-re counter) (complex-re denominator))
                                    (/ (complex-im counter) (complex-re denominator)))))]))

(define (eval e)
  (match e
    [(const n) (complex n 0)]
    [(i) (complex 0 1)]
    [(binop op l r) ((op->proc op) (eval l) (eval r))]))


(define (parse q)
  (cond [(number? q) (const q)]
        [(and (symbol? q) (eq? q 'i) (i))]
        [(and (list? q) (eq? (length q) 3) (symbol? (first q)))
         (binop (first q) (parse (second q)) (parse (third q)))]))

(define (test) (eval (parse '(+ (* 2 3) (* 4 5)))))

(define test2 (parse '(* i i)))
test2
(eval test2)
(define test3 (parse '(+ 3 (* i 8))))
test3
(eval test3)
(define test4 (parse '(* i (* i i))))
test4
(eval test4)
(define test5 (parse '(/ (+ 1 (* 8 i)) (+ 2 (* 3 i)))))
test5
(eval test5)
