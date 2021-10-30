#lang racket

(provide (struct-out const) (struct-out binop) rpn->arith)

;; -------------------------------
;; Wyrazenia w odwr. not. polskiej
;; -------------------------------

(define (member? x list)
  (not (equal? (member x list) #f)))

(define (rpn-expr? e)
  (and (list? e)
       (pair? e)
       (andmap (lambda (x) (or (number? x) (member x '(+ - * /))))
               e)))

;; ----------------------
;; Wyrazenia arytmetyczne
;; ----------------------

(struct const (val)    #:transparent)
(struct binop (op l r) #:transparent)

(define (arith-expr? e)
  (match e
    [(const n) (number? n)]
    [(binop op l r)
     (and (symbol? op) (arith-expr? l) (arith-expr? r))]
    [_ false]))

;; ----------
;; Kompilacja
;; ----------

(define (rpn->arith e)
  (define (rpn->arith expr stack)
    (cond
      [(and (empty? expr) (= (length stack) 1)) (car stack)]
      [(empty? expr) (displayln "Not a valid expression")]
      [else (if (number? (car expr))
                (rpn->arith (cdr expr) (cons (const (car expr)) stack))
                (let[(newop (binop (first expr) (second stack) (first stack)))
                     (newstack (cddr stack))]
                  (rpn->arith (cdr expr) (cons newop newstack))))]))
  (rpn->arith e null))

; Mozesz tez dodac jakies procedury pomocnicze i testy
(define test '(6 7 * 4 +))
(rpn->arith test)
(define test2 '(3 3 3 *))
(rpn->arith test2)
(define test3 '(1 2 3 4 + + +))
(rpn->arith test3)
(define test4 '(3 4 - 5 6 + *))
(rpn->arith test4)
 

