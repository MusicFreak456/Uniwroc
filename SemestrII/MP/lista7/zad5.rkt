#lang racket

; --------- ;
; Wyrazenia ;
; --------- ;

(struct const    (val)      #:transparent)
(struct binop    (op l r)   #:transparent)
(struct var-expr (id)       #:transparent)
(struct let-expr (id e1 e2) #:transparent)

(define (expr? e)
  (match e
    [(const n) (number? n)]
    [(binop op l r) (and (symbol? op) (expr? l) (expr? r))]
    [(var-expr x) (symbol? x)]
    [(let-expr x e1 e2) (and (symbol? x) (expr? e1) (expr? e2))]
    [_ false]))

(define (parse q)
  (cond
    [(number? q) (const q)]
    [(symbol? q) (var-expr q)]
    [(and (list? q) (eq? (length q) 3) (eq? (first q) 'let))
     (let-expr (first (second q))
               (parse (second (second q)))
               (parse (third q)))]
    [(and (list? q) (eq? (length q) 3) (symbol? (first q)))
     (binop (first q)
            (parse (second q))
            (parse (third q)))]))

(define (test-parse) (parse '(let [x (+ 2 2)] (+ x 1))))

; ---------- ;
; Srodowiska ;
; ---------- ;

(struct environ (xs))

(define env-empty (environ null))
(define (env-add x v env)
  (environ (cons (cons x v) (environ-xs env))))
(define (env-lookup x env)
  (define (assoc-lookup xs)
    (cond [(null? xs) (error "Unknown identifier" x)]
          [(eq? x (car (car xs))) (cdr (car xs))]
          [else (assoc-lookup (cdr xs))]))
  (assoc-lookup (environ-xs env)))

(define (rename expr)
  (define (rename-env env expr)
    (match expr
      [(const n) expr]
      [(var-expr id) (var-expr (env-lookup id env))]
      [(binop op l r) (binop op (rename-env env l) (rename-env env r))]
      [(let-expr x e1 e2) (let ([newx (gensym x)])
                            (let-expr newx e1 (rename-env (env-add x newx env) e2)))]))
  (rename-env env-empty expr))

(define test (let-expr 'x (const 3)
                       (binop
                         '+
                         (var-expr 'x)
                         (let-expr 'x (const 5) (var-expr 'x )))))
test
(rename test)

(define test2 (binop '+
                     (let-expr 'x (const 1) (var-expr 'x))
                     (let-expr 'x (const 1) (var-expr 'x))))
test2
(rename test2)

