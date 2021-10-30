#lang racket

(provide (struct-out const) (struct-out binop) (struct-out var-expr) (struct-out let-expr) (struct-out pos) (struct-out var-free) (struct-out var-bound) annotate-expression)

;; ---------------
;; Jezyk wejsciowy
;; ---------------

(struct pos (file line col)     #:transparent)
  
(struct const    (val)          #:transparent)
(struct binop    (op l r)       #:transparent)
(struct var-expr (id)           #:transparent)
(struct let-expr (loc id e1 e2) #:transparent)

(define (expr? e)
  (match e
    [(const n)      (number? n)]
    [(binop op l r) (and (symbol? op) (expr? l) (expr? r))]
    [(var-expr x)   (symbol? x)]
    [(let-expr loc x e1 e2)
     (and (pos? loc) (symbol? x) (expr? e1) (expr? e2))]
    [_ false]))

(define (make-pos s)
  (pos (syntax-source s)
       (syntax-line   s)
       (syntax-column s)))

(define (parse e)
  (let ([r (syntax-e e)])
    (cond
      [(number? r) (const r)]
      [(symbol? r) (var-expr r)]
      [(and (list? r) (= 3 (length r)))
       (match (syntax-e (car r))
         ['let (let* ([e-def (syntax-e (second r))]
                      [x     (syntax-e (first e-def))])
                 (let-expr (make-pos (first e-def))
                           (if (symbol? x) x (error "parse error!"))
                           (parse (second e-def))
                           (parse (third r))))]
         [op   (binop op (parse (second r)) (parse (third r)))])]
      [else (error "parse error!")])))

;;----------------
;; Środowisko
;;----------------

(struct environment (xs))

(define empty-environment (environment null))

(define (add-to-environment x value env)
  (environment (cons (cons x value) (environment-xs env))))

(define (look-up-env x env)
  (define (get-value xs)
    (cond [(null? xs) xs]
          [(eq? x (car (car xs))) (cdr (car xs))]
          [else (get-value (cdr xs))]))
  (get-value (environment-xs env)))


;; ---------------
;; Jezyk wyjsciowy
;; ---------------

(struct var-free  (id)     #:transparent)
(struct var-bound (pos id) #:transparent)

(define (expr-annot? e)
  (match e
    [(const n)         (number? n)]
    [(binop op l r)    (and (symbol? op) (expr-annot? l) (expr-annot? r))]
    [(var-free x)      (symbol? x)]
    [(var-bound loc x) (and (pos? loc) (symbol? x))]
    [(let-expr loc x e1 e2)
     (and (pos? loc) (symbol? x) (expr-annot? e1) (expr-annot? e2))]
    [_ false]))

(define (annotate-expression e)
  (define (annotate-exp-env e env)
    (match e
      [(const n) e]
      [(var-expr id) (let ([position (look-up-env id env)])
                       (if (null? position) (var-free id)
                           (var-bound position id)))]
      [(binop op l r) (binop op (annotate-exp-env l env) (annotate-exp-env r env))]
      [(let-expr loc x e1 e2) (let-expr
                               loc
                               x
                               (annotate-exp-env e1 env)
                               (annotate-exp-env e2 (add-to-environment x loc env)))]))
  (annotate-exp-env e empty-environment))

(define test (parse #'(let [x 5] x)))
test
(newline)
(annotate-expression test)
(newline)
(newline)

(define test2 (parse #'(let [x 5] y)))
test2
(newline)
(annotate-expression test2)
(newline)
(newline)

(define test3 (parse #'(let [x 5] (* y x))))
test3
(newline)
(annotate-expression test3)

