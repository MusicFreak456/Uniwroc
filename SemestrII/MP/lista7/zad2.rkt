#lang racket

; --------- ;
; Wyrazenia ;
; --------- ;

(struct const    (val)        #:transparent)
(struct binop    (op l r)     #:transparent)
(struct var-expr (id)         #:transparent)
(struct let-expr (id e1 e2)   #:transparent)
(struct big-sum  (i n m f)    #:transparent)
(struct de-integral (a b f x) #:transparent)
(struct min-f-set (f)         #:transparent)


(define (expr? e)
  (match e
    [(const n) (number? n)]
    [(binop op l r) (and (symbol? op) (expr? l) (expr? r))]
    [(var-expr x) (symbol? x)]
    [(let-expr x e1 e2) (and (symbol? x) (expr? e1) (expr? e2))]
    [(big-sum i n m f) (and (var-expr? i) (const? n) (const? m) (expr? f))]
    [(de-integral a b f x) (and (const? a) (const? b) (expr? f) (var-expr? x))]
    [(min-f-set f) (expr? f)]
    [_ false]))

(define (parse q)
  (cond
    [(number? q) (const q)]
    [(symbol? q) (var-expr q)]
    [(and (list? q) (= (length q) 5) (eq? (first q) 'big-sum))
     (big-sum (var-expr (second q)) (const (third q)) (const (fourth q)) (parse (fifth q)))]
    [(and (list? q) (= (length q) 5) (eq? (first q) 'de-integral))
     (de-integral (const (second q)) (const (third q)) (parse (fourth q)) (var-expr (fifth q)))]
    [(and (list? q) (= (length q) 2) (eq? (first q) 'min-f-set))
     (min-f-set (parse (second q)))]
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

; --------- ;
; Ewaluacja ;
; --------- ;

(define (value? v)
  (number? v))

(define (op->proc op)
  (match op ['+ +] ['- -] ['* *] ['/ /] ['expt expt]))

(define (eval-env e env)
  (match e
    [(const n) n]
    [(binop op l r) ((op->proc op) (eval-env l env)
                                   (eval-env r env))]
    [(let-expr x e1 e2)
     (eval-env e2 (env-add x (eval-env e1 env) env))]
    [(var-expr x) (env-lookup x env)]))

(define (eval e) (eval-env e env-empty))

(define (test-eval) (eval (parse '(let [x 5] (let [y (let [z (+ 5 x)] z)] (+ y x))))))

(define test (parse '(expt 2 4)))
test
(eval test)

(define test2 (parse '(big-sum i 1 2 (+ 1 i))))
test2
(expr? test2)

(define test3 (parse '(de-integral 1 2 (expt x 2) x)))
test3
(expr? test3)

(define test4 (parse '(min-f-set (+ 1 x))))
test4

(expr? (const 5))