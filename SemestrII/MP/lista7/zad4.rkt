#lang racket

(struct variable (v)         #:transparent)
(struct neg  (subf)     #:transparent)
(struct disj (l r)      #:transparent)
(struct conj (l r)      #:transparent)
(struct exists (x f)    #:transparent)
(struct forall (x f)    #:transparent)

(define (formula? f)
 (match f
   [(variable v) (symbol? v)]
   [(neg subf) (formula? subf)]
   [(conj l r) (and (formula? l) (formula? r))]
   [(disj l r) (and (formula? l) (formula? r))]
   [(exists x sub) (and (variable? x) (formula? sub))]
   [(forall x sub) (and (variable? x) (formula? sub))]
   [_ false]))

(define (parse f)
  (cond
    [(symbol? f) (variable f)]
    [(and (list? f) (= (length f) 2) (eq? (first f) 'not))
     (neg (parse (second f)))]
    [(and (list? f) (= (length f) 3) (eq? (first f) 'or))
     (disj (parse (second f)) (parse (third f)))]
    [(and (list? f) (= (length f) 3) (eq? (first f) 'and))
     (conj (parse (second f)) (parse (third f)))]
    [(and (list? f) (= (length f) 3) (eq? (first f) 'exists))
     (exists (parse (second f)) (parse (third f)))]
    [(and (list? f) (= (length f) 3) (eq? (first f) 'forall))
     (forall (parse (second f)) (parse (third f)))]))

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

(define (eval-formula f)
  (define (eval-env env f)
    (match f
      [(variable v) (env-lookup v env)]
      [(neg subf) (not (eval-env env subf))]
      [(conj l r) (and (eval-env env l) (eval-env env r))]
      [(disj l r) (or (eval-env env l) (eval-env env r))]
      [(exists x f) (or (eval-env (env-add (variable-v x) #t env) f) (eval-env (env-add (variable-v x) #f env) f))]
      [(forall x f) (and (eval-env (env-add  (variable-v x) #t env) f) (eval-env (env-add  (variable-v x) #f env) f))]))
  (eval-env env-empty f))

(define test (parse '(exists x (not x))))
test
(eval-formula test)

(define test2 (parse '(forall x (or x (not x)))))
test2
(eval-formula test2)

(define test3 (parse '(exists x (and x (not x)))))
test3
(eval-formula test3)










      

   