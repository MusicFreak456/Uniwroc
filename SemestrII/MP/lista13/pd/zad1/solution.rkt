#lang typed/racket

(provide parse typecheck)

(struct const    ([val : (U Number Boolean)])            #:transparent)
(struct binop    ([op : Symbol] [l : Expr] [r : Expr])   #:transparent)
(struct var-expr ([id : Symbol])                         #:transparent)
(struct let-expr ([id : Symbol] [e1 : Expr] [e2 : Expr]) #:transparent)
(struct if-expr  ([eb : Expr] [et : Expr] [ef : Expr])   #:transparent)

(define-type Expr (U const
                     binop
                     var-expr
                     let-expr
                     if-expr))

(define-predicate expr? Expr)

;----------------------Dodane definicje składni konkretnej-------------------------------
(define-type ConcrNum Number)
(define-type ConcrBool (U 'true 'false))
(define-type ConcrVar Symbol)
(define-type ConcrLet (List 'let (List Symbol Concr) Concr))
(define-type ConcrBin (List Symbol Concr Concr))
(define-type ConcrIf (List 'if Concr Concr Concr))

(define-predicate concr-num? ConcrNum)
(define-predicate concr-bool? ConcrBool)
(define-predicate concr-var? ConcrVar)
(define-predicate concr-let? ConcrLet)
(define-predicate concr-bin? ConcrBin)
(define-predicate concr-if? ConcrIf)

(define-type Concr (U
                    ConcrNum
                    ConcrBool
                    ConcrVar
                    ConcrLet
                    ConcrBin
                    ConcrIf))

;----------------------Parser wykorzystujący powyższe definicje----------------------------
(: parse (-> Concr Expr))
(define (parse q)
  (cond
    [(concr-num? q) (const q)]
    [(concr-bool? q) (const (if (eq? q 'true)
                                true
                                false))] 
    [(concr-var? q) (var-expr q)]
    [(concr-let? q)
     (let-expr (first (second q))
               (parse (second (second q)))
               (parse (third q)))]
     [(concr-bin? q)
     (binop (first q)
            (parse (second q))
            (parse (third q)))]
    [(concr-if? q)
     (if-expr (parse (second q))
              (parse (third q))
              (parse (fourth q)))]))

(define-type EType (U 'real 'boolean))

;----------------------------Środowisko typów------------------------

(struct environ ([xs : (Listof (Pairof Symbol EType))]))
(define-type Env environ)

(define env-empty (environ null))

(: env-add (-> Symbol EType Env Env))
(define (env-add x v env)
  (environ (cons (cons x v) (environ-xs env))))

(: env-lookup (-> Symbol Env EType))
(define (env-lookup x env)
  (: assoc-lookup (-> (Listof (Pairof Symbol EType)) EType))
  (define (assoc-lookup xs)
    (cond [(null? xs) (error "Unknown identifier" x)]
          [(eq? x (car (car xs))) (cdr (car xs))]
          [else (assoc-lookup (cdr xs))]))
  (assoc-lookup (environ-xs env)))


;---------------------------Type Checker---------------------------------
(: member? (-> Any (Listof Any) Boolean))
(define (member? x xs) (if (not (not (member x xs))) #t #f))

(define arith-operators (list '+ '- '* '/ '%))
(define comp-operators (list '= '> '>= '< '<=))
(define logic-operators (list 'and 'or))

(: op->type (-> Symbol (U EType #f) (U EType #f) (U EType #f)))
(define (op->type op l r)
  (cond
    [(member? op arith-operators) (if (and (eq? l 'real)
                                           (eq? r 'real))
                                      'real
                                      #f)]
    [(member? op comp-operators) (if (and (eq? l 'real)
                                          (eq? r 'real))
                                     'boolean
                                     #f)]
    [(member? op logic-operators) (if (and (eq? l 'boolean)
                                           (eq? r 'boolean))
                                      'boolean
                                      #f)]
    [else #f]))




(: typecheck-env (-> Expr Env (U EType #f)))
(define (typecheck-env e env)
  (match e
    [(const n) (if (number? n) 'real 'boolean)]
    [(var-expr x) (env-lookup x env)]
    [(binop op l r) (let ([left (typecheck-env l env)]
                          [right (typecheck-env r env)])
                      (op->type op left right))]
    [(let-expr x e1 e2) (let ([type-e1 (typecheck-env e1 env)])
                          (if (eq? type-e1 #f) #f
                              (typecheck-env e2 (env-add x type-e1 env))))]
    [(if-expr eb et ef) (let ([condition (typecheck-env eb env)]
                              [true-type (typecheck-env et env)]
                              [false-type (typecheck-env ef env)])
                          (cond
                            [(not (eq? condition 'boolean)) #f]
                            [(not (eq? true-type false-type)) #f]
                            [else true-type]))]))


(: typecheck (-> Expr (U EType #f)))
(define (typecheck e)
  (typecheck-env e env-empty))



;Testy
(typecheck (parse '2))
(typecheck (parse '(+ 4 2)))
(typecheck (parse '(+ true 2)))
(typecheck (parse '(if true (+ 4 2) 5)))
(typecheck (parse '(if true (+ 4 2) false)))
(typecheck (parse '(if (> 666 42) (+ 4 2) 5)))
(typecheck (parse '(let (x 42) (if (= 42 42) x 10))))
(typecheck (parse '(let (x true) (if (= 42 42) x false))))
(typecheck (parse '(let (x true) (if (= 42 42) x 5))))
















   
  
