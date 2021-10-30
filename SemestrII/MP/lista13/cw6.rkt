#lang typed/racket

; --------- ;
; Wyrazenia ;
; --------- ;

(struct const    ([val : (U Number Boolean)])      #:transparent)
(struct binop    ([op : Symbol] [l : Expr] [r : Expr])   #:transparent)
(struct var-expr ([id : Symbol])       #:transparent)
(struct let-expr ([id : Symbol] [e1 : Expr] [e2 : Expr]) #:transparent)
(struct if-expr  ([eb : Expr] [et : Expr] [ef : Expr]) #:transparent)

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
;----------------------parse wykorzystujący powyższe definicje----------------------------
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
              (parse (fourth q)))]
   ))

(define (test-parse) (parse '(let [x (+ 2 2)] (+ x 1))))

; ---------- ;
; Srodowiska ;
; ---------- ;

(struct environ ([xs : (Listof (Pairof Symbol Value))]))

(define-type Env environ)

(define env-empty (environ null))
(: env-add (-> Symbol Value Env Env))
(define (env-add x v env)
  (environ (cons (cons x v) (environ-xs env))))
(: env-lookup (-> Symbol Env Value))
(define (env-lookup x env)
  (: assoc-lookup (-> (Listof (Pairof Symbol Value)) Value))
  (define (assoc-lookup xs)
    (cond [(null? xs) (error "Unknown identifier" x)]
          [(eq? x (car (car xs))) (cdr (car xs))]
          [else (assoc-lookup (cdr xs))]))
  (assoc-lookup (environ-xs env)))

; --------- ;
; Ewaluacja ;
; --------- ;

(define-type Value (U Number Boolean))
(define-predicate value? Value)

(: op->proc (-> Symbol (-> Value Value Value)))
(define (op->proc op)
  (match op ['+ (arith-convert +)]
            ['- (arith-convert -)]
            ['* (arith-convert *)]
            ['/ (arith-convert /)]
            ['% (modulo-convert modulo)] 
            ['= (compare-convert =)]
            ['> (compare-convert >)]
            ['>= (compare-convert >=)]
            ['< (compare-convert <)]
            ['<= (compare-convert <=)]
            ['and (lambda (x y) (and x y))]
            ['or  (lambda (x y) (or  x y))]))

(: arith-convert (-> (-> Number Number Number) (-> Value Value Value)))
(define (arith-convert p)
  (lambda (a b)
    (cond
      [(and (number? a) (number? b)) (p a b)]
      [else (error "Type mismatch")])))

(: compare-convert (-> (-> Real Real Boolean) (-> Value Value Value)))
(define (compare-convert p)
  (lambda (a b)
    (cond
      [(and (real? a) (real? b)) (p a b)]
      [else (error "Type mismatch")])))

(: modulo-convert (-> (-> Integer Integer Integer) (-> Value Value Value)))
(define (modulo-convert p)
  (lambda (a b)
    (cond
      [(and (exact-integer? a) (exact-integer? b)) (p a b)]
      [else (error "Type mismatch")])))


(: eval-env (-> Expr Env Value))
(define (eval-env e env)
  (match e
    [(const n) n]
    [(binop op l r) ((op->proc op) (eval-env l env)
                                   (eval-env r env))]
    [(let-expr x e1 e2)
     (eval-env e2 (env-add x (eval-env e1 env) env))]
    [(var-expr x) (env-lookup x env)]
    [(if-expr eb et ef) (if (eval-env eb env) ; <----------------- !!!
                            (eval-env et env)
                            (eval-env ef env))]))
(: eval (-> Expr Value))
(define (eval e) (eval-env e env-empty))

(define program
  '(if (or (< (% 123 10) 5)
           true)
       (+ 2 3)
       (/ 2 0)))

(define (test-eval) (eval (parse program)))
