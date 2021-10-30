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

(define test (parse '(exists x (and x (not x)))))
test
(formula? test)


   