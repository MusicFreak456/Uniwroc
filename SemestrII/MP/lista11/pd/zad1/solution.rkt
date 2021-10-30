#lang racket

(require racket/contract)

(provide (contract-out
          [with-labels with-labels/c]
          [foldr-map foldr-map/c]
          [pair-from pair-from/c]))
(provide with-labels/c foldr-map/c pair-from/c)

;----------------------WITH-LABELS-------------------------------------------------------------

(define with-labels/c
  (parametric->/c [a b] (->
                         (-> a b)
                         (listof a)
                         (listof (cons/c b (cons/c a null?))))))
  
(define (with-labels f xs)
  (if (null? xs) xs
      (cons (list (f (car xs)) (car xs)) (with-labels f (cdr xs)))))

(with-labels number->string (list 1 2 3))

;----------------------FOLDR-MAP---------------------------------------------------------------

(define foldr-map/c
  (parametric->/c [oe a e] (-> (-> oe a (cons/c e a)) a (listof oe) (cons/c (listof e) a))) )
  

(define/contract (foldr-map f acc xs)
  foldr-map/c
  (if (null? xs) (cons null acc)
      (let* ([next (foldr-map f acc (cdr xs))]
             [prod (f (car xs) (cdr next))])
        (cons (cons (car prod) (car next)) (cdr prod)))))
      
(foldr-map (lambda (x a) (cons a (+ a x))) 0 '(1 2 3))


;-----------------------PAIR-FROM--------------------------------------------------------------

(define pair-from/c
  (parametric->/c [a b c] (-> (-> a b) (-> a c) (-> a (cons/c b c)))))
  

(define (pair-from f g)
  (lambda (x) (cons (f x) (g x))))

((pair-from (lambda (x) (+ x 1)) (lambda (x) (* x 2))) 2)