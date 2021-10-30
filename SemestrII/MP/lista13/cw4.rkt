#lang typed/racket

(struct [a b] node ([v : a] [xs : b]) #:transparent)
(struct leaf () #:transparent)

(define-type (Rosetree a) (U leaf (node a (Listof (Rosetree a)))))
(define-predicate rosetree? (Rosetree Any))

(define exemple
  (node 1 (list (leaf) (node 2 (list)) (node 3 (list)))))

(: preorder (All (a) (-> (Rosetree a) (Listof a))))
(define (preorder t)
  (match t
    [(node v xs) (cons v (apply append (map (ann preorder (-> (Rosetree a) (Listof a))) xs)))]
    [(leaf) (list)]))