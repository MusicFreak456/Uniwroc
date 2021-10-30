#lang racket

(require racket/contract)

(define filter-parametric/c
  (parametric->/c [a] (-> (-> a boolean?) (listof a) (listof a))))

(define/contract (filter-para p? xs)
  filter-parametric/c
  (cond
    [(null? xs) xs]
    [(p? (car xs)) (cons (car xs) (filter p? (cdr xs)))]
    [else (filter p? (cdr xs))]))

(filter-para number? (list 1 2 'p 3))

(define (contains? l1 l2)
  (andmap (lambda (x) (member x l2)) l1))

(define (contained? xs)
  (lambda (ys) (contains? ys xs)))


(define filter-dep/c
  (->i ([p? (and/c procedure? (-> any/c boolean?))]
        [xs (listof any/c)])
       [result (xs) (and/c (listof any/c) (contained? xs))]
       #:post (p? result)
       (andmap p? result)))

(define/contract (filter-dep p? xs)
  filter-dep/c
  (cond
    [(null? xs) xs]
    [(p? (car xs)) (cons (car xs) (filter p? (cdr xs)))]
    [else (filter p? (cdr xs))]))

(filter-dep number? (list 1 2 'p 3))