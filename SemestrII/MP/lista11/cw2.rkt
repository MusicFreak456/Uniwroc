#lang racket

(require racket/contract)

(define/contract (sublists xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (if (null? xs)
      (list null)
      (append-map
        (lambda (ys) (list (cons (car xs) ys) ys))
        (sublists (cdr xs)))))

(sublists '(1 2))
(sublists '(1 2 3))
