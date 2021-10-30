#lang racket

(require racket/contract)

(define/contract (prefixes xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (if (null? xs) (list xs)
      (cons null (map (lambda (x) (cons (car xs) x)) (suffixes (cdr xs))))))

(define/contract (suffixes xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (if (null? xs)
      (list xs)
      (cons xs (suffixes (cdr xs)))))

(suffixes '(2 1 3))
