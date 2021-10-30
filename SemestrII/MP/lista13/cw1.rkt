#lang typed/racket

(: prefixes (All (a) (-> (Listof a) (Listof (Listof a)))))
(define (prefixes xs)
  (if (null? xs) (list xs)
      (cons null (map (ann (lambda (x) (cons (car xs) x)) (-> (Listof a) (Listof a))) (prefixes (cdr xs))))))


