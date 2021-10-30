#lang racket

(require racket/contract)

;(parametric->/c [a b] (-> a b a ))
;(parametric->/c [a b c] (-> (-> a b c) (-> a b) a c))
;(parametric->/c [a b c] (-> (->b c) (-> a b) (-> a c)))
;(parametric->/c [a] (-> (-> (-> a a) a) a))

(define/contract (proc1 x y)
  (parametric->/c [a b] (-> a b a ))
  x)

(proc1 1 2)

(define/contract (proc2 f g arg)
  (parametric->/c [a b c] (-> (-> a b c) (-> a b) a c))
  (f arg (g arg)))

(proc2 (lambda (a b) (+ a b)) (lambda (x) x) 1)

(define/contract (proc3 f g)
  (parametric->/c [a b c] (-> (-> b c) (-> a b) (-> a c)))
  (lambda (x) (f (g x))))

((proc3 (lambda (x) (+ x 1)) (lambda (x) (+ x 1))) 1)

(define/contract (proc4 f)
  (parametric->/c [a] (-> (-> (-> a a) a) a))
  (f (lambda (x) x)))

(proc4 (lambda (f) (f 4)))
