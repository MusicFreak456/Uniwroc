#lang racket

(require racket/contract)


(define (proc x)
  (parametric->/c [a b] (-> a b))
  (error))