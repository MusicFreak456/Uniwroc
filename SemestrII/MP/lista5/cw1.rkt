#lang racket

(define (var? t)
  (symbol? t))

(define (neg? t)
  (and (list? t)
       (= 2 (length t))
       (eq? 'neg (car t))))

(define (neg f)
  (list 'neg f))

(define (neg-subf f)
  (second f))

(define (conj? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'conj (car t))))

(define (conj lf rf)
  (list 'conj lf rf))

(define (conj-left f)
  (second f))

(define (conj-right f)
  (third f))

(define (disj? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'disj (car t))))

(define (disj lf rf)
  (list 'disj lf rf))

(define (disj-left f)
  (second f))

(define (disj-right f)
  (third f))

(define (prop? f)
  (or (var? f)
      (and (neg? f)
           (prop? (neg-subf f)))
      (and (disj? f)
           (prop? (disj-left f))
           (prop? (disj-right f)))
      (and (conj? f)
           (prop? (conj-left f))
           (prop? (conj-right f)))))

(prop? (conj (neg 'x) (disj 'y 'z)))