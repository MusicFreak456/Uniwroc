#lang racket

(require racklog)

(define %my-append
  (%rel (x xs ys zs)
        [(null ys ys)]
        [((cons x xs) ys (cons x zs))
         (%my-append xs ys zs)]))

(define %my-member
  (%rel (x xs y)
        [(x (cons x xs))]
        [(y (cons x xs))
         (%my-member y xs)]))

(define %sublist
  (%rel (xs ys zs x)
        [(xs ys) (%my-append xs zs ys)]
        [(xs (cons x ys))
         (%sublist xs ys)]))

(define %select
  (%rel (x xs y ys)
        [(x (cons x xs) xs)]
        [(y (cons x xs) (cons x ys))
         (%select y xs ys)]))

;; prosta rekurencyjna definicja
(define %simple-length
  (%rel (x xs n m)
        [(null 0)]
        [((cons x xs) n)
         (%simple-length xs m)
         (%is n (+ m 1))]))

(%which () (%sublist '(1 3) '(1 2 3)))
