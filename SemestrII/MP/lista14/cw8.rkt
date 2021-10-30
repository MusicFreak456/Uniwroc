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

(define (list->num xs)
  (define (aux acc xs)
    (match xs
      ['() acc]
      [(cons x xs) (aux (+ (* acc 10) x) xs)]))
  
  (aux 0 xs))

(define %sublist
  (%rel (x xs ys z)
    [('() xs)]
    [((cons x xs) (cons x ys))
       (%sublist xs ys)]
    [((cons z xs) (cons x ys))
       (%sublist (cons z xs) ys)]))



;’() nie matchuje się z null; Trochę to dziwne
;

(define %perm
  (%rel (x xs ys zs)
        [(null null)]
        [((cons x xs) ys)
         (%select x ys zs)
         (%perm xs zs)]))


;(list->num chyba zawsze zwraca 0)
;Potwierdzam

(define %variation
  (%rel (xs ys zs)
    [(xs zs)
     (%perm ys zs)
     (%sublist xs ys)])) ; Jak zrobić szybciej?

(define %is-list-sum
  (%rel (xs ys zs x y z)
     [(xs ys zs)
      (%is x (list->num xs))
      (%is y (list->num ys))
      (%is z (list->num zs))
      (%is z (+ x y))]))

(time (%which (d e m n o r s y)
     (%variation (list d e m n o r s y) (list 0 1 2 3 4 5 6 7 8 9))
     (%=/= s 0)
     (%=/= m 0)
     (%is-list-sum (list s e n d) 
                   (list m o r e)
                   (list m o n e y))
))
