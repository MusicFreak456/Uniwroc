#lang racket

(struct bdlist (v [prev #:mutable] [next #:mutable]) #:transparent)

(define (list->bdlist xs)
  (define (aux xs bdxs)
    (cond
      [(null? xs) null]
      [else (let ([new-elem (bdlist (car xs) bdxs null)])
              (begin
                (set-bdlist-next! bdxs new-elem)
                (aux (cdr xs) new-elem)
                bdxs))]))
  (aux (cdr xs) (bdlist (car xs) null null)))

(define (cycle-bdlist! xs)
  (define (aux xs head)
    (if (null? (bdlist-next xs))
        (begin
          (set-bdlist-next! xs head)
          (set-bdlist-prev! head xs))
        (aux (bdlist-next xs) head)))
  (if (null?  xs)
      (error "IMPOSIBRU")
      (aux xs xs)))

(define (decycle-bdlist! xs)
  (define (aux xs)
    (if (null? (bdlist-prev (bdlist-next xs)))
        (set-bdlist-next! xs null)
        (aux (bdlist-next xs))))
  (if (null? xs)
      (error "IMPOSIBRU")
      (begin
        (set-bdlist-prev! xs null)
        (aux xs))))
  

(define test (list->bdlist (list 1 2 3)))
(cycle-bdlist! test)
test
(decycle-bdlist! test)
test
