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
  (if (null? xs) null
  (aux (cdr xs) (bdlist (car xs) null null))))

(define test (list 1 2 3))

(list->bdlist test)

(bdlist-v (bdlist-next (bdlist-next (list->bdlist test))))

(define test2 (list->bdlist (list 1 'p 3)))


(define (bdfilter p? xs)
  (define (aux p? xs prev)
    (if (null? xs) null
        (if (p? (bdlist-v xs))
            (let ([new-elem (bdlist (bdlist-v xs) prev null)])
              (begin
                (set-bdlist-next! new-elem (aux p? (bdlist-next xs) new-elem))
                new-elem))
            (begin (set-bdlist-next! prev (aux p? (bdlist-next xs) prev))
                   prev))))     
  (if (null? xs) xs
      (aux p? (bdlist-next xs) (if (p? (bdlist-v xs))
                                   (bdlist (bdlist-v xs) (bdlist-prev xs) null)
                                   null))))

(bdfilter number? test2)