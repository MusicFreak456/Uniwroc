#lang racket

(define list-to-be-reversed (list 1 2 3))

(define (reverse lis)
  (if (null? (cdr lis)) (list (car lis))
      (append (reverse (cdr lis)) (list (car lis)))))

(reverse list-to-be-reversed)

(define (reverse-iter-aux reversed given)
  (if (empty? given) reversed
      (reverse-iter-aux (append (car given) reversed) (cdr given))))

(define (reverse-iter lis)
  (reverse-iter-aux '() lis))

(reverse list-to-be-reversed)