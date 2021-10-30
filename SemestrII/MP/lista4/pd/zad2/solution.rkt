#lang racket

(require "leftist.rkt")
(provide heapsort)

(define (heapsort list) (sortowanko-kopcowanko list))

(define (sortowanko-kopcowanko list)
  (define (sortowanko-kopcowanko-aux heap)
  (if (heap-empty? heap) null
      (cons (car (heap-min heap)) (sortowanko-kopcowanko-aux (heap-pop heap)))))
  (define (create-heap res list)
    (if (null? list) res
        (create-heap (heap-insert (make-elem (car list) (car list)) res) (cdr list))))
  (let ((new-heap (create-heap empty-heap list)))
    (sortowanko-kopcowanko-aux new-heap)))












