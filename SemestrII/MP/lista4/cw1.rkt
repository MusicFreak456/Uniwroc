#lang racket

(define (queens board-size)
  (define (empty-board)
    (define (empty-board-aux res)
      (if (= (length res) board-size) res
          (empty-board-aux (cons 0 res))))
    (empty-board-aux '()))
  (define (adjoin-position row col rest)
    (if (= (- board-size (length rest) -1) row)
        (cons col (cdr rest))
        (cons (car rest) (adjoin-position row col (cdr rest)))))
  (define (safe? k positions)
    (define same? (if
  )

(queens 3)