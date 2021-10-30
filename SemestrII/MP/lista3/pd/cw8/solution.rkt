#lang racket

(provide merge split mergesort)

(define (merge ls1 ls2)
  (define (merge-aux result ls1 ls2)
    (cond [(and (empty? ls1) (empty? ls2)) result]
          [(empty? ls1) (append result ls2)]
          [(empty? ls2) (append result ls1)]
          [else (if (< (car ls1) (car ls2))
                    (merge-aux (append result (list (car ls1)) ) (cdr ls1) ls2)
                    (merge-aux (append result (list (car ls2)) ) ls1 (cdr ls2)))]))
  (merge-aux '() ls1 ls2))

(define (split ls)
  (define len (length ls))
  (define half (ceiling (/ len 2)))
  (define (split-aux result to-be-processed)
    (if (= (length result) half) (cons result to-be-processed)
        (split-aux (append result (list (car to-be-processed))) (cdr to-be-processed))))
  (split-aux '() ls))

(define (mergesort ls)
  (if (<= (length ls) 1) ls
      (let ((split-res (split ls)))
        (merge (mergesort (car split-res)) (mergesort (cdr split-res))))))


;;Testy z internetu (z dużym prawdopodobieństwem ktoś w grupie może mieć identyczne ;) )
(define a (list 12 15 23 4 6 10 35 28)) ;;Even number of element
(define b '()) ;;Empty list 
(define c (list 4 6 10 12 15 23 28 35));;already sorted array
(define d (list 12 15 23 4 6 10 35)) ;;odd length array
(define e (list 35 28 23 15 12 10 6 4)) ;;descending sorted array input
(define f (list 12));;one element
(define g (list 12 4)); // two elements
(define h (list 12 15 23 4 6 10 35 28 100 130 500 1000 235 554 75 345 800 222 333 888 444 111 666 777 60));;large list of elements
(define i (list 12 15 -23 -4 6 10 -35 28));;negative elements
(define j (list 12 12 23 4 6 6 10 -35 28));;duplicate elements
(define k (list 12 12 12 12 12)) ;;Same element

(mergesort a)
(mergesort b)
(mergesort c)
(mergesort d)
(mergesort e)
(mergesort f)
(mergesort g)
(mergesort h)
(mergesort i)
(mergesort j)
(mergesort k)


