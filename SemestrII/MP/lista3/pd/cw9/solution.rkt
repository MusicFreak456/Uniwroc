#lang racket

(provide partition quicksort)

(define (partition n xs)
  (define (partition-aux ls1 ls2 n to-be-processed)
    (if (empty? to-be-processed) (cons ls1 ls2)
        (if (<= (car to-be-processed) n)
            (partition-aux (append (list (car to-be-processed)) ls1) ls2 n (cdr to-be-processed))
            (partition-aux ls1 (append (list (car to-be-processed)) ls2) n (cdr to-be-processed)))))
  (partition-aux '() '() n xs))

(define list1 (list 1 2 1 3))

(define (quicksort xs)
  (define len (length xs))
  (if (<= len 1) xs
      (let ((split (partition (car xs) (cdr xs))))
        (append (quicksort (car split)) (append (list (car xs))(quicksort (cdr split)))))))

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

(quicksort a)
(quicksort b)
(quicksort c)
(quicksort d)
(quicksort e)
(quicksort f)
(quicksort g)
(quicksort h)
(quicksort i)
(quicksort j)
(quicksort k)
