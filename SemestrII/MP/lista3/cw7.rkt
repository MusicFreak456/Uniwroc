#lang racket

(define test (list 1 2 4))

(define (insert what lis where)
  (define (insert-aux index res to-be-processed)
    (cond
      [(= index where)(insert-aux (+ index 1) (append res (list what)) to-be-processed)]
      [(empty? to-be-processed) res]
      [else (insert-aux (+ index 1) (append res (list (car to-be-processed))) (cdr to-be-processed))]))
  (insert-aux 0 '() lis))

(define (permi ls)
  (define (permi-iter what ls)
    (define len (length ls))
    (define (permi-iter-aux res index)
      (if (= index len)
          (append res (list (insert what ls index)))
          (permi-iter-aux (append res (list (insert what ls index))) (+ index 1))))
   (permi-iter-aux '() 0))
  (define (iter-thru what to-proc)
    (define len (length to-proc))
    (define (iter-thru-aux res index to-proce)
      (cond
        [(empty? to-proce) (append res (permi-iter what '()))] 
        [(= index (- len 1) ) (append res (permi-iter what (list-ref to-proce index)))]
        [else (iter-thru-aux (append res (permi-iter what (list-ref to-proce index))) (+ index 1) to-proce)]))
    (iter-thru-aux '() 0 to-proc))
  (if (empty? ls) ls
      (iter-thru (car ls) (permi (cdr ls)))))

(permi '(1 2 3))
  
    
  