#lang racket
;; Na podstawie szablonu ze skosa

(provide
 make-elem
 elem-priority
 elem-val
 empty-heap
 heap-insert
 heap-merge
 heap-min
 heap-pop
 heap-empty?)

(define (inc n)
  (+ n 1))

(define (tagged-list? len-xs tag xs)
  (and (list? xs)
       (= len-xs (length xs))
       (eq? (first xs) tag)))

(define (make-elem pri val)
  (cons pri val))

(define (elem-priority x)
  (car x))

(define (elem-val x)
  (cdr x))

(define leaf 'leaf)

(define (leaf? h) (eq? 'leaf h))

(define (hnode? h)
  (and (tagged-list? 5 'hnode h)
       (natural? (caddr h))))

(define (make-hnode elem heap-a heap-b)
  (if (> (rank heap-a) (rank heap-b))
     (list 'hnode elem (+ (rank heap-b) 1) heap-a heap-b)
     (list 'hnode elem (+ (rank heap-a) 1) heap-b heap-a)))
        

(define (hnode-elem h)
  (second h))

(define (hnode-left h)
  (fourth h))

(define (hnode-right h)
  (fifth h))

(define (hnode-rank h)
  (third h))

(define (hord? p h)
  (or (leaf? h)
      (<= p (elem-priority (hnode-elem h)))))

(define (heap? h)
  (or (leaf? h)
      (and (hnode? h)
           (heap? (hnode-left h))
           (heap? (hnode-right h))
           (<= (rank (hnode-right h))
               (rank (hnode-left h)))
           (= (rank h) (inc (rank (hnode-right h))))
           (hord? (elem-priority (hnode-elem h))
                  (hnode-left h))
           (hord? (elem-priority (hnode-elem h))
                  (hnode-right h)))))
(define (rank h)
  (if (leaf? h)
      0
      (hnode-rank h)))

(define empty-heap leaf)

(define (heap-empty? h)
  (leaf? h))

(define (heap-insert elt heap)
  (heap-merge heap (make-hnode elt leaf leaf)))

(define (heap-min heap)
  (hnode-elem heap))

(define (heap-pop heap)
  (heap-merge (hnode-left heap) (hnode-right heap)))

(define (heap-merge h1 h2)
  (cond
   [(leaf? h1) h2]
   [(leaf? h2) h1]
   [( > (elem-priority (heap-min h1)) (elem-priority (heap-min h2)))
          (let ((e (heap-min h2) )
               (h1 (hnode-left h2))
               (hr (hnode-right h2))
               (h h1))
                   (make-hnode e (heap-merge hr h) h1))]
   [else (let ((e (heap-min h1) )
               (h1 (hnode-left h1))
               (hr (hnode-right h1))
               (h h2))
                   (make-hnode e (heap-merge hr h) h1))]
   ))

  


(define e1 (make-elem 0 'first))
(define e2 (make-elem 1 'second))
(define e3 (make-elem 2 'third))
(define e4 (make-elem 3 'fourth))
(define e5 (make-elem 4 'fifth))

(define heap1 (heap-insert e5 (heap-insert e4 (heap-insert e3(heap-insert e2 (heap-insert e1 empty-heap))))))
(define heap2 (heap-insert e1 (heap-insert e1 (heap-insert e1(heap-insert e1 (heap-insert e1 empty-heap))))))
(define heap3 (heap-insert e1 (heap-insert e4 (heap-insert e3(heap-insert e2 (heap-insert e5 empty-heap))))))
(define heap4 (heap-insert e2 (heap-insert e1 (heap-insert e3(heap-insert e4 (heap-insert e5 empty-heap))))))

(heap? heap1)
(heap? heap2)
(heap? heap3)
(heap? heap4)




