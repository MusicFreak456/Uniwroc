#lang racket

(require quickcheck)

(define-signature intset^
  ((contracted
	[intset? (-> any/c boolean?)]
	[setmember? (-> integer? intset? boolean?)]
;--------------------------------------------------------------
	[empty-set intset?]
;--------------------------------------------------------------
	[singleton (-> integer? intset?)]
;--------------------------------------------------------------
	[union (-> intset? intset? intset?)]
	[intersection (-> intset? intset? intset?)])))

(define (strictly-sorted? l)
  (cond [(null? l) #t]
    	[(null? (cdr l)) #t]
    	[else (and (< (car l) (cadr l))
               	(strictly-sorted? (cdr l)))]))


(define-unit intset-ordered-list@
  (import)
  (export intset^)

  ; 1. próba:
  ;(define (intset? s)
  ; (and ((listof integer?) s)  (sorted? s)) ;sorted? z k1-sort.rkt

  (define intset?
    (and/c (listof integer?) strictly-sorted?))

  (define (setmember? n ns)  ; (member n ns)
    (cond
      [(null? ns) #f]
      [(= n (car ns)) #t]
      [(> n (car ns)) (setmember? n (cdr ns))]
      [(< n (car ns)) #f]))
;--------------------------------------------------------------
  (define empty-set '())
;--------------------------------------------------------------
  (define (singleton x) (list x))
;--------------------------------------------------------------
  (define (union xs ys)
    (cond
      [(null? xs) ys]
      [(null? ys) xs]
      [(= (car xs) (car ys))  (cons (car xs)
                                    (union (cdr xs) (cdr ys)))]
      [(< (car xs) (car ys))  (cons (car xs)
                                    (union (cdr xs)      ys ))]
      [(> (car xs) (car ys))  (cons (car ys)
                                    (union      xs  (cdr ys)))]))
  (define (intersection xs ys)
    (cond
      [(null? xs) null]
      [(null? ys) null]
      [(= (car xs) (car ys))  (cons (car xs)
                                  (intersection (cdr xs) (cdr ys)))]
      [(< (car xs) (car ys))      (intersection (cdr xs)      ys  )]
      [(> (car xs) (car ys))      (intersection    xs     (cdr ys))]
 ))
)


(define-values/invoke-unit/infer intset-ordered-list@)

(union '(1 2 3) '(2 5 6))

(define arbitrary-intlist
  (arbitrary-list arbitrary-integer))

(define (intlist->intset xs)  ;niezależna od implementacji
   (if (null? xs)
       empty-set
       (union (singleton (car xs)) (intlist->intset (cdr xs)))))
(define test (intlist->intset '(3 1 2)))
test

(quickcheck
  (property ([x arbitrary-integer])
    (not (setmember? x empty-set))))

(quickcheck
  (property ([x arbitrary-integer]
             [y arbitrary-integer])
    (equal? (= x y) (setmember? x (singleton y)))))

;----------------------------------------------------------------
(quickcheck   ; własność niezależna od implementacji
  (property ([xl arbitrary-intlist]
             [yl arbitrary-intlist]
             [a  arbitrary-integer])
  (let ([x (intlist->intset xl)]
        [y (intlist->intset yl)])
    (equal? (setmember? a (union x y))
            (or (setmember? a x) (setmember? a y)))
  )
))


(quickcheck
  (property ([xl arbitrary-intlist]
             [yl arbitrary-intlist]
             [a  arbitrary-integer])
  (let ([x (intlist->intset xl)]
        [y (intlist->intset yl)])
    (equal? (setmember? a (intersection x y))
            (and (setmember? a x) (setmember? a y)))
  )
))

(quickcheck   ; własność niezależna od implementacji
  (property ([xl arbitrary-intlist]
             [yl arbitrary-intlist]
             [zl arbitrary-intlist]
             [a  arbitrary-integer])
  (let ([x (intlist->intset xl)]
        [y (intlist->intset yl)]
        [z (intlist->intset zl)])
    (equal? (setmember? a (union x (intersection y z)))
            (setmember? a (intersection (union x y) (union x z))))
  )
))

(quickcheck   ; własność niezależna od implementacji
  (property ([xl arbitrary-intlist]
             [yl arbitrary-intlist]
             [zl arbitrary-intlist]
             [a  arbitrary-integer])
  (let ([x (intlist->intset xl)]
        [y (intlist->intset yl)]
        [z (intlist->intset zl)])
    (equal? (setmember? a (intersection x (union y z)))
       (setmember? a (union (intersection x y) (intersection x z))))
  )
))





