#lang racket

(define (make-point x y) (cons x y))

(define (point-x point) (car point))
(define (point-y point) (cdr point))
(define (point? cand) (and (pair? cand)
                           (real? (point-x cand)) (real? (point-y cand))))

(define (make-vect point-a point-b) (cons point-a point-b))
(define (vect-begin vec) (car vec))
(define (vect-end vec) (cdr vec))
(define (vect? cand) (and (point? (vect-begin cand) (vect-end cand))))

(define (vect-length vec)
  (define y-diff (- (point-y (vect-end vec)) (point-y (vect-begin vec))))
  (define x-diff (- (point-x (vect-end vec)) (point-x (vect-begin vec))))
  (sqrt (+ (sqr y-diff) (sqr x-diff))))

(define (vect-scale v k) 
  (define diff (make-vect (- (point-x (vect-begin v))
                             (point-x (vect-end v)))
                          (- (point-y (vect-begin v))
                             (point-y (vect-end v)))))
                            (make-vect (vect-begin v)
                                    (make-point (- (point-x (vect-begin v)) (* k (point-x diff)))
                                                (- (point-y (vect-begin v)) (* k (point-y diff))))))
(define (vect-translate v p)
  (define diff (make-vect (- (point-x p)
                             (point-x (vect-begin v)))
                          (- (point-y p)
                             (point-y (vect-begin v)))))
  (make-vect p (make-point (+ (point-x (vect-end v)) (point-x diff))
                           (+ (point-y (vect-end v)) (point-y diff)))))

( define ( display-point p )
( display "(")
( display ( point-x p ) )
( display ", ")
( display ( point-y p ) )
( display ")")
   )

( define ( display-vect v )
( display "[")
( display-point ( vect-begin v ) )
( display ", ")
( display-point ( vect-end v ) )
( display "]")
   (newline))

(define a (make-point 0 0))
(define b (make-point 1 1))

(define test (make-vect a b))
(vect-length test)

(display-vect test)
(define result (vect-scale test 5))
(display-vect result)

(define translate (vect-translate test b))
(display-vect translate)







                                    
 