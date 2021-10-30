#lang racket


;;iteracyjny algorytm na obliczenie An

(define (a-iter-iter res prev num den iter i)
  (cond [(= i -1) 1]
        [(= i 0) 0]
        [(> iter i) res]
        [else (a-iter-iter (+ (* (den iter) res) (* (num iter) prev)) res num den (+ iter 1) i)]))

(define (a-iter num den i) (a-iter-iter 0 1 num den 1 i))

;;iteracyjny algorytm na obliczanie Bn


(define (b-iter-iter res prev num den iter i)
  (cond [(= i -1) 0]
        [(= i 0) 1]
        [(> iter i) res]
        [else (b-iter-iter (+ (* (den iter) res) (* (num iter) prev)) res num den (+ iter 1) i)]))

(define (b-iter num den i) (b-iter-iter 1 0 num den 1 i))


;;algorytm na oblliczenie fk przy zadanym num i den

(define (cont-frac-iter  num den res next iter) (if (< (abs (- res next)) 0.00001) res (cont-frac-iter num den next (/ (a-iter num den iter) (b-iter num den iter)) (+ iter 1)))) 

(define (cont-frac num den) (cont-frac-iter num den (/ (a-iter num den 1) (b-iter num den 1)) (/ (a-iter num den 2) (b-iter num den 2)) 3))


;;Testy

(define (atan-cf x) (cont-frac (lambda (i) (if (= i 1) x (sqr (* (- i 1) x)))) (lambda (i) (+ (*(- i 1) 2.0) 1))))

(atan-cf 14)
(atan 14)

(define fi (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0))))

fi

