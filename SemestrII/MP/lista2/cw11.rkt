#lang racket

(provide nth-root)

;;Poniższe funkcje pisane na podstawie notatek z wykładu
(define (close-enough x y) (< (abs (- x y)) 0.00001))

(define (average x y) (/ (+ x y) 2))

(define (average-damp f) (lambda (x) (average x (f x))))

(define (fixed-point s f) (if (close-enough s (f s)) s (fixed-point (f s) f)))

;;Funkcja repeated z cw. 3
(define (repeated p n)
  (define (compose f g) (lambda (x) (f (g x))))
  (define (identity x) x )
  (if (= n 0) identity (compose p (repeated p (- n 1)))))


;;"Eksperymentalne" szacowanie potrzebnej ilości tłumień:
(define (2-nd-root x) (let ((fun (lambda (i) (/ x i))))
                        (fixed-point 1.0 ((repeated average-damp 1) fun))))

(2-nd-root 4)

(define (3-rd-root x) (let ((fun (lambda (i) (/ x (expt i 2)))))
                        (fixed-point 1.0 ((repeated average-damp 1) fun))))

(3-rd-root 8)


;;(define (4-th-root x) (let ((fun (lambda (i) (/ x (expt i 3)))))
;;                        (fixed-point 1.0 ((repeated average-damp 1) fun))))
;;jednokrotne nie działa dla 4 stopnia

(define (4-th-root x) (let ((fun (lambda (i) (/ x (expt i 3)))))
                        (fixed-point 1.0 ((repeated average-damp 2) fun))))

(4-th-root 16)

(define (5-th-root x) (let ((fun (lambda (i) (/ x (expt i 4)))))
                        (fixed-point 1.0 ((repeated average-damp 2) fun))))

(5-th-root 32)

(define (7-th-root x) (let ((fun (lambda (i) (/ x (expt i 6)))))
                        (fixed-point 1.0 ((repeated average-damp 2) fun))))

(7-th-root 128)

;;(define (8-th-root x) (let ((fun (lambda (i) (/ x (expt i 7)))))
;;                        (fixed-point 1.0 ((repeated average-damp 2) fun))))
;;Dwukrotne nie zadziała dla stopnia 8, ale działało dla 7, więc prawdopodobnie ilość tłumień to podłoga z logarytmu
;;o podstawie dwa ze stopnia pierwiastka

(define (nth-root n x) (let ((fun (lambda (i) (/ x (expt i (- n 1))))))
                         (fixed-point 1.0 ((repeated average-damp (floor (log n 2))) fun))))

(nth-root 8 2)
(nth-root 15 2)





