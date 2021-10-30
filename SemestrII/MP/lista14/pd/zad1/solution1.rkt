#lang racket

;Pisane we współpracy z Krystianem Jasionkiem i Piotrem Dobiechem


(require racklog)

(provide solve)

;; transpozycja tablicy zakodowanej jako lista list
(define (transpose xss)
  (cond [(null? xss) xss]
        ((null? (car xss)) (transpose (cdr xss)))
        [else (cons (map car xss)
                    (transpose (map cdr xss)))]))

;; procedura pomocnicza
;; tworzy listę n-elementową zawierającą wyniki n-krotnego
;; wywołania procedury f
(define (repeat-fn n f)
  (if (eq? 0 n) null
      (cons (f) (repeat-fn (- n 1) f))))

;; tworzy tablicę n na m elementów, zawierającą świeże
;; zmienne logiczne
(define (make-rect n m)
  (repeat-fn m (lambda () (repeat-fn n _))))

(define %skip*
  (%rel (xs ys)
        [((cons '_ xs) (cons '_ xs))]
        [((cons '* xs) ys)
         (%skip* xs ys)]))

;; predykat binarny
;; (%row-ok xs ys) oznacza, że xs opisuje wiersz (lub kolumnę) ys
(define %row-ok
  (%rel (x y xs ys zs)
        [(null null)]
        [(null (cons '_ xs))
         (%row-ok null xs)]
        [((cons x xs) (cons '_ ys))
         (%row-ok (cons x xs) ys)]
        [((cons x xs) (cons '* ys))
         (%skip* (cons '* ys) zs)
         (%length-ok x (cons '* ys))
         (%row-ok xs zs)]))

(define %length-ok
  (%rel (xs y x)
        [(0 null)]
        [(0 (cons '_ xs))]
        [(x (cons '* xs))
         (%is y (- x 1))
         (%length-ok y xs)]))

(define %board-ok
  (%rel (x xs y ys)
        [(null null)]
        [((cons x xs) (cons y ys))
         (%row-ok x y)
         (%board-ok xs ys)]))

;; funkcja rozwiązująca zagadkę
(define (solve rows cols)
  (define board (make-rect (length cols) (length rows)))
  (define tboard (transpose board))
  (define ret (%which (xss)
                      (%= xss board)
                      (%board-ok rows board)
                      (%board-ok cols tboard)
                      ))
  (and ret (cdar ret)))

(solve '((2) (1) (1)) '((1 1) (2)))
;; testy
(time (equal? (solve '((2) (1) (1)) '((1 1) (2)))
        '((* *)
          (_ *)
          (* _))))

(time (equal? (solve '((2) (2 1) (1 1) (2)) '((2) (2 1) (1 1) (2)))
        '((_ * * _)
          (* * _ *)
          (* _ _ *)
          (_ * * _))))

#|
(time (equal? (solve '((4) (6) (2 2) (2 2) (6) (4) (2) (2) (2))
               '((9) (9) (2 2) (2 2) (4) (4)))
        '((* * * * _ _)
          (* * * * * *)
          (* * _ _ * *)
          (* * _ _ * *)
          (* * * * * *)
          (* * * * _ _)
          (* * _ _ _ _)
          (* * _ _ _ _)
          (* * _ _ _ _))))|#

;; TODO: możesz dodać własne testy

