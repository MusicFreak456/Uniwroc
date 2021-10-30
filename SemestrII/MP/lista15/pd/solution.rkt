#lang racket

;Implementacja dining-table napisana po konsultacji z Pawłem Zmarzłym
(provide philosopher)
#|
;-----------------------Procedury z wykładu-------------------------
(define (random-sleep)
  (sleep (/ (random) 100)))

(define (run-concurrent . thunks)
  (define threads (map thread thunks))
  (for-each thread-wait threads))

;---------------------Implementacja dining-table---------------------
(define (make-dining-table)
  (define forks (list
                 (make-semaphore 1)
                 (make-semaphore 1)
                 (make-semaphore 1)
                 (make-semaphore 1)
                 (make-semaphore 1)))
  (define (pick-fork k)
    (random-sleep)
    (semaphore-wait (list-ref forks k)))
  (define (put-fork k)
    (random-sleep)
    (semaphore-post (list-ref forks k)))

  (lambda (action)
    (match action
      ['pick-fork pick-fork]
      ['put-fork put-fork])))
|#
;-------------Procedura philosopher i procedury pomocnicze------------
;----------Klasyczny problem, więc i klasyczne rozwiązanie ;)---------

(define (right-handed-philosopher dinner-table id)
  ((dinner-table 'pick-fork) (modulo (+ id 1) 5))
  ((dinner-table 'pick-fork) id)
  ((dinner-table 'put-fork) id)
  ((dinner-table 'put-fork) (modulo (+ id 1) 5)))

(define (left-handed-philosopher dinner-table id)
  ((dinner-table 'pick-fork) id)
  ((dinner-table 'pick-fork) (+ id 1))
  ((dinner-table 'put-fork) (+ id 1))
  ((dinner-table 'put-fork) id))
  

(define (philosopher dinner-table id)
  (if (= id 4)
      (right-handed-philosopher dinner-table id)
      (left-handed-philosopher dinner-table id)))
        
#|
;-------------------Przykładowe testy----------------------------------

(define (test)
  (define dining-table (make-dining-table))
  (run-concurrent
   (lambda ()
     (for ([i 3])
       (philosopher dining-table 0)))
   (lambda ()
     (for ([i 5])
       (philosopher dining-table 1)))
   (lambda ()
     (for ([i 1])
       (philosopher dining-table 2)))
   (lambda ()
     (for ([i 2])
       (philosopher dining-table 3)))
   (lambda ()
     (for ([i 2])
       (philosopher dining-table 4)))))

    
(for ([i 1000])
  (displayln (string-append "done " (number->string i)))
  (test))
|#
;------------------------------------------------------------------------