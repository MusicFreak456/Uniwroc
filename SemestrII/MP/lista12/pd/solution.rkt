#lang racket

(require "graph.rkt")
(provide bag-stack@ bag-fifo@)

;; struktura danych - stos
(define-unit bag-stack@
  (import)
  (export bag^)
  
  (define (stack-list s) (second s))
  (define (stack-init xs) (list 'stack-bag xs))

  (define (bag? s) (and
                    (list? s)
                    (eq? (length s) 2)
                    (list? (second s))
                    (eq? (car s) 'stack-bag)))
  (define empty-bag (stack-init '()))
  (define (bag-empty? s) (null? (stack-list s)))
  (define (bag-insert s v) (stack-init (cons v (stack-list s))))
  (define (bag-peek s) (car (stack-list s)))
  (define (bag-remove s) (stack-init (cdr (stack-list s))))
)

;; struktura danych - kolejka FIFO
(define-unit bag-fifo@
  (import)
  (export bag^)
  (define (fifo-in f) (second f))
  (define (fifo-out f) (third f))
  (define (fifo-init xs ys) (list 'fifo-bag xs ys))
  (define (bag? f) (and
                    (list? f)
                    (eq? (length f) 3)
                    (list? (second f))
                    (list? (third f))
                    (eq? (car f) 'fifo-bag)))
  (define empty-bag (fifo-init '() '()))
  (define (bag-empty? f) (and (null? (fifo-in f)) (null? (fifo-out f))))
  (define (bag-insert f v) (fifo-init (cons v (fifo-in f)) (fifo-out f)))
  (define (bag-peek f) (if (null? (fifo-out f))
                           (car (reverse (fifo-in f)))
                           (car (fifo-out f))))
  (define (bag-remove f) (if (null? (fifo-out f))
                             (fifo-init '() (cdr (reverse (fifo-in f))))
                             (fifo-init (fifo-in f) (cdr (fifo-out f))))) 
)

;; otwarcie komponentów stosu i kolejki

(define-values/invoke-unit bag-stack@
  (import)
  (export (prefix stack: bag^)))

(define-values/invoke-unit bag-fifo@
  (import)
  (export (prefix fifo: bag^)))

;; testy w Quickchecku
(require quickcheck)

(displayln "Testy dla obu:")

;; testy kolejek i stosów
(define-unit bag-tests@
  (import bag^)
  (export)
  
  ;; test przykładowy: jeśli do pustej struktury dodamy element
  ;; i od razu go usuniemy, wynikowa struktura jest pusta
  (quickcheck
   (property ([s arbitrary-symbol])
             (bag-empty? (bag-remove (bag-insert empty-bag s)))))

  (quickcheck
   (property ([x arbitrary-symbol]
              [y arbitrary-integer])
             (bag-empty? (bag-remove (bag-remove (bag-insert (bag-insert empty-bag x) y))))))

  (quickcheck
   (property ([x arbitrary-integer])
             (eq? x (bag-peek (bag-insert empty-bag x)))))
  
  (quickcheck
   (property ([x arbitrary-integer])
             (bag? (bag-insert empty-bag x))))
)

;; uruchomienie testów dla obu struktur danych

(invoke-unit bag-tests@ (import (prefix stack: bag^)))
(invoke-unit bag-tests@ (import (prefix fifo: bag^)))

(displayln "Testy dla stosu:")

;Testy stosu
(define-unit stack-tests@
  (import bag^)
  (export)

  (quickcheck
   (property ([x arbitrary-integer]
              [y arbitrary-integer])
            (eq? (bag-peek (bag-insert (bag-insert empty-bag x) y)) y)))
  (quickcheck
   (property ([x arbitrary-integer]
             [y arbitrary-integer])
            (eq? (bag-peek (bag-remove (bag-insert (bag-insert empty-bag x) y))) x))) 
)

(invoke-unit stack-tests@ (import (prefix stack: bag^)))

(displayln "Testy kolejki:")

;Testy kolejki
(define-unit fifo-tests@
  (import bag^)
  (export)

  (quickcheck
   (property ([x arbitrary-integer]
              [y arbitrary-integer])
            (eq? (bag-peek (bag-insert (bag-insert empty-bag x) y)) x)))

  (quickcheck
   (property ([x arbitrary-integer]
             [y arbitrary-integer])
            (eq? (bag-peek (bag-remove (bag-insert (bag-insert empty-bag x) y))) y)))
)

(invoke-unit fifo-tests@ (import (prefix fifo: bag^)))

;; otwarcie komponentu grafu
(define-values/invoke-unit/infer simple-graph@)

;; otwarcie komponentów przeszukiwania 
;; w głąb i wszerz
(define-values/invoke-unit graph-search@
  (import graph^ (prefix stack: bag^))
  (export (prefix dfs: graph-search^)))

(define-values/invoke-unit graph-search@
  (import graph^ (prefix fifo: bag^))
  (export (prefix bfs: graph-search^)))

(displayln "Testy grafów")

;; graf testowy
(define test-graph
  (graph
   (list 1 2 3 4)
   (list (edge 1 3)
         (edge 1 2)
         (edge 2 4))))

(define test-graph1
  (graph
   (list 1 2 3 4)
   (list (edge 1 2)
         (edge 2 3)
         (edge 3 4))))

(define test-graph2
  (graph
   (list 1 2 3 4 5 6)
   (list (edge 1 2)
         (edge 1 3)
         (edge 2 1)
         (edge 2 3)
         (edge 2 4)
         (edge 2 5)
         (edge 2 6))))
(define test-graph3
  (graph
   '()
   '()))

;; uruchomienie przeszukiwania na przykładowym grafie
(bfs:search test-graph 1)
(dfs:search test-graph 1)

(bfs:search test-graph1 1)
(dfs:search test-graph1 1)

(bfs:search test-graph2 1)
(dfs:search test-graph2 1)

(bfs:search test-graph3 1)
(dfs:search test-graph3 1)