#lang racket

(require "props.rkt")
(provide falsifiable-cnf?)

;;;;;;;;;;Funkcje z ćwiczeń;;;;;;;;;;;;
(define (prop? f)
  (or (var? f)
      (and (neg? f)
           (prop? (neg-subf f)))
      (and (disj? f)
           (prop? (disj-left f))
           (prop? (disj-right f)))
      (and (conj? f)
           (prop? (conj-left f))
           (prop? (conj-right f)))))


(define (free-vars f)
  (define all
    (cond [(var? f) (list f)]
          [(neg? f) (free-vars (neg-subf f))]
          [(disj? f) (append (free-vars (disj-left f)) (free-vars (disj-right f))) ]
          [(conj? f) (append (free-vars (conj-left f)) (free-vars (conj-right f))) ]))
  (remove-duplicates all))

(define(gen-vals xs)
  (if (null? xs)
      (list null)
      (let*
           ((vss (gen-vals (cdr xs)))
            (x ( car xs ))
            (vst (map (lambda (vs) (cons (list x true) vs)) vss))
            (vsf (map (lambda (vs) (cons (list x false) vs)) vss)))
         (append vst vsf))))

(define (eval-formula f vals)
  (define (find-val x vals)
    (if (eq? (caar vals) x) (second (car vals)) (find-val x (cdr vals))))
  (cond [(var? f) (find-val f vals)]
        [(neg? f) (if (eval-formula (neg-subf f) vals) false true)]
        [(conj? f) (and (eval-formula (conj-left f) vals) (eval-formula (conj-right f) vals))]
        [(disj? f) (or (eval-formula (disj-left f) vals) (eval-formula (disj-right f) vals))]))

(define (falsifiable-eval? f)
  (define vals (gen-vals (free-vars f)))
  (define (check-val f vals)
    (if (empty? vals) false
        (if (not (eval-formula f (car vals)))
            (car vals)
            (check-val f (cdr vals)))))
  (check-val f vals))

;;;;;;;;;;;;Konwersja do nnf;;;;;;;;;;;;;;;;;;;;;;;;;
(define (lit? t)
       (or (var? t)
           (and (neg? t) (var? (neg-subf t)))))

(define (nnf? f)
  (cond [(lit? f) true]
        [(conj? f) (and (nnf? (conj-left f)) (nnf? (conj-right f)))]
        [(disj? f) (and (nnf? (disj-left f)) (nnf? (disj-right f)))]
        [else false]))

(define (convert-to-nnf f)
  (cond [(nnf? f) f]
        [(disj? f) (disj (convert-to-nnf (disj-left f)) (convert-to-nnf (disj-right f)))]
        [(conj? f) (conj (convert-to-nnf (conj-left f)) (convert-to-nnf (conj-right f)))]
        [(lit? f) f]
        [(neg? f) (convert-neg-nnf (neg-subf f))]))

(define (convert-neg-nnf f)
  (cond [(neg? f) (neg-subf f)]
        [(conj? f) (disj (convert-to-nnf (neg (conj-left f))) (convert-to-nnf (neg(conj-right f))))]
        [(disj? f) (conj (convert-to-nnf (neg (disj-left f))) (convert-to-nnf (neg(disj-right f))))]))


;;;;;;;;;;;;;;;;Konwersja z nnf do cnf;;;;;;;;;;;;;;;;;;;;;;;;;
(define (clause? x)
   (and (list? x)
         (andmap lit? x)))

(define (cnf? x)
  (and (list? x)
       (andmap clause? x)))

;;Starałem się, żeby funkcja generowała w miarę "czyste" formuły
;;tj. bez duplikatów literałów w klauzuli i bez duplikatów klauzul
(define (nnf-to-cnf f)
  (cond [(lit? f) (list (list f))]
        [(conj? f) (append (nnf-to-cnf (conj-left f)) (nnf-to-cnf (conj-right f)))]
        [(disj? f) (mult-cnf (nnf-to-cnf (disj-left f)) (nnf-to-cnf (disj-right f)))]))

;;"Wymnażanie" każdego nawiasu z każdym z dwóch cnf'ów
(define (mult-cnf ls1 ls2)
  (define (rm-dup xss)
    (if (empty? xss) null
        (cons (remove-duplicates (car xss)) (rm-dup (cdr xss)))))
  (define (mult-aux res l1 l2)
    (cond [(and (null? l1) (null? l2)) null]
          [(null? l2) (mult-aux res (cdr l1) ls2)]
          [(null? l1) res]
          [else (mult-aux (cons (append (car l1) (car l2)) res) l1 (cdr l2))]))
  (remove-duplicates (rm-dup (mult-aux null ls1 ls2))))

(define (eval-cnf f vals)
  (define (find-val x vals)
    (if (eq? (caar vals) x) (second (car vals)) (find-val x (cdr vals))))
  (define (eval-clause cl)
    (if (empty? cl) false
    (let ((x (car cl)))
          (cond [(neg? x) (or (not (find-val (neg-subf x) vals)) (eval-clause (cdr cl)))]
                [else (or (find-val x vals) (eval-clause (cdr cl)))]))))
  (if (empty? f) true
      (if (eval-clause (car f))
          (eval-cnf (cdr f) vals)
          false)))

;;;;;;;;;;;;;;;;;;Właściwa część zadania;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (falsifiable-cnf? f)
  (define nnfed (convert-to-nnf f))
  
  (define cnfed (nnf-to-cnf nnfed))
  
  (define (falsify-clause cl)
    (if (empty? cl) null
        (let ([x (car cl)])
          (cond [(var? x) (cons (list x #f) (falsify-clause (cdr cl)))]
                [else (cons (list (neg-subf x) #t) (falsify-clause (cdr cl)))]))))
  
  (define (ok-vars? vars)
    (let ([clean-vars (map (lambda (xs) (car xs)) vars)])
    (= (length clean-vars) (length (remove-duplicates clean-vars)))))
  
  (define (iter-thru-clauses cnf-formula)
    (if (empty? cnf-formula) false
        (let* ([next-clause (car cnf-formula)]
              [false-vars (falsify-clause next-clause)])
          (if (ok-vars? false-vars) false-vars
              (iter-thru-clauses (cdr cnf-formula))))))
  
  (iter-thru-clauses cnfed))


;;Poniższa funkcja znajduje falsyfikowalne wartościowanie poprzez sprawdzanie każdego po kolei
(define (falsifiable-brute-force f)
  (define generated (gen-vals (free-vars f)))
  (define (iter-through vals formula)
    (cond [(empty? vals) #f]
          [(eval-formula f (car vals)) (iter-through (cdr vals) formula)]
          [else (car vals)]))
  (iter-through generated f))
      
(define formula (conj 'h (disj 'i (disj 'f (conj 't (disj (conj 'x (neg 'y)) (conj (conj 'p 'q) (disj 'r (conj 'o 'p)))))))))

(define formula2 (conj (neg 'e) (disj 'd (disj 'c(conj 'b(disj 'a (conj 'h (disj 'i (disj 'f (conj 't (disj (conj 'x (neg 'y)) (conj (conj 'p 'q) (disj 'r (conj 'o 'p))))))))))))))

(define formula3 (disj 'ac (conj 'ad (conj (neg 'ac) (disj 'ab (conj 'aa (conj (neg 'e) (disj 'd (disj 'c(conj 'b(disj 'a (conj 'h (disj 'i (disj 'f (conj 't (disj (conj 'x (neg 'y)) (conj (conj 'p 'q) (disj 'r (conj 'o 'p)))))))))))))))))))

;;Przykładowe porównania:

(time (falsifiable-brute-force formula))

(time (falsifiable-cnf? formula))

(time (falsifiable-brute-force formula2))

(time (falsifiable-cnf? formula2))

(time (falsifiable-brute-force formula3))

(time (falsifiable-cnf? formula3))

;;Nie udało mi się osiągnąć konsekwentnych wyników w środowisku DrRacket,
;;aczkolwiek uruchamiając program bezpośrednio z terminala, w uśrednieniu, formuła
;;posiadająca co najwyżej 10 zmiennych była falsyfikowana w bardzo podobnym czasie przez
;;oba algorytmy. Przy 15 zmiennych, algorytm generujący wszystkie wartościowania
;;okazywał się mniej więcej 5 razy wolniejszy, natomiast przy 20 zmiennych obliczał się
;;już około 1800 razy wolniej (w stosunku do drugiego).












  
















