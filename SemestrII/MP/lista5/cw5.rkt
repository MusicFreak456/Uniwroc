#lang racket


(define (var? t)
  (symbol? t))

(define (neg? t)
  (and (list? t)
       (= 2 (length t))
       (eq? 'neg (car t))))

(define (neg f)
  (list 'neg f))

(define (neg-subf f)
  (second f))

(define (conj? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'conj (car t))))

(define (conj lf rf)
  (list 'conj lf rf))

(define (conj-left f)
  (second f))

(define (conj-right f)
  (third f))

(define (disj? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'disj (car t))))

(define (disj lf rf)
  (list 'disj lf rf))

(define (disj-left f)
  (second f))

(define (disj-right f)
  (third f))

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


(define (lit? t)
       (or (var? t)
           (and (neg? t) (var? (neg-subf t)))))

(define (nnf? f)
  (cond [(lit? f) true]
        [(conj? f) (and (nnf? (conj-left f)) (nnf? (conj-right f)))]
        [(disj? f) (and (nnf? (disj-left f)) (nnf? (disj-right f)))]
        [else false]))

(define nnf-formula (conj 'x (disj (conj (neg 'x) 'y) 'z)))
(nnf? nnf-formula)
(define not-nnf-formula (conj (neg 'x) (neg (disj 'y 'z))))
(nnf? not-nnf-formula)
        





  
















