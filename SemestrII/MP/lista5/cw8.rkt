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
(define not-nnf-formula (conj (neg 'x) (neg (disj 'y 'z))))

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

(define converted-formula (convert-to-nnf not-nnf-formula))

(define (clause? x)
   (and (list? x)
         (andmap lit? x)))

(define (cnf? x)
  (and (list? x)
       (andmap clause? x)))

(define cnf-formula (conj (disj 'x 'y) (disj (neg 'x) (neg 'y))))

;;Interesująca część:

(define (nnf-to-cnf f)
  (cond [(lit? f) (list (list f))]
        [(conj? f) (append (nnf-to-cnf (conj-left f)) (nnf-to-cnf (conj-right f)))]
        [(disj? f) (mult-cnf (nnf-to-cnf (disj-left f)) (nnf-to-cnf (disj-right f)))]))

(define (mult-cnf ls1 ls2);;Wymnażanie w przypadku P v Q
  (define (rm-dup xss)
    (if (empty? xss) null
        (cons (remove-duplicates (car xss)) (rm-dup (cdr xss)))))
  (define (mult-aux res l1 l2)
    (cond [(and (null? l1) (null? l2)) null]
          [(null? l2) (mult-aux res (cdr l1) ls2)]
          [(null? l1) res]
          [else (mult-aux (cons (append (car l1) (car l2)) res) l1 (cdr l2))]))
  (remove-duplicates (rm-dup (mult-aux null ls1 ls2))))

(define cnf-exe (list (list 'x 'y 'z) (list (neg 'x) 'y))) 
(define cnf-exe2 (list (list 'x 'y) (list 'y)))
(mult-cnf cnf-exe cnf-exe2);;Sprawdzenie czy wymnażanie działa

cnf-formula
converted-formula
(nnf-to-cnf converted-formula)
(cnf? (nnf-to-cnf cnf-formula))
(cnf? (nnf-to-cnf converted-formula))

(define test (disj (disj (conj 'x 'y) 'z) (neg 'x)))
test
(define test-vals (gen-vals (free-vars test)))
test-vals
(define cnfed (nnf-to-cnf test))
(cnf? cnfed)
cnfed;;to jest to samo co było, sprawdziłem

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

(eval-cnf cnfed (car test-vals))
(define false-vars '((x #t) (y #f) (z #f)))
(eval-cnf cnfed false-vars)
(define true-vars '((x #t) (y #f) (z #t)))
(eval-cnf cnfed true-vars)







  
















