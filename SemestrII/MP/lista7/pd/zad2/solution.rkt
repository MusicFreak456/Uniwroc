#lang racket

;Wspólnie debugowane z Dawidem Dudkiem

(provide (struct-out const) (struct-out binop) (struct-out var-expr) (struct-out let-expr) (struct-out var-dead) find-dead-vars)

; --------- ;
; Wyrazenia ;
; --------- ;

(struct const    (val)      #:transparent)
(struct binop    (op l r)   #:transparent)
(struct var-expr (id)       #:transparent)
(struct var-dead (id)       #:transparent)
(struct let-expr (id e1 e2) #:transparent)

(define (expr? e)
  (match e
    [(const n) (number? n)]
    [(binop op l r) (and (symbol? op) (expr? l) (expr? r))]
    [(var-expr x) (symbol? x)]
    [(var-dead x) (symbol? x)]
    [(let-expr x e1 e2) (and (symbol? x) (expr? e1) (expr? e2))]
    [_ false]))

(define (parse q)
  (cond
    [(number? q) (const q)]
    [(symbol? q) (var-expr q)]
    [(and (list? q) (eq? (length q) 3) (eq? (first q) 'let))
     (let-expr (first (second q))
               (parse (second (second q)))
               (parse (third q)))]
    [(and (list? q) (eq? (length q) 3) (symbol? (first q)))
     (binop (first q)
            (parse (second q))
            (parse (third q)))]))
;----------------;
; Środowisko     ;
;----------------;

(define empty-environment (set))

(define (add-to-environment x env) (set-add env x))

(define (lookup-env x env) (set-member? env x))

;--------------------------;
; Wolne zmienne z wykładu  ; 
;--------------------------;

(define (free-vars e)
  (define (free-vars-env e env)
    (match e
      [(const n) (set)]
      [(binop op l r)
       (set-union (free-vars-env l env)
                  (free-vars-env r env))]
      [(let-expr x e1 e2)
       (set-union (free-vars-env e1 env)
                  (free-vars-env e2 (add-to-environment x env)))]
      [(var-expr x)
       (if (lookup-env x env)
           (set) (list->set (list x)))]))
  (free-vars-env e empty-environment))

; ---------------------------------- ;
; Wyszukaj ostatnie uzycie zmiennych ;
; ---------------------------------- ;

(define (find-dead-vars e)
  (define (find-dead-env e env)
    (match e
      [(const n) e]
      [(var-expr id) (if (lookup-env id env) e (var-dead id))]
      [(binop op l r) (let ([free-var-env (set-union env (free-vars r))])
                        (binop op
                               (find-dead-env l free-var-env)
                               (find-dead-env r env)))]
      [(let-expr x e1 e2) (let ([free-var-env (set-union env (set-remove (free-vars e2) x))])
       (let-expr x (find-dead-env e1 free-var-env) (find-dead-env e2 (set-remove env x))))]
      ))
  (find-dead-env e empty-environment))


;;Mini testy
(define test1 (let-expr 'x (const 3) (binop '+ (var-expr 'x) (var-expr 'x))))
test1
(find-dead-vars test1)

(newline)
(define test2 (let-expr 'x (const 3)
                  (binop '+ (var-expr 'x )
                          (let-expr 'x (const 5)
                               (binop '+ (var-expr 'x )
                                       (var-expr 'x ))))))
test2
(find-dead-vars test2)

(newline)
(newline)
; Testy pożyczone od Pawła Zmarzłego

; Przykład z dużą liczbą let'ów.
(find-dead-vars (parse '(let [x 1] (let [x x] (+ 5 x)))))

; Przykład z większą liczbą let'ów i zmiennych.
(find-dead-vars (parse '(let [x 1] (let [y x] (let [x (+ x y)] (+ x y))))))

; Przykład z nieużywaną zmienną.
(find-dead-vars (parse '(let [x 1] (let [y x] (+ x x)))))

; Przykład, w którym szukając ostatniego wystąpienia zmiennej x
; napotykamy (var-dead 'y).
(find-dead-vars (parse '(let [x 1] (let [y x] (+ x y)))))



; Świetny test pożyczony od Dawida Dudka

(find-dead-vars (parse '(let (y 5) (+ (- y (let (y 3) y)) (* y y)))))

; Tutaj z małą modyfikacją komplikującą sprawy
(find-dead-vars (parse '(let (y 5) (+ (- y (let (y y) y)) (* y y)))))

