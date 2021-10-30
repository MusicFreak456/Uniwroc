#lang racket

(require racket/contract)

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (close-to? x y)
  (< (abs (- x y)) 0.00001))

(define (average x y)
  (/ (+ x y) 2))

;; obliczenie punktu stałego funkcji rzeczywistej przez iterację (może się zapętlić)
(define (fixed-point s f)
  (if (close-to? s (f s))
      s
      (fixed-point (f s) f)))

(let ((fp (fixed-point 1.0 cos)))
  ;; display/displayln pozwalają nam wypisać wartości na ekran
  (display "Punkt stały funkcji cosinus w okolicy 1: ")
  (displayln fp)
  (close-to? fp (cos fp)))

;; żeby znaleźć wartość pierwiastka z dwóch należy rozwiązać równanie 2 = x * x
;; można myśleć że wystarczy w tym celu znaleźć punkt stały transformacji x ↦ 2 / x
;; ale obliczenie (fixed-point 1.0 (lambda (x) (/ 2 x))) zapętla się:
;; oscylujemy między wartościami 1 i 2

;; NB: tu zrobiliśmy błąd na wykładzie: wzięliśmy funkcję x ↦ 2 / (x * x) i
;; obliczyliśmy (poprawnie) wartość pierwiastka sześciennego z 2

;; żeby przerwać oscylację, możemy zastosować tłumienie z uśrednianiem:
(define (average-damp f)
  (lambda (x)
    (average x (f x))))

;; teraz możemy obliczyć wartość pierwiastka z dwóch:
(let* ((transform (lambda (x) (/ 2 x)))
       (fp (fixed-point 1.0 (average-damp transform))))
  (display "Przybliżona wartość pierwiastka z dwóch: ")
  (displayln fp)
  (close-to? 2 (* fp fp)))

;; w ten sposób możemy zdefiniować również pierwiastkowanie jako procedurę:
;; zwróć uwagę na zmienną x, której wolne wystąpienie w definicji operacji
;; (która sama jest argumentem average-damp) związane jest w definicji sqrt!

(define (sqrt? x)
  (lambda (res) (close-to? (* res res) x)))

(define sqrt/c
  (->i ([x number?])
        [result (x) (sqrt? x)]))

(define/contract (sqrt x)
  sqrt/c
  (let ((op (lambda (y) (/ x y))))
    (fixed-point 1.0 (average-damp op))))
(sqrt 4)



