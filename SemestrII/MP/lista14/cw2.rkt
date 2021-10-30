#lang racket

(require racklog)

;; predykat unarny %male reprezentuje zbiór mężczyzn
(define %male
  (%rel ()
        [('adam)]
        [('john)]
        [('joshua)]
        [('mark)]
        [('david)]))

;; predykat unarny %female reprezentuje zbiór kobiet
(define %female
  (%rel ()
        [('eve)]
        [('helen)]
        [('ivonne)]
        [('anna)]))

;; predykat binarny %parent reprezentuje relację bycia rodzicem
(define %parent
  (%rel ()
        [('adam 'helen)]
        [('adam 'ivonne)]
        [('adam 'anna)]
        [('eve 'helen)]
        [('eve 'ivonne)]
        [('eve 'anna)]
        [('john 'joshua)]
        [('helen 'joshua)]
        [('ivonne 'david)]
        [('mark 'david)]))

;; predykat binarny %sibling reprezentuje relację bycia rodzeństwem
(define %sibling
  (%rel (a b c)
        [(a b)
         (%parent c a)
         (%parent c b)]))

;; predykat binarny %sister reprezentuje relację bycia siostrą
(define %sister
  (%rel (a b)
        [(a b)
         (%sibling a b)
         (%female a)]))

;; predykat binarny %ancestor reprezentuje relację bycia przodkiem
(define %ancestor
  (%rel (a b c)
        [(a b)
         (%parent a b)]
        [(a b)
         (%parent a c)
         (%ancestor c b)]))

(define %grandson
  (%rel (a b c)
        [(a b)
         (%male a)
         (%parent b c)
         (%parent c a)]))

(define %cousin
  (%rel (a b c d)
        [(a b)
         (%parent c a)
         (%parent d b)
         (%sibling c d)]))

(define %is_mother
  (%rel (a b)
        [(a)
         (%female a)
         (%parent a b)]))

(define %is_father
  (%rel (a b)
        [(a)
         (%male a)
         (%parent a b)]))

(%find-all () (%ancestor 'mark 'john))

(%find-all (x) (%ancestor 'adam x))

(%find-all (x) (%sister 'ivonne x))

(%find-all (x y) (%cousin x y))

