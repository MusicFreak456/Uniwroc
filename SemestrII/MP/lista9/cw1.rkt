#lang racket

(require racket/mpair)

(define test (mlist 1 2 3 4 5))
test

(define (mreverse! mxs)
  (if (or (null? mxs) (null? (mcdr mxs))) mxs
      (let ([tail (mreverse! (mcdr mxs))])
        (begin
           (set-mcdr! (mcdr mxs) mxs)
           (set-mcdr! mxs null)
           tail))))



(mreverse! test)
test

  
                    