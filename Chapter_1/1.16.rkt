#lang racket
(define (expt base n)
  (expt-iter base n 1))

(define (expt-iter base n curr)
  (cond
    ((= n 0) curr)
    ((even? n) (expt-iter (* base base) (/ n 2) curr))
    (else (expt-iter base (- n 1) (* curr base)))))