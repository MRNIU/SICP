#lang racket
(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (product m n)
  (cond
    ((or (= m 0) (= n 0)) 0)
    ((even? n) (product (double m) (halve n)))
    (else (+ m (product m (- n 1))))))