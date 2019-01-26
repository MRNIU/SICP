#lang racket
(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (product-iter m n curr)
  (cond
    ((or (= n 0) (= m 0)) curr)
    ((even? n) (product-iter (double m) (halve n) curr))
    (else (product-iter (+ m curr) (- m 1) (- n 1)))))

(define (product m n)
  (product-iter m n 0))