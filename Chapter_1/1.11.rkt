#lang racket
(define (f-rec n)
  (if (< n 3) n
      (+
       (f-rec (- n 1))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))
  
(define (f-iter a b c n)
  (if (= n 0) c
      (f-iter (+
               a
               (* 2 b)
               (* 3 c))
              a
              b
              (- n 1))))

(define (f2 n)
  (f-iter 2 1 0 n))