#lang racket
; 以三个数为参数，返回较大的两个数之和
(define (sum-of-max-two x y z)
  (cond ((and (< x y) (< x z)) (+ y z))
        ((and (< y x) (< y z)) (+ x z))
        ((and (< z x) (< z y)) (+ x y))
        (else `error!)))