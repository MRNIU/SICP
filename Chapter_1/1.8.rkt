#lang racket
(define (square x)
  (* x x))

(define (improve x y)
  (/ (+ (/ x (square y)) (* 2 y)) 3))
  
(define (good-enough? x y)
  (< (/ (abs (- (improve x y) y)) y) 0.000001))

(define (cube-root x y)
  (cond ((good-enough? x y) y)
        (else (cube-root x (improve x y)))))

; > (cube-root (* 5 5 5) 1.0)
; > 5.000000000287929