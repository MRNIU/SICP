#lang racket
(define (determine-result a n)
  (cond ((and (not (= a 1))
              (not (= a (- n 1)))
              (= (remainder (square a) n) 1)) 0)
        (else (remainder (square a) n))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (determine-result (expmod base (/ exp 2) m) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
        (else #f)))

(define (square n)
  (* n n))