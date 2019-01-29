#lang racket
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((fast-prime? n 3) (report-prime (- (current-inexact-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start count)
  (cond ((= count 0) (newline))
        ((timed-prime-test start) (search-for-primes (next-odd start) (- count 1)))
        (else (search-for-primes (next-odd start) count))))

(define (next-odd n)
  (cond ((even? n) (+ n 1))
        (else (+ n 2))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        (fermat-test n) (fast-prime? n (- times 1))
        (else #f)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))
  