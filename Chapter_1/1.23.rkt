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
  (cond ((prime? n) (report-prime (- (current-inexact-milliseconds) start-time)))))

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

; 原因：(next n) 调用开销