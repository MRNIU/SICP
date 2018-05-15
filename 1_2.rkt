#lang racket
(define (inc a)
  (+ a 1))

(define (dec a)
  (- a 1))

(define (A x y)
  (cond ((= y 0) 0)
        ((= y x) (* 2 y))
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;1.11
(define (fun n)
  (fun-iter 0 1 2 n))

(define (fun-rec n)
  (cond ((< n 3) n)
        (else (+
               (fun-rec (- n 1))
               (* 2 (fun-rec (- n 2)))
               (* 3 (fun-rec (- n 3)))))))
       
(define (fun-iter a b c n)
  (cond ((= n 0) a)
        ((= n 1) b)
        ((= n 2) c)
        (else (fun-iter b c (+ (* 3 a) (* 2 b) c) (- n 1)))))


;1.12
(define (pascal r c)
  (pascal-rec r c))

(define (pascal-rec r c)
  (cond ((> c r) (error "unvalid col value !"))
        ((or (= r c) (= c 0)) 1)
        (else (+
               (pascal-rec (- r 1) (- c 1))
               (pascal-rec (- r 1) c)))))

;1.15
(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1)) angle
      (p (sine (/ angle 3.0)))))

;1.16
(define (expt b n)
  (expt-iter b n 1))

(define (expt-rec b n)
  (cond ((= n 0) 1)
        (else (* b (expt-rec b (- n 1))))))

(define (expt-iter b n result)
  (if (= n 0) result
      (expt-iter b (- n 1) (* b result))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        (( even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square x)
  (* x x))

(define (_fast-expt b n)
    (_expt-iter b n 1))

(define (_expt-iter b n a)
  (cond ((= n 0)
         a)
        ((even? n)
         (expt-iter (square b)
                    (/ n 2)
                    a))
        ((odd? n)
         (expt-iter b
                    (- n 1)
                    (* b a)))))

(define (_* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))


;1.17
(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (multi a n)
  (cond ((= n 0)
         0)
        ((even? n)
         (double (multi a (halve n))))
        ((odd? n)
         (+ a (multi a (- n 1))))))

;1.18
(define (multiiter a b)
  (multi-iter a b 0))

(define (multi-iter a b product)
  (cond ((= b 0) 0)
        ((even? b)
         ((multi-iter (double a) (halve b) product)))
        ((odd? b)
         ((multi-iter a (- b 1) (+ a product))))))


;1.19
;此题来源 https://book.douban.com/annotation/17994049/
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter
          a
          b
          (+ (square p) (square q))     ; 计算 p'
          (+ (* 2 p q) (square q))      ; 计算 q'
          (/ count 2)))
        (else (fib-iter
               (+ (* b q)
                  (* a q)
                  (* a p))
               (+ (* b p)
                  (* a q))
                  p
                  q
                  (- count 1)))))

;1.20
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  

;素数检测
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;费马检查
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))
         
;1.21
;(smallest-divisor 199) ;199
;(smallest-divisor 1999) ;1999
;(smallest-divisor 19999) ;7

;1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
  #f))
;使用 current-inexact-milliseconds 替换 runtime
;使用 #f 代替 else

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; 生成连续奇数
(define (next-odd n)
  (if (odd? n)
      (+ n 2)
      (+ n 1)))

; 生成从 start 开始的连续 count 个素数
(define (continue-primes start count)
  (cond ((= count 0) (display "are primes."))
        ((prime? start)
         (display start)
         (newline)
         (continue-primes (next-odd start) (- count 1)))
        (else
         (continue-primes (next-odd start) count))))

(define (search-for-primes n)
  (continue-primes n 3)
  (newline))

(define (search-for-primes- start count)
  (cond ((= count 0) (newline))
        ((timed-prime-test start) (search-for-primes- (next-odd start) (- count 1)))
        (else (search-for-primes- (next-odd start) count))))


;1.23
; 调用 next 过程会造成时间消耗，虽然步数少了一半，但因为调用的存在运行时间并不是减少两倍，
; 将 next 过程换成表达式后可以发现，运行时间确实减半.
(define (find-divisor-pro n test)
  (cond ((> (* test test) n) n)
        ((divides? n test) test)
        (else (find-divisor-pro n (+ test 2))))) ; 直接加2,不再调用next过程

(define (smallest-divisor-pro n)
  (find-divisor-pro n 3)) ;; 直接从3开始

(define (prime?-pro n)
  (= n (smallest-divisor-pro n)))

;1.24
(define (timed-fast-prime-test n)
  (newline)
  (display n)
  (start-fast-prime-test n (current-inexact-milliseconds)))

(define (start-fast-prime-test n start-time)
  (if (fast-prime? n 3)
      (report-prime (- (current-inexact-milliseconds) start-time))
  #f))

;1.25
; 见脚注 46 p34

;1.26
; 显式使用乘法，expmod 执行两次递归调用
; 原本的 expmod 在每次 exp 为偶数时,可以将计算量减少一半

;1.27