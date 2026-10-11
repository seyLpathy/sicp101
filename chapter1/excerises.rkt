#lang racket
(define (add-v1 a b)
    (if (= a 0)
        b
        (inc (+ (dec a) b))))
;next form
(define (add-v2 a b)
    (if (= a 0)
        b
        (add-v2 (dec a) (inc b))))

(define (inc a) (+ a 1))
(define (dec a) (- a 1))


(add-v1 3 5)
(add-v2 3 5)

;excerise 1.10
(define (A x y)
    (cond ((= y 0) 0)
            ((= x 0) (* 2 y))
            ((= y 1) 2)
            (else (A (- x 1)
                (A x (- y 1))))))

;(A 1 10)
;(A 2 4)
;(A 3 3)
;from bottom to the top

(define (g n) (A 1 n))
;2^n
(define (h n) (A 2 n))
;2^n^n

;fibonacci with iteration
(define (fib n)
    (fib-iter 1 0 n))

(define (fib-iter a b count)
        (if (= count 0)
            b
            (fib-iter (+ a b) a (- count 1))))

(fib 6)

; tree recursive for counting changes
(define (first_deminator coins) 
    (cond ((= coins 1) 50)
            ((= coins 2) 25)
            ((= coins 3) 10)
            ((= coins 4) 5)
            ((= coins 5) 1)
    ))

(define (count-change amount)
    (cc amount 5))

(define (cc amount kind-of-coins)
    (cond ((= amount 0) 1)
        ((or (< amount 0) (= kind-of-coins 0)) 0)
        (else (+ (cc amount (- kind-of-coins 1))
                    (cc (- amount (first_deminator kind-of-coins)) kind-of-coins)))
    ))

; (count-change 100)


;iterative algorithms for counting 
(define (count-change-iter amount)
    (define coins '(1 5 10 25 50))
    (let ([ways (make-vector (+ amount 1) 0)])
    (vector-set! ways 0 1)
    (let loop-coins ([cs coins])
        (cond
        [(null? cs) (vector-ref ways amount)]
        [else
        (let loop-amount ([a (car cs)])
            (when (<= a amount)
                (vector-set! ways a
                            (+ (vector-ref ways a)
                               (vector-ref ways (- a (car cs)))))
                (loop-amount (+ a 1))))
        (loop-coins (cdr cs))
        ]))))

;(count-change-iter 100)

;excerise1.11
;recursive 
(define (f n) 
    (cond ((< n 3) n)
        ((>= n 3) (+ (+ (f (- n 1)) (* 2 (f (- n 2)))) (* 3 (f (- n 3)))))
    ))

;(f 5)
;iterative
(define (f-v2 n)
    (f-iter 0 1 2 n)
)
(define (f-iter a b c n)
        (cond ((< n 3) n)
                ((= n 3) (+ c (+ (* 2 b) (* 3 a))))
                (else (f-iter b c (+ c (+ (+ (* 2 b) (* 3 a)))) (- n 1))))
)
;(f-v2 5)

;excerise 1.12                                     
;todo need furthering working for this (maybe using two-dimensions arrays)

;excerise 1.13
;math problem ,induction 

;excerise 

;great common divisor by euclid's algorithm
;增长速度对数级别
(define (GCD x y)
    (if (= y 0) x (GCD y (remainder x y))))
;(GCD 28 16)

;excerise1.20
;normal-order evaluation won't transfer any value to the applicated procedure

(define (small-divisor n)
    (find-divisor n 2))

(define (small-divisor-v2 n)
    (find-divisor-v2 n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

;1.23 improve the iteration with divisor
(define (next input) (if (= input 2) 3 (+ input 2)))

;1.24
; check whether a power n remainder n is equal to n
(define (expmod base exp m)
(cond ((= exp 0) 1)
((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
(else (remainder (* base (expmod base (- exp 1) m)) m))))

;using a random n smaller than n to test whether expmod works 
(define (fermat-test n)
(define (try-it a)
(= (expmod a n n) a))
(try-it (+ 1 (random (- n 1)))))

;return true only if n times fermat test always true
(define (fast-prime? n times)
(cond ((= times 0) true)
((fermat-test n) (fast-prime? n (- times 1)))
(else false)))

(define (find-divisor-v2 n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-v2 n (next test-divisor)))))
(define (divides? a b)
    (= (remainder b a) 0))
(define (square x) (* x x))
(define (prime? x) (= x (small-divisor x)))
(define (prime-v2? x) (= x (small-divisor-v2 x)))
;excerise 1.21
;(prime? 199)
;(prime? 1999)
;(prime? 19999)                    
;simple test for prime function

;excerise 1.22
(define (timed-prime-test n)
    (start-prime-test n (current-inexact-milliseconds)))

(define (timed-prime-test-v2 n)
    (start-prime-test-v2 n (current-inexact-milliseconds)))

(define (timed-prime-test-fast n)
    (start-prime-test-fast n (current-inexact-milliseconds)))

(define (start-prime-test-fast n start-time)
    (when (fast-prime? n 50)
        (newline)
        (display n)
        (report-prime (- (current-inexact-milliseconds) start-time))))
(define (start-prime-test n start-time)
    (when (prime? n)
        (newline)
        (display n)
        (report-prime (- (current-inexact-milliseconds) start-time))))

(define (start-prime-test-v2 n start-time)
    (when (prime-v2? n)
        (newline)
        (display n)
        (report-prime (- (current-inexact-milliseconds) start-time))))
(define (report-prime elapsed-time)
    (display " *** ")
    (display (real->decimal-string elapsed-time 4)))

;checks the primality of consecutive odd integers in the range [start, end]
(define (search-for-primes start end)
    (define from (if (even? start) (+ start 1) start))
    (let loop ([n from])
        (when (<= n end)
            (timed-prime-test n)
            (loop (+ n 2)))))

(define (search-for-primes-v2 start end)
    (define from (if (even? start) (+ start 1) start))
    (let loop ([n from])
        (when (<= n end)
            (timed-prime-test-v2 n)
            (loop (+ n 2)))))

(define (search-for-primes-fast start end)
    (define from (if (even? start) (+ start 1) start))
    (let loop ([n from])
        (when (<= n end)
            (timed-prime-test-fast n)
            (loop (+ n 2)))))

;(search-for-primes 1000 1022)
;(search-for-primes 10000 10040)
;(search-for-primes 100000 100050)
;(search-for-primes 1000000 1000040)
;(search-for-primes 10000000 10000122)
;(search-for-primes-v2 10000000 10000122)
;(search-for-primes-fast 100000 100050)
;(search-for-primes-fast 1000000 1000040)
;(search-for-primes-fast 10000000 10000122)

;excerise1.25 
;把remainder移动至计算的最后一步

;excerise1.26 not using square function means that you have to when power is even ,you actually calculating the splited part twice ,which means you restore logn to n

;excerise1.27  pick a even number that could pass the fermat test with given number  like 561, 1105, 1729, 2465, 2821, and 6601
;extreme density of credit numbers could be used as probability test
(define (fermat-test-exception n) (= (expmod 561 n n) 561))
(fermat-test-exception 8)


;excerise1.28
;todo pure math problem,need further work
(define (expmod-v2 base exp m)
(cond ((= exp 0) 1)
((even? exp) 
(let ([r (expmod-v2 base (/ exp 2) m)])
    (if (and (not (= r 1)) (not (= r (- m 1))) 
    (= (remainder (square r) m) 1))
    0
    (remainder (square r) m))))
(else (remainder (* base (expmod-v2 base (- exp 1) m)) m))))

(define (miller-rabin-test n) 
    (define (try-it a) (= (expmod-v2 a (- n 1) n) 1))
    (try-it (+ 1 (random (- n 1)))))
(miller-rabin-test 347)



