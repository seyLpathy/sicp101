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

(count-change-iter 100)

;excerise1.11
;recursive 
(define (f n) 
    (cond ((< n 3) n)
        ((>= n 3) (+ (+ (f (- n 1)) (* 2 (f (- n 2)))) (* 3 (f (- n 3)))))
    ))

(f 5)
;iterative
(define (f-v2 n)
    (f-iter 0 1 2 n)
)
(define (f-iter a b c n)
        (cond ((< n 3) n)
                ((= n 3) (+ c (+ (* 2 b) (* 3 a))))
                (else (f-iter b c (+ c (+ (+ (* 2 b) (* 3 a)))) (- n 1))))
)
(f-v2 5)

;excerise 1.12                                     
;todo need furthering working for this (maybe using two-dimensions arrays)

;excerise 1.13
;math problem ,induction 

;excerise 

