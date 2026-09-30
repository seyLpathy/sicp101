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