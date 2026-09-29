#lang racket
(provide better-enough)
(provide better-enough)
;; newton's iteration 
(define tolerance 0.0001)

(define (average a b) (/ (+ a b) 2))

(define (good-enough guess x)
    (< (abs (- (* guess guess) x)) tolerance))

(define (better-enough guess x)
    (< (abs (- guess x)) (* 0.000001 guess)))
;if the change is smaller than 0.0001% of guess in one iteration,then the precision is tolerable
(define (next-guess guess x) 
    (average guess (/ x guess)))

(define (better-sqrt-iter guess x)
    (if (better-enough guess (next-guess guess x))
    guess
    (better-sqrt-iter (next-guess guess x) x)))

(define (sqrt-iter guess x)
    (if (good-enough guess x)
    guess
    (sqrt-iter (next-guess guess x) x)))

;(sqrt-iter 3.0 2)
;(better-sqrt-iter 1.0 2)