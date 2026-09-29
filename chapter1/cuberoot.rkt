#lang racket 
(require "square.rkt")
(define (cube-guess guess x) (/ (+ (/ x (* guess guess)) (* 2 guess)) 3 ))
(define (cube-iter guess x) 
    (if (better-enough guess (cube-guess guess x))
        guess
        (cube-iter (cube-guess guess x) x)) )
(cube-iter 4.0 27)