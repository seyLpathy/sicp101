#lang racket 
(require "square.rkt")
(define (new-if predicate then-clause else-clause)
        (cond (predicate then-clause)
                (else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define (new-sqrt-iter guess x)
    (new-if (good-enough guess x)
    guess
    (new-sqrt-iter (next-guess guess x) x)))

(new-sqrt-iter 3.0 2)
;Racket是应用序求值，调用一个函数时，所有实参都必须先算出来才能进入函数体