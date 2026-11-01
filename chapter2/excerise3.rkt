#lang racket
(require "excerise2.rkt")
(require "../chapter1/excerises.rkt")
(define (enumerate-interval x y) (if (> x y) null (cons x (enumerate-interval (+ x 1) y))))
(enumerate-interval 1 8)
(define (make-pair n)
;debug 
(accumulate append null (map (lambda (i) (map (lambda (j) (list i j))
(enumerate-interval 1 (- i 1))))
(enumerate-interval 1 n))))
;(make-pair 8)
(define (flatmap proc seq)
(accumulate append null (map proc seq)))
;excerise 2.40
(define (make-pair-sum pair) (list (car pair) (cdr pair) (+ (car pair) (cdr pair))))
(define (prime-sum? x) (prime? (+ (car x) (cdr x))))
(define (prime-sum-pairs n)
    (map make-pair-sum (filter prime-sum? (make-pair n))))

;excerise 2.41
(define (order-triple-equality n s)
  (filter (lambda (x) (= s (accumulate + 0 x))) (make-triple n)))
  

;need to understanding only remove one level parenthess
(define (make-triple n)
          (accumulate append null 
            (map (lambda (i) 
                (accumulate append null
                    (map (lambda (j) 
                        (map (lambda (k) (list i j k))
                            (enumerate-interval 1 (- j 1))))      
                    (enumerate-interval 1 (- i 1)))))
            (enumerate-interval 1 n))))

;(make-triple 8)
(order-triple-equality 8 10)
