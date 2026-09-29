#lang racket
(define (sumofsquare x y)
    (+ (* x x) (* y y)))
(define (>= x y) (or (> x y) (= x y)))
(define (squareSumofThree x y z)
    (cond ((and (>= x y) (>= z y)) (sumofsquare x z))
        ((and (>= y z) (>= x z)) (sumofsquare y x))
        (else (sumofsquare z y))))

;test the function 
(squareSumofThree 3 4 5)
(squareSumofThree 5 3 4)       
(squareSumofThree 4 5 3)       
