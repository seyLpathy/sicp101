#lang racket
; specify which local version of lisp to load 
(define pi 3.1415926)
(define radius 10)
(* pi (* radius radius))

;define a compund procedure ,function
(define (square x) (* x x))
(square 4)
;(define (<name> <formal parameters) <body>)



