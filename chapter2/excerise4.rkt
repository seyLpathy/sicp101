#lang racket
(require sicp-pict)
(provide "sub-vect")
(define einstein2 (beside einstein (flip-vert einstein)))
;(paint einstein2)
;right-split painter
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
            (beside painter (below smaller smaller)))))
;excerise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1))))
            (below painter (beside up up)))))
;(paint (right-split einstein 5))
;(paint (up-split einstein 5))

;excerise 2.45 right-split implements by split
(define (split p1 p2)
  (define (split-aid painter n)
    (if (= n 0) painter (let ((extension (split-aid painter (- n 1)))) (p1 painter (p2 extension extension))
  )))
  (lambda (painter n) (split-aid painter n)))

(define right-split-v1 (split beside below))
(define up-split-v1 (split below beside))
;(paint (right-split-v1 einstein 5))
;(paint (up-split-v1 einstein 5))
(define (corner-split painter n)
(if (= n 0)
painter
(let ((up (up-split painter (- n 1)))
(right (right-split painter (- n 1))))
(let ((top-left (beside up up))
(bottom-right (below right right))
(corner (corner-split painter (- n 1))))
(beside (below painter top-left)
(below bottom-right corner))))))
;(paint (corner-split einstein 5))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
;two level abstraction
;one the square pattern tl br bl br
;second every piece of the square is also a pattern


;excerise 2.46 and 2.47
;define frame by list or cons
;the abstraction is the same
;((1 2) (2 3) (4 5))
(define (make-frame origin edge1 edge2) (list origin edge1 edge2) )
(define (make-frame-v1 origin edge1 edge2) (cons origin (cons edge1 edge2)))
(define (make-vect x y) (list x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))
(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2)) ))
(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2)) ))
(define (scale-vect s v) (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))
(define (edge-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))
(define (origin-frame frame) (car frame))
(define (edge-frame-v1 frame) (cadr frame))
(define (edge2-frame-v1 frame) (caddr frame))
(define (origin-frame-v1 frame) (car frame))
(define (frame-coord-map frame) 
  (lambda (v)
  (add-vect 
    (origin-frame frame)
    (add-vect (scale-vect (xcor-vect v) (edge-frame frame)) (scale-vect (ycor-vect v) (edge2-frame frame)))
    )
  )
)
(define A (make-vect 1 2))
(define B (make-vect 3 4))
(define C (make-vect 5 6))

(define test-frame (make-frame A B C))
;((1 2) (3 4) (5 6))
(define test-frame-v1 (make-frame-v1 A B C))
;((1 2) (3 4) 5 6)
(origin-frame test-frame)
(edge-frame test-frame)
(edge2-frame test-frame)
(origin-frame-v1 test-frame)
(edge-frame-v1 test-frame)
(edge2-frame-v1 test-frame)

;excerise 2.48
(require sicp-pict)

;(paint (vects->painter (list (vect 0. 0.) (vect 1. 1.) (vect 1. 0.))))
;(paint (segments->painter (list (segment (vect 0. 0.) (vect 1. 1.))
 ;                              (segment (vect 0. 1.) (vect 1. 0.)))))
(define (make-segment start end) (list start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cadr seg))

;excerise 2.49
;; 用你自己 2.46/2.47 的 list 表示
(define outline
  (segments->painter
   (list (segment (vect 0 0) (vect 1 0))
         (segment (vect 1 0) (vect 1 1))
         (segment (vect 1 1) (vect 0 1))
         (segment (vect 0 1) (vect 0 0)))))
;(paint outline)
(paint (vects->painter (list (vect 0. 0.5) (vect 0.5 1.) (vect 1. 0.5) (vect 0.5 0.) (vect 0. 0.5))))


