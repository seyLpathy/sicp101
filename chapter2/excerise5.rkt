#lang racket
(require (except-in sicp-pict
                    transform-painter flip-horiz flip-vert
                    rotate90 rotate180 rotate270))

(define (sub-vect v1 v2)
  (vect (- (vector-xcor v1) (vector-xcor v2))
        (- (vector-ycor v1) (vector-ycor v2))))                    
(define (transform-painter painter origin corner1 corner2)
(lambda (frame)
(let ((m (frame-coord-map frame)))
(let ((new-origin (m origin)))
(painter
(make-frame new-origin
(sub-vect (m corner1) new-origin)
(sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
        (transform-painter painter
        (vect 0.0 1.0) (vect 1.0 1.0) (vect 0.0 0.0))
)

;(paint (flip-vert einstein))
;excierise 2.50

(define (flip-horiz painter)
    (transform-painter painter
                    (vect 1. 0.) (vect 0. 0.) (vect 1. 1.))
)

(define (rotate270 painter)
    (transform-painter painter
                    (vect 0. 1.) (vect 0. 0.) (vect 1. 1.))
)

(define (rotate180 painter)
  (transform-painter painter
                     (vect 1. 1.) (vect 0. 1.) (vect 1. 0.)))
;(paint (rotate180 einstein))
;(paint (rotate270 einstein))


;excerise 2.51 
(define (beside painter1 painter2)
(let ((split-point (vect 0.5 0.0)))
(let ((paint-left (transform-painter painter1 (vect 0.0 0.0) split-point (vect 0.0 1.0)))
(paint-right (transform-painter painter2 split-point (vect 1.0 0.0) (vect 0.5 1.0))))
(lambda (frame) (paint-left frame) (paint-right frame)))))


(define (below-v1 painter1 painter2)
(let ((split-point (vect 0.0 0.5)))
(let ((paint-top (transform-painter painter1 (vect 0.0 0.0) (vect 0.0 1.0) split-point))
(paint-bottom (transform-painter painter2 split-point (vect 1.0 0.5) (vect 0.0 1.0))))
(lambda (frame) (paint-top frame) (paint-bottom frame)))))

(paint (beside einstein einstein))
(paint (below-v1 einstein einstein))

