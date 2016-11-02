;#!/usr/local/bin/gosh
#lang racket

(require racket/draw)
(require racket/gui)

(define nil '())

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
 
;(define origin 0)
;(define edge1 1)
;(define edge2 1)

(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-left
           (transform-painter painter1
                              split-point
							  (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              (make-vect 0.0 0.0)
							  (make-vect 0.5 1.0)
							  split-point
							   )))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
		
(define (below painter)
   (let (beside painter))		
   (let (rotate90 painter))
   (let (flip-horiz painter)))   
	
		


