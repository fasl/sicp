;#!/usr/local/bin/gosh

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

  (define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
 
;(define origin 0)
;(define edge1 1)
;(define edge2 1)

(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))