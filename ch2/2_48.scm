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


