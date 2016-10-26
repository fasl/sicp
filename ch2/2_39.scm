;#!/usr/local/bin/gosh

(use www.cgi)
(use srfi-19)
(use text.tree)
(use text.html-lite)

;(define (square-list items)
; (if (null? items)
;  ()
; (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
  
 (define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

 (define (fold-right op initial sequence)
    (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (reverse items)
 (if (null? items)
     items
     (append (reverse (cdr items)) (list (car items)))))

;(print (reverse (list 1 2 3 4)))
	 
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
	 
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence ))

(define (reverse2 sequence)
 (fold-right (lambda (x y) (cons x y)) '() sequence))
  
(print (reverse2 (list 5 6)))
(print (reverse2 (list 1 2 3 4)))
  
(define (reverse3 sequence)
 (fold-left (lambda (x y) (cons y x)) '() sequence))

(print (reverse3 (list 1 2 3 4)))