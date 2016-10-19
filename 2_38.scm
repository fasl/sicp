
;#!/usr/local/bin/gosh

(use www.cgi)
(use srfi-19)
(use text.tree)
(use text.html-lite)

;(define (square-list items)
; (if (null? items)
;  ()
; (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list items)
 (map (lambda (x) (* x x)) items))
(square-list (list 1 2 3 4))
;(1 4 9 16)



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
  
(print (fold-right / 1 (list 1 2 3)))

(print (fold-left / 1 (list 1 2 3)))

(print (fold-right list '() (list 1 2 3)))

(print (fold-left list '() (list 1 2 3)))
