
;#!/usr/local/bin/gosh

(use www.cgi)
(use srfi-19)
(use text.tree)
(use text.html-lite)
;;;;;;;;;;;  Q3.12
;(define x (list 'a 'b))
;(define y (list 'c 'd))
;(define z (append x y))
;(print z)
;(print (cdr x))
;(define w (append! x y))
;(print w)
;(print (cdr x))
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
;(print (length odds))


(define a (list 1 2))
;(print a)
(define x (list 1 3 '( 5 7) 9))
(define y '((7)))
(define z '(1 (2 (3 (4 (5 (6 7)))))))

(print x )
(print (cdr(cdr x)))
(print (cdr (car (cdr(cdr x)))))
(print (car (cdr (car (cdr(cdr x))))))
(print `-------------)
(print y)
(print (car (car y)))
(print `*******************)
(print z)
(print (car (cdr z)))
(print (cdr (car (cdr z))))
(print (car (cdr (car (cdr z)))))
(print (car (cdr (car (cdr (car (cdr z)))))))
(print (cdr (car (cdr (car (cdr (car (cdr (car (cdr z))))))))))
(print (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z)))))))))))))
;(print (cdr (cdr (cdr z))))