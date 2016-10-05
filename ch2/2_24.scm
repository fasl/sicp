
;#!/usr/local/bin/gosh

(use www.cgi)
(use srfi-19)
(use text.tree)
(use text.html-lite)

(define x (list 1 (list 2 (list 3 4))))

(list x)

(length 2)