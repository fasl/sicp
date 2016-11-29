(define true #t)
(define false #f)
(define nil '())

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;問題 2.64 2.3.3章
;次の手続きlist->treeは順序づけられたリストを釣合っている二進木へ変換する. 
;補助手続きpartial-treeは引数として, 整数nと少くてもn個の要素のリストをとり, 
;リストの最初のn個の要素を含む釣合っている木を構成する. partial-treeが返す結果は
;(consで作った)対で, そのcarは構成された木, cdrは木に含まれなかった要素のリストである. 
;a. partial-treeがどう働くか, 出来るだけ明快な短い説明を書け. list->treeがリスト(1 3 5 7 9 11)に対して作る木を描け. 
;b. list->treeがn個の要素のリストを変換するのに必要なステップ数の増加の程度はどのくらいか. 

;Ans b:n

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (print "a")
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define odds '(1))
(print (list->tree odds))						  
(define odds '(1 3))
(print (list->tree odds))						  
(define odds '(1 3 5))
(print (list->tree odds))					  
(define odds '(1 3 5 7))
(print (list->tree odds))
(define odds '(1 3 5 7 9 11 13))
(print (list->tree odds))