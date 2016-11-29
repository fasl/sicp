(define true #t)
(define false #f)
(define nil '())

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

  ;問題 2.63 2.3.3章
;下の二つの手続きはそれぞれ二進木をリストに変換する. 
;a. 二つの手続きはすべての木に対して同じ結果を生じるか. そうでなければ, 結果はどう違うか. 二つの手続きは図2.16のような木からどういうリストを生じるか. 
;b. n個の要素の釣合っている木をリストに変換するのに必要なステップ数の増加の程度は, 二つの手続きで同じか. 違うなら, どちらがより遅く増加するか. 

(define (tree->list-1 tree)
  (print "A")
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  
  (define (copy-to-list tree result-list)
    (print "B")
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
 
(define test-tree '(7 (3 (1 () ()) (5 () ())) (11 (9 () ()) (13 () ()))))
(define test-tree2 '(7 () (11 (9 () ()) (13 () ()))))
(define test-tree3 '(11 (9 () ()) (13 () ())))

(print (tree->list-1 test-tree))
(print (tree->list-2 test-tree))

(print (tree->list-1 test-tree2))
(print (tree->list-2 test-tree2))

(print (tree->list-1 test-tree3))
(print (tree->list-2 test-tree3))
  
