(define true #t)
(define false #f)
(define nil '())

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree) (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; 記号
                               (cadr pair))  ; 頻度
                    (make-leaf-set (cdr pairs))))))
  
;符号化木と例題の通信文を定義する: 
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                  (make-leaf 'B 2)
                  (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;Q2_68
 ;; ----------------------------------------------- 
 ;; EXERCISE 2.68 
 ;; ----------------------------------------------- 
  
 ;; helpers 
(define (element-of-set? x set) 
   (cond ((null? set) false) 
         ((equal? x (car set)) true) 
         (else (element-of-set? x (cdr set))))) 
(define (make-leaf symbol weight) 
   (list 'leaf symbol weight)) 
(define (leaf? object) 
   (eq? (car object) 'leaf)) 
(define (symbol-leaf x) (cadr x)) 
(define (left-branch tree) (car tree)) 
(define (right-branch tree) (cadr tree)) 
(define (symbols tree) 
   (if (leaf? tree) 
       (list (symbol-leaf tree)) 
       (caddr tree))) 
(define (weight-leaf x) (caddr x)) 
(define (make-code-tree left right) 
   (list left 
         right 
         (append (symbols left) (symbols right)) 
         (+ (weight left) (weight right)))) 
(define (weight tree) 
   (if (leaf? tree) 
       (weight-leaf tree) 
       (cadddr tree))) 
  
(define (encode message tree) 
   (if (null? message) 
       '() 
       (append (encode-symbol (car message) tree) 
               (encode (cdr message) tree)))) 
  
 ;; solution 
(define (encode-symbol smb tree) 
   (define (branch-correct? branch) 
     (if (leaf? branch) 
         (equal? smb (symbol-leaf branch)) 
         (element-of-set? smb (symbols branch)))) 
  
   (let ((lb (left-branch tree)) 
         (rb (right-branch tree))) 
     (cond ((branch-correct? lb) 
            (if (leaf? lb) '(0) (cons 0 (encode-symbol smb lb)))) 
           ((branch-correct? rb) 
            (if (leaf? rb) '(1) (cons 1 (encode-symbol smb rb)))) 
           (else (error "bad symbol -- ENCODE-SYMBOL" bit))))) 
  
 ;; tests 
(define sample-tree 
   (make-code-tree (make-leaf 'A 4) 
                   (make-code-tree 
                    (make-leaf 'B 2) 
                    (make-code-tree (make-leaf 'D 1) 
                                    (make-leaf 'C 1))))) 
;(print (encode '(A D A B B C A) sample-tree))
;(0 1 1 0 0 1 0 1 0 1 1 1 0) 

 ;Q2.69 
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
  
(define (successive-merge leaves) 
   (if (null? (cdr leaves)) 
       (car leaves) 
       (successive-merge 
        (adjoin-set (make-code-tree (car leaves) (cadr leaves)) 
                    (cddr leaves))))) 
							  
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define size-of-set length) 
(define first-in-ordered-set car) 
(define second-in-ordered-set cadr) 
(define (subset set n) 
     (if (= n 0) 
         set  
         (subset (cdr set) (- n 1)))) 

(print (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))

(define test-tree (generate-huffman-tree '((A 3) (B 5) (C 6) (D 6)))) 
(print (encode '(A B C D) test-tree) 




