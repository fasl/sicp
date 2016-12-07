(define true #t)
(define false #f)
(define nil '())

(define (element-of-set? x set) 
   (cond ((null? set) false) 
         ((equal? x (car set)) true) 
         (else (element-of-set? x (cdr set))))) 

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

;Q2_67
;通信文を復号化するのにdecode手続きを使い, 結果を示せ. 

; A:0 B:10 C:110 D:111 答え　ACABBC  
;(print sample-tree)
;(print (decode sample-message sample-tree))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

  (define (element-of-tree? symbol tree)
    (pair? (memq symbol (symbols tree))))
		 
;(print (encode '(A D A B B C A) sample-tree))

(define sample-tree 
   (make-code-tree (make-leaf 'A 4) 
                   (make-code-tree 
                    (make-leaf 'B 2) 
                    (make-code-tree (make-leaf 'D 1) 
                                    (make-leaf 'C 1))))) 
;(print (encode '(A D A B B C A) sample-tree))

;Q 2.64
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
					  
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge
        (adjoin-set
          (make-code-tree (car pairs) (cadr pairs))
          (cddr pairs)))))

(define pairs '((A 4) (B 2) (C 1) (D 1)))
;(print (generate-huffman-tree pairs)) 

;Q2.70 http://community.schemewiki.org/?sicp-ex-2.70
(define rocktree (generate-huffman-tree '((A 2) (NA 16) (BOOM  1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))) 

(define rock-song '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom)) 
  
(define encoded-rock-song (encode rock-song rocktree)) 

(print (encoded-rock-song))
(print (length encoded-rock-song))
  

