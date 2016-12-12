(define true #t)
(define false #f)
(define nil '())

;2.74
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;のように書ける. これに対応し, 汎用演算を引数に作用させるapply-generic手続きは,
;単に演算の名前をデータオブジェクトに渡し, オブジェクトに仕事をさせるだけである:48 

(define (apply-generic op arg) (arg op))

