(define factorial
  (lambda (n)
    (if (= n 1)
        1
        (* (factorial (- n 1))
           n))))

(factorial 52)
(expt 2 -2)

(define power
  (lambda (base exponent)
    (if (= exponent 0)
        1
        (* base (power base (- exponent 1))))))

(power 2 9)
(expt 2 9)

(define square-2.4
  (lambda (n)
    (if (= n 0)
        0
        (if (even? n)
            (* (square-2.4 (/ n 2))
               4)
            (+ (square-2.4 (- n 1))
               (- (+ n n) 1))))))
(square-2.4 202)
(define square
  (lambda (x)
    (* x x)))
(square 202)

(define print-sign
  (lambda (n)
    (cond ((= n 0) 'Zero)
          ((> n 0) 'Pos)
          ((< n 0) 'Neg))))
(print-sign -3)
(eq? 'Neg 'neg)

(define (pair-copy pr)
  (cons (car pr) (cdr pr)))
(define lst '(1 2 3 4))
(define lst-copy (pair-copy lst)
(set-car! (cdr lst-copy) 99)
lst
(define lst-app (append lst))
(set-car! (cdr lst) 33)
lst-app

;; append copies all but last list
(define lst1 '(1 2 3))
(define lst2 '(10 20 30))
(define lst12 (append lst1 lst2 '()))
st12
(set-car! lst1 11)
lst1
(set-car! lst2 1010)

(define (rev lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst))
              (list (car lst)))))
(rev '(2 4 6 8 10))
