(define (quadruple x)
  (let ((double (lambda (x)
                  (+ x x))))
    (double (double x))))

(quadruple 9)

(define (quad2 x)
  (display "begin quad2 ")
  (define (double x)
    (+ x x))
  (double (double x)))

(quad2 9)

(define (num-parity n)
  (letrec ((n-even? (lambda (n)
                      (if (= n 0)
                          'even
                          (n-odd? (- n 1)))))
           (n-odd? (lambda (n)
                     (if (= n 0)
                         'odd
                         (n-even? (- n 1))))))
    (n-even? n)))

(num-parity 82)
