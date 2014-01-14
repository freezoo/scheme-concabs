(reverse '(1 2 3 4 5))

(define (some-long-name x)
  (+ x 10))

(some-long-name 18)

(define a 5)
(cond ((= a 1)
       (display "one"))
      ((= a 2)
       (display "two"))
      ((= a 3)
       (display "three"))
      (else
       (display "not sure")))

(define (disp-and-ret n)
  (display n)
  n)

(disp-and-ret 9)

(define (multi-display . args)
  (if (null? args)
      (if #f #t)  ;; unspecified return value -- void
      (begin
        (display (first args))
        (apply multi-display (cdr args)))))

(multi-display "a" "b" "c")

(define void?? (if #f #t))
void??

(define (count-and-disp count . args)
  (if (null? args)
      count
      (begin
        (display (first args))
        ;; include count in list
        (apply count-and-disp (cons (+ 1 count) (cdr args))))))

(count-and-disp 10 "1" "a b" "3")
