(+ 1 1)

(define (f x)
  (* 2 x))

(f 9)
(random 9)
(/ 3 0)
(+ 1 9)
(save-image-as-epsf (filled-triangle 0 0 0.5 0.5 0.5 0) "scheme/tri.eps")

+

sqrt
(* (+ 5 3) (- 5 3))
(/ (+ (* (- 17 14)
         5)
      6)
   7)

(define ark-volume (* (* 300 50) 30))
(lambda (x) (+ x (* 5/100 x)))
((lambda (x) (+ x (* 5/100 x))) 1.29)

(define f (lambda (x) (* x x)))
(define square (lambda (x) (* x x)))
(define f (lambda (x) (+ 10 x)))
(f 7)
(square 7)

(define mylist '(1 2 3))

(define alter-head
  (lambda (lst)
    (set-car! lst 0)))

(alter-head mylist)
mylist

(define x 9)
(define alter-var
  (lambda (n)
    (set! n 2)))
(alter-var x)
x

