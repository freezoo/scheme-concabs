(define f (lambda (x) (* x x)))
(define square f)
(square 9)

(define f (lambda (x) (+ x 2)))
(square 9)
(f 9)

(define pi 3.14159265)
(define cylinder-volume
  (lambda (radius height)
    (* (* pi (square radius))
       height)))
(cylinder-volume 5 4)

;; p. 12 ex. 1.4
(define candy-temp
  (lambda (temp elev)
    (round (- temp (/ elev 500)))))
(candy-temp 244 5280)

;; p. 13 ex. 1.5
(define tax
  (lambda (earnings)
    (if (< earnings 10000)
        0
        (* 0.2 (- earnings 10000)))))
(tax 12500)

;; p. 18 ex. 1.12
(define f
  (lambda (x y)
    (if (even? x)
        7
        (* x y))))
(f 1 16)

;; ex 1.13
(- (+ (* 7 7) (* 2 2 2 2 2 2 2)) 4)
(define average
  (lambda (x y)
    (/ (+ x y) 2)))
(average 66 2)
