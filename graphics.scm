;; quilting
;; load and send fungraph.scm and then quilting.scm

(load "scheme/fungraph.scm")
(load "scheme/quilting.scm")

(define eps
  (lambda (image filename)
    (save-image-as-epsf image (string-append "scheme/" filename ".eps"))))

(save-image-as-epsf (stack (stack rcross-bb corner-bb) test-bb) "scheme/stack2.eps")

(save-image-as-epsf (stack (stack rcross-bb corner-bb)
                           (stack (quarter-turn-right test-bb) test-bb)) "scheme/stack3.eps")
(save-image-as-epsf (stack (stack (stack rcross-bb rcross-bb) rcross-bb) rcross-bb) "scheme/stackrcross.eps")

;; p. 16 ex. 1.9
(define half-turn
  (lambda (img)
    (quarter-turn-right (quarter-turn-right img))))

(define quarter-turn-left
  (lambda (img)
    (half-turn (quarter-turn-right img))))

(define side-by-side
  (lambda (img-a img-b)
    (quarter-turn-right
     (stack (quarter-turn-left img-b)
            (quarter-turn-left img-a)))))

(save-image-as-epsf (side-by-side rcross-bb corner-bb) "scheme/side.eps")

(define pinwheel
  (lambda (img)
    (stack (side-by-side (quarter-turn-right img) (half-turn img))
           (side-by-side img (quarter-turn-left img)))))

(save-image-as-epsf (pinwheel test-bb) "scheme/pinwheeltest.eps")

(eps (pinwheel rcross-bb) "pinwheelcross")
(eps (pinwheel (pinwheel rcross-bb)) "cross2")
(eps (pinwheel (pinwheel (pinwheel rcross-bb))) "cross3")

(define upside-down-tri
  (filled-triangle 0 0 1 1 -1 1))

(eps (pinwheel upside-down-tri) "pinwheel")
(eps (side-by-side (side-by-side upside-down-tri rcross-bb) test-bb) "side3")

(define half-tri-br
  (filled-triangle -1 -1 1 -1 1 1))
(define half-tri-tr
  (quarter-turn-left half-tri-br))
(define half-tri-tl
  (half-turn half-tri-br))
(define half-tri-bl
  (quarter-turn-right half-tri-br))
(define square
  (invert (filled-triangle 0 0 0 0 0 0)))
(define empty
  (filled-triangle 0 0 0 0 0 0))

(define sao-paulo
  (stack (side-by-side (side-by-side (side-by-side half-tri-br square) half-tri-bl) empty)
         (side-by-side (side-by-side (side-by-side empty half-tri-tr) square) half-tri-tl)))

(define sao-paulo-left
  (stack (side-by-side half-tri-br square)
         (side-by-side empty half-tri-tr)))

(define sao-paulo-right
  (stack (side-by-side half-tri-bl empty)
         (side-by-side square half-tri-tl)))

(eps (stack
      (side-by-side sao-paulo sao-paulo)
      (side-by-side (side-by-side sao-paulo-right sao-paulo) sao-paulo-left))
     "saopaulo2")

(define sao-paulo-unit-cell
  (stack sao-paulo
         (side-by-side sao-paulo-right sao-paulo-left)))

(eps (stack
      (side-by-side sao-paulo-unit-cell sao-paulo-unit-cell)
      (side-by-side sao-paulo-unit-cell sao-paulo-unit-cell))
     "saopaulo2")

(define sao-paulo-unit-cell-2
  (stack sao-paulo sao-paulo))

(eps (stack
      (side-by-side sao-paulo-unit-cell-2 sao-paulo-unit-cell-2)
      (side-by-side sao-paulo-unit-cell-2 sao-paulo-unit-cell-2))
     "saopaulo2")

(eps (pinwheel
      (pinwheel
       (pinwheel
        (pinwheel rcross-bb))))
     "manyrcross")

