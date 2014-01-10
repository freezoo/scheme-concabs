;; This file contains a pseudo-random number generator written in Scheme.
;;
;; The goal is to provide the random procedure assumed by the textbook
;; Concrete Abstractions: An Introduction to Computer Science Using Scheme
;; by Max Hailperin, Barbara Kaiser, and Karl Knight.
;;
;; In addition to the random procedure, this file also provides a randomize
;; procedure, which can be used to reset the pseudo-random number generator
;; back to a known starting point.  The initial starting point is the
;; one that would result from doing (randomize 1).  The argument to
;; randomize must be a positive exact integer strictly less than 2 to the 63,
;; while random can take as its argument any positive exact integer that is
;; less than or equal to 2 to the 63.
;;
;; This file makes use of one other procedure from outside of the R4RS
;; standard, but which is also assumed by the textbook, namely error.
;;
;; The algorithm used here is a two-stage one.  First there is a simple
;; linear congruential generator, implemented by the internal procedure
;; rnd.  It then feeds into the "shuffling" algorithm of Bays and Durham.
;; More information about both of these algorithms is available in section
;; 3.2 of Knuth's Art of Computer Programming.
;;
;; This file written by Max Hailperin <max@gustavus.edu>.
;; Last revision March 26, 1998.

(define random #f)
(define randomize #f)

(let* ((a 3141592653589793237)
       (c a)
       (x 1)
       (m (expt 2 63))
       (v-size 128)
       (v (make-vector v-size))
       (y #f))
  (define (rnd)
    (set! x (remainder (+ (* a x) c) m))
    x)
  (set! randomize
        (lambda (n)
          (if (not (and (integer? n)
                        (exact? n)
                        (positive? n)
                        (< n m)))
              (error "Argument to randomize not a positive exact integer less than maximum value" n m)
              (begin (set! x n)
                     (let loop ((i 0))
                       (if (= i v-size)
                           'done
                           (begin (vector-set! v i (rnd))
                                  (loop (+ i 1)))))
                     (set! y (rnd))))))
  (randomize x)
  (set! random
        (lambda (max)
          (if (not (and (integer? max)
                        (exact? max)
                        (positive? max)
                        (<= max m)))
              (error "Argument to random not a positive exact integer less than or equal to maximum value" max m)
              (let ((j (quotient (* v-size y) m)))
                (set! y (vector-ref v j))
                (vector-set! v j (rnd))
                (quotient (* y max) m))))))