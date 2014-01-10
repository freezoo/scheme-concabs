;; This file contains definitions of images that crudely resemble the ten
;; decimal digits.  They are named zero-bb through nine-bb.  These are presumed
;; to exist by one of the review problems in chapter 2 of Concrete
;; Abstractions: An Introduction to Computer Science Using Scheme, by
;; Max Hailperin, Barbara Kaiser, and Karl Knight.
;;
;; The design of these digits is patterned on seven-segment LED displays.
;; You view a digit eight as consisting of seven segments:
;;
;;  ***     <- a horizontal top segment
;; *   *
;; *   *    <- two vertical segments, top-left and top-right
;;  ***     <- a horizontal mid segment
;; *   *
;; *   *    <- two vertical segments, bottom-left and bottom-right
;;  ***     <- a horizontal bottom segment
;;
;; All the other digits can be approximated by using some subset of these
;; seven segments.
;;
;; Revision 1.2 as of 2003/02/12 17:13:18

(define filled-rectangle
  (lambda (x0 y0 x1 y1)
    (overlay (filled-triangle x0 y0 x0 y1 x1 y1)
             (filled-triangle x0 y0 x1 y0 x1 y1))))

(define top-seg
  (filled-rectangle -.6 .95 .6 .85))

(define mid-seg
  (filled-rectangle -.6 .05 .6 -.05))

(define bottom-seg
  (filled-rectangle -.6 -.95 .6 -.85))

(define top-left-seg
  (filled-rectangle -.75 .95 -.65 .025))

(define top-right-seg
  (filled-rectangle .75 .95 .65 .025))

(define bottom-left-seg
  (filled-rectangle -.75 -.95 -.65 -.025))

(define bottom-right-seg
  (filled-rectangle .75 -.95 .65 -.025))

(define zero-bb
  (overlay (overlay top-seg top-left-seg)
           (overlay
            (overlay top-right-seg bottom-left-seg)
            (overlay bottom-right-seg bottom-seg))))

(define one-bb
  (overlay top-right-seg bottom-right-seg))

(define two-bb
  (overlay (overlay top-seg 
                    (overlay top-right-seg mid-seg))
           (overlay bottom-left-seg bottom-seg)))

(define three-bb
  (overlay (overlay top-seg 
                    (overlay mid-seg bottom-seg))
           (overlay top-right-seg bottom-right-seg)))

(define four-bb
  (overlay (overlay top-left-seg top-right-seg)
           (overlay mid-seg bottom-right-seg)))

(define five-bb
  (overlay (overlay top-seg 
                    (overlay top-left-seg mid-seg))
           (overlay bottom-right-seg bottom-seg)))

(define six-bb
  (overlay (overlay (overlay top-seg top-left-seg)
                    (overlay mid-seg bottom-right-seg))
           (overlay bottom-seg bottom-left-seg)))

(define seven-bb
  (overlay top-seg
           (overlay top-right-seg bottom-right-seg)))

(define eight-bb
  (overlay (overlay (overlay top-seg top-left-seg)
                    (overlay mid-seg top-right-seg))
           (overlay bottom-seg
                    (overlay bottom-left-seg bottom-right-seg))))

(define nine-bb
  (overlay (overlay (overlay top-seg top-left-seg)
                    (overlay mid-seg bottom-right-seg))
           (overlay bottom-seg top-right-seg)))
