;; This file defines the error procedure assumed by the textbook
;; Concrete Abstractions: An Introduction to Computer Science Using Scheme
;; by Max Hailperin, Barbara Kaiser, and Karl Knight.
;;
;; This file makes use of only R4RS standard Scheme features.  This
;; causes no problem with actually reporting the error, but means
;; there is good way to explicitly abort execution after the error has
;; been reported.  Instead, we use the internally-defined procedure
;; abort, which gives the user a chance to abort execution using some
;; system-specific user-interface means.  If you are using this file
;; with a particular Scheme implementation that doesn't supply the
;; error procedure, but does provide a good way to abort execution,
;; you probably want to edit the definition below appropriately.
;;
;; This file written by Max Hailperin <max@gustavus.edu>.
;; Last revision March 26, 1998.

(define error
  (lambda (message . values)
    (define abort
      (lambda ()
        (newline)
        (display "Either abort execution (using some user-interface means)")
        (newline)
        (display " or enter a value to return from the error procedure")
        (newline)
        (read)))
    (newline)
    (display "Error: ")
    (display message)
    (for-each (lambda (value)
                (display " ")
                (write value))
              values)
    (newline)
    (abort)))