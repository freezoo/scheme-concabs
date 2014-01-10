;; This file contains a system-independent version of the object-oriented
;; programming system for use with chapter 14 of Concrete Abstractions:
;; An Introduction to Computer Science Using Scheme, by Max Hailperin,
;; Barbara Kaiser, and Karl Knight. The construction of this system is
;; explained in section 14.4, and portions the below code appear in that
;; section.  However, there is also code here that is not in the text.
;;
;; Revision 1.3 as of 1999/06/15 21:53:30

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; A procedure for displaying the class hierarchy, to
; help mere humans understand what's going on.
;
(define show-class-hierarchy
  (lambda ()
    (define display-times
      (lambda (output count)
        (if (= count 0)
            'done
            (begin (display output)
                   (display-times output (- count 1))))))
    (define show-from
      (lambda (class level)
        (display-times "   " level)
        (display (class/get-name class))
        (newline)
        (for-each (lambda (c) (show-from c (+ level 1)))
                  (class/get-subclasses class))))
    (newline)
    (show-from object-class 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Auxilliary routines
;
(define overlap
  (lambda (names alist)
    (cond ((null? names) #f)
          ((assq (car names) alist) (car names))
          (else (overlap (cdr names) alist)))))

(define alist-from-onto
  (lambda (names num alist)
    (if (null? names)
        alist
        (alist-from-onto (cdr names)
                         (+ num 1)
                         (cons (list (car names)
                                     num)
                               alist)))))

(define vector-copy!
  (lambda (source target)
    (from-to-do 0 (- (vector-length source) 1)
                (lambda (i)
                  (vector-set! target i (vector-ref source i))))))

(define from-to-do
  (lambda (low high proc)
    (cond ((> low high) 'done)
          (else (proc low)
                (from-to-do (+ low 1) high proc)))))

(define apply-below
  (lambda (class proc apply-to?)
    (for-each (lambda (subclass)
                (if (apply-to? subclass)
                    (begin (proc subclass)
                           (apply-below subclass proc apply-to?))))
              (class/get-subclasses class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The two fundamental classes: object and class
;
; These two are cooked up by hand, since they provide the
; machinery that it takes to build classes, hence can't
; be used to build themselves.
;
; However, it may help understand them to know that they
; in principle (aside from the circularity and the lack
; of a superclass for the object class) could be defined
; as:
;
; (define-class
;   'object         ; name
;   'no-superclass  ; superclass
;   '(class)        ; instance variables
;   '(init          ; methods
;     describe))
;
; (define-class
;   'class          ; name
;   object-class    ; superclass
;   '(name          ; instance variables
;     subclasses
;     num-ivars
;     ivar-alist
;     num-methods
;     method-alist
;     method-vector
;     method-set?-vector
;     ancestry)
;   '(instantiator  ; methods
;     predicate
;     getter
;     setter
;     method
;     non-overridable-method
;     set-method!
;     ivar-position
;     method-position))

;; The class objects themselves; the first instance
;; variable of each (class) is left to be filled in later:

(define class-class
  (vector 'class-class-goes-here
          'class ; name
          '()    ; subclasses
          10     ; num-ivars
          '((class 0) ; ivar-alist (These position numbers must be
            (name 1)  ;     matched by the actual positioning of the items
            (subclasses 2); in this class-class vector as well as the ones
            (num-ivars 3) ; in the object-class vector below.)
            (ivar-alist 4)
            (num-methods  5)
            (method-alist 6)
            (method-vector 7)
            (method-set?-vector 8)
            (ancestry 9))
          11     ; num-methods
          '((init 0) ; method-alist
            (describe 1)
            (instantiator 2)
            (predicate 3)
            (getter 4)
            (setter 5)
            (method 6)
            (non-overridable-method 7)
            (set-method! 8)
            (ivar-position 9)
            (method-position 10))
          (make-vector 11)    ; method-vector
          (make-vector 11)    ; method-set?-vector
          (make-vector 2)))   ; ancestry

(define object-class
  (vector class-class        ; class
          'object            ; name
          (list class-class) ; subclasses
          1                  ; num-ivars
          '((class 0))       ; ivar-alist
          2                  ; num-methods
          '((init 0)         ; method-alist
            (describe 1))
          (make-vector 2)    ; method-vector
          (make-vector 2)    ; method-set?-vector
          (make-vector 1)))  ; ancestry

;; We define a smattering of getters and setters that are needed
;; in the "bootstrapping" process; the rest get defined later
;; once a mechanism is in place to do all a class's definitions.
;; The vector indices (numerical positions) used in these need to
;; be kept consistent with the ones in the ivar-alists for class-class
;; and object-class, above.

(define object/set-class! (lambda (obj val) (vector-set! obj 0 val)))

;; we also keep a copy of this crude object/set-class! around for use
;; in instantiators, because the normal setters check to make sure the
;; object in which they are doing the setting is of the appropriate class
;; (for object/set-class!, that it is of class "object"), and that can't
;; be done until the class has been installed -- so we need an unchecked
;; version to install it with

(define unchecked-object/set-class! object/set-class!)

;; we need unchecked-object/get-class for a similar reason, namely that
;; normal getters check using the class predicate that they are getting
;; from the right class, but the class predicates themselves use
;; [unchecked-]object/get-class, which would lead to infinite recursion

(define unchecked-object/get-class (lambda (obj) (vector-ref obj 0)))

(define class/get-name (lambda (obj) (vector-ref obj 1)))

(define class/get-subclasses (lambda (obj) (vector-ref obj 2)))

(define class/get-num-ivars (lambda (obj) (vector-ref obj 3)))

;; unchecked-class/get-num-ivars is needed for the exact same reason as
;; unchecked-object/get-class, above

(define unchecked-class/get-num-ivars class/get-num-ivars)

(define class/get-ivar-alist (lambda (obj) (vector-ref obj 4)))

(define class/get-method-alist (lambda (obj) (vector-ref obj 6)))

(define class/get-method-vector (lambda (obj) (vector-ref obj 7)))

(define class/set-method-vector! (lambda (obj val) (vector-set! obj 7 val)))

(define class/get-method-set?-vector (lambda (obj) (vector-ref obj 8)))

(define class/get-ancestry (lambda (obj) (vector-ref obj 9)))

(define class/set-ancestry! (lambda (obj val) (vector-set! obj 9 val)))

;; unchecked-class/get-ancestry is needed for the exact same reason as
;; unchecked-object/get-class, above

(define unchecked-class/get-ancestry class/get-ancestry)

;; We need to show that no methods have been set yet:

(vector-fill! (class/get-method-set?-vector object-class) #f)

(vector-fill! (class/get-method-set?-vector class-class) #f)

;; We need to fill in the ancestry vectors:

(vector-set! (class/get-ancestry object-class)
             0
             object-class)

(let ((a (class/get-ancestry class-class)))
  (vector-set! a 0 object-class)
  (vector-set! a 1 class-class))

;; The last instance variable remaining to be set
;; is what makes class-class be an instances of itself:

(unchecked-object/set-class! class-class class-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Methods of the two fundamental classes (object & class)
;
; Several of the methods are not only installed using
; class/set-method!, but also *defined* to have their
; names, for example class/set-method! itself; each
; of these has the comment:
;
;    temporary real, later replaced with virtual
;
; The meaning of this is that for now, the name, such as
; class/set-method!, is being defined to be the specific
; method, but later it will be defined to be a "virtual"
; method that simply retrieves the appropriate method
; from the method vector and applies it.  This virtual
; method indirection is crucial to allowing subclasses
; to overide the methods. For example, it allows a
; subclass of the class class to provide a different
; way of doing the set-method! method.  However, for
; the sake of "bootstrapping" (i.e., resolving the
; "chicken and egg" problem), we temporarily put the
; real methods under those names, since for now there
; are no subclasses of class anyhow, and then let them
; be replaced later.

(define class/method-position ; temporary real, later replaced with virtual
  (lambda (this method-name)
    (let ((lookup (assq method-name (class/get-method-alist this))))
      (if lookup
          (cadr lookup)
          (error "method name not present in class"
                 method-name (class/get-name this))))))

(define class/set-method! ; temporary real, later replaced with virtual
  (lambda (this method-name method)
    (let ((index (class/method-position this method-name)))
      (vector-set! (class/get-method-vector this)
                   index
                   method)
      (vector-set! (class/get-method-set?-vector this)
                   index
                   #t)
      (apply-below this
                   (lambda (class)
                     (vector-set! (class/get-method-vector class)
                                  index
                                  method))
                   (lambda (class)
                     (not (vector-ref (class/get-method-set?-vector
                                       class)
                                      index)))))
    method-name))

(class/set-method! class-class 'method-position class/method-position)

(class/set-method! class-class 'set-method! class/set-method!)

(class/set-method!
 object-class 'init
 (lambda (this) 'done))

(class/set-method!
 object-class 'describe
 (lambda (this)
   (let ((class (object/get-class this)))
     (newline)
     (display "An instance of the class ")
     (display (class/get-name class))
     (newline)
     (display "with the following instance variable values:")
     (newline)
     (for-each (lambda (ivar-pair)
                 (display "   ")
                 (display (car ivar-pair))
                 (display ": ")
                 (oops-display (vector-ref this (cadr ivar-pair)))
                 (newline))
               (class/get-ivar-alist class))
     (newline))))

(define oops-display
  (lambda (val)
    (cond ((object? val)
           (display "[an object of class ")
           (display (class/get-name (object/get-class val)))
           (display "]"))
          ((vector? val)
           (display "[a ")
           (display (vector-length val))
           (display " element vector]"))
          ((list? val)
           (display "[a ")
           (display (length val))
           (display " element list]"))
          ((pair? val)
           (display "[a non-list pair]"))
          (else ; symbol/string/number/boolean/procedure/char
           (write val)))))

(class/set-method!
 class-class 'init
 (lambda (this class-name superclass instvar-names method-names)
   (object^init this)
   (let ((instvar-overlap (overlap instvar-names
                                   (class/get-ivar-alist superclass))))
     (if instvar-overlap
         (error "make-class: instance variable in superclass"
                instvar-overlap)))
   (let ((method-overlap  (overlap method-names
                                   (class/get-method-alist superclass))))
     (if method-overlap
         (error "make-class: method in superclass"
                method-overlap)))
   (class/set-name! this class-name)
   (class/set-subclasses! this '())
   (class/set-subclasses! superclass
                          (cons this
                                (class/get-subclasses superclass)))
   (class/set-num-ivars! this (+ (class/get-num-ivars superclass)
                                 (length instvar-names)))
   (class/set-ivar-alist! this
                          (alist-from-onto
                           instvar-names
                           (class/get-num-ivars superclass)
                           (class/get-ivar-alist superclass)))
   (class/set-method-alist! this
                            (alist-from-onto
                             method-names
                             (class/get-num-methods superclass)
                             (class/get-method-alist superclass)))
   (let ((num-methods (+ (class/get-num-methods superclass)
                         (length method-names))))
     (class/set-num-methods! this num-methods)
     (let ((method-vector (make-vector num-methods)))
       (class/set-method-vector! this method-vector)
       (vector-copy! (class/get-method-vector superclass)
                     method-vector)
       (for-each (lambda (method-name)
                   (vector-set! method-vector
                                (class/method-position this
                                                       method-name)
                                (lambda (object . args)
                                  (error "Unimplemented method"
                                         method-name))))
                 method-names))
     (let ((method-set?-vector (make-vector num-methods)))
       (class/set-method-set?-vector! this method-set?-vector)
       (vector-fill! method-set?-vector #f)))
   (let ((ancestry (make-vector (+ (vector-length
                                    (class/get-ancestry superclass))
                                   1))))
     (class/set-ancestry! this ancestry)
     (vector-copy! (class/get-ancestry superclass) ancestry)
     (vector-set! ancestry
                  (- (vector-length ancestry) 1)
                  this))))

(class/set-method!
 class-class 'describe
 (lambda (this)
   (define display-instance-var-inheritance
     (lambda (index)
       (let ((ancestry (class/get-ancestry this)))
         (define loop
           (lambda (level)
             (let ((ancestor (vector-ref ancestry level)))
               (if (< index (class/get-num-ivars ancestor))
                   (if (eq? ancestor this)
                       (display "new")
                       (begin (display "from ")
                              (display (class/get-name ancestor))))
                   (loop (+ level 1))))))
         (loop 0))))
   (define display-method-name-inheritance
     (lambda (index)
       (let ((ancestry (class/get-ancestry this)))
         (define loop
           (lambda (level)
             (let ((ancestor (vector-ref ancestry level)))
               (if (< index (class/get-num-methods ancestor))
                   (if (eq? ancestor this)
                       (display "new name")
                       (begin (display "name from ")
                              (display (class/get-name ancestor))))
                   (loop (+ level 1))))))
         (loop 0))))
   (define display-method-implementation-inheritance
     (lambda (index)
       (let ((ancestry (class/get-ancestry this)))
         (define loop
           (lambda (level)
             (if (< level 0)
                 (display "unimplemented")
                 (let ((ancestor (vector-ref ancestry level)))
                   (if (>= index (class/get-num-methods ancestor))
                       (display "unimplemented")
                       (if (vector-ref (class/get-method-set?-vector ancestor)
                                       index)
                           (if (eq? ancestor this)
                               (display "new implementation")
                               (begin (display "implementation from ")
                                      (display (class/get-name ancestor))))
                           (loop (- level 1))))))))
         (loop (- (vector-length ancestry) 1)))))
   (newline)
   (display "The class ")
   (display (class/get-name this))
   (display " has the following ancestry:")
   (newline)
   (for-each (lambda (ancestor)
               (display "   ")
               (display (class/get-name ancestor))
               (newline))
             (vector->list (class/get-ancestry this)))
   (display "and the following immediate subclasses:")
   (newline)
   (for-each (lambda (subclass)
               (display "   ")
               (display (class/get-name subclass))
               (newline))
             (class/get-subclasses this))
   (display "and the following instance variables (including inherited ones):")
   (newline)
   (for-each (lambda (ivar-pair)
               (display "   ")
               (display (car ivar-pair))
               (display " (")
               (display-instance-var-inheritance (cadr ivar-pair))
               (display ")")
               (newline))
             (class/get-ivar-alist this))
   (display "and the following method names (including inherited ones):")
   (newline)
   (for-each (lambda (method-pair)
               (display "   ")
               (display (car method-pair))
               (display " (")
               (display-method-name-inheritance (cadr method-pair))
               (display ", ")
               (display-method-implementation-inheritance (cadr method-pair))
               (display ")")
               (newline))
             (class/get-method-alist this))
   (newline)))

(define class/instantiator ; temporary real, later replaced with virtual
  (lambda (this)
    (let ((num-ivars (class/get-num-ivars this)))
      (lambda init-args
        (let ((instance (make-vector num-ivars)))
          (unchecked-object/set-class! instance this)
          (apply object/init (cons instance init-args))
          instance)))))

(class/set-method!
 class-class 'instantiator
 class/instantiator)

;; The class predicates produced by the below procedure include a number
;; of consistency checks intended to reduce the chance of being fooled
;; by vectors that just happen to look like objects; it isn't totally
;; foolproof, however.  For example, the ancestry vector of object-class
;; looks totally indistinguishable from an instance of the object class.

(define class/predicate ; temporary real, later replaced with virtual
  (lambda (this)
    (let ((level (- (vector-length (class/get-ancestry this)) 1))
          (min-length (class/get-num-ivars this))
          (min-class-length (class/get-num-ivars class-class)))
      (lambda (object)
        (and (vector? object)
             (>= (vector-length object) min-length)
             (let ((class (unchecked-object/get-class object)))
               (and (vector? class)
                    (>= (vector-length class) min-class-length)
                    (let ((a (unchecked-class/get-ancestry class))
                          (size (unchecked-class/get-num-ivars class)))
                      (and (number? size)
                           (= size (vector-length object))
                           (vector? a)
                           (eq? (vector-ref a (- (vector-length a) 1))
                                class)
                           (> (vector-length a) level)
                           (eq? (vector-ref a level)
                                this))))))))))

(class/set-method!
 class-class 'predicate
 class/predicate)

(define class/getter ; temporary real, later replaced with virtual
  (lambda (this instvar-name)
    (let ((index (class/ivar-position this instvar-name))
          (ok? (class/predicate this)))
      (lambda (object)
        (if (ok? object)
            (vector-ref object index)
            ;; If object is not OK, the error message is different than shown
            ;; in the book (hopefully more helpful), and is produced by a
            ;; separate bad-oops-application procedure, given below.
            (bad-oops-application
             (getter-name (class/get-name this) instvar-name)
             object))))))

(class/set-method!
 class-class 'getter
 class/getter)

(define bad-oops-application  ; error message for mis-applied oops procedure
  (lambda (procedure-name first-arg)
    (if (object? first-arg)
        (error "OOPS procedure applied to object of wrong class"
               (list procedure-name
                     (class/get-name (object/get-class first-arg))
                     '...))
        (error "OOPS procedure applied to non-object"
               (list procedure-name first-arg '...)))))

(define class/setter ; temporary real, later replaced with virtual
  (lambda (this instvar-name)
    (let ((index (class/ivar-position this instvar-name))
          (ok? (class/predicate this)))
      (lambda (object value)
        (if (ok? object)
            (begin
              (vector-set! object index value)
              'set-done)
            ;; As in class/getter, the error message below is different from
            ;; what is shown in the book.
            (bad-oops-application
             (setter-name (class/get-name this) instvar-name)
             object))))))

(class/set-method!
 class-class 'setter
 class/setter)

(define class/method ; temporary real, later replaced with virtual
  (lambda (this meth-name)
    (let ((index (class/method-position this meth-name))
          (ok? (class/predicate this)))
      (lambda (object . args)  ; the "virtual method"
        ; gets real method from object's class
        ; and applies it instead
        (if (ok? object)
            (let ((method (vector-ref (class/get-method-vector
                                       (object/get-class object))
                                      index)))
              (apply method
                     (cons object args)))
            (bad-oops-application
             (method-name (class/get-name this) meth-name)
             object))))))

(class/set-method!
 class-class 'method
 class/method)

(define class/non-overridable-method
  ; temporary real, later replaced with virtual
  (lambda (this method-name)
    (let ((index (class/method-position this method-name))
          (method-vector (class/get-method-vector this))
          (ok? (class/predicate this)))
      (lambda (object . args)  ; the "virtual method"
        ; gets real method from this **class**
        ; (i.e., this) and applies it instead
        (if (ok? object)
            (let ((method (vector-ref method-vector index)))
              (apply method
                     (cons object args)))
            (bad-oops-application
             (non-overridable-method-name (class/get-name this)
                                          method-name)
             object))))))

(class/set-method!
 class-class 'non-overridable-method
 class/non-overridable-method)

(define class/ivar-position ; temporary real, later replaced with virtual
  (lambda (this ivar-name)
    (let ((lookup (assq ivar-name (class/get-ivar-alist this))))
      (if lookup
          (cadr lookup)
          (error "instance variable name not present in class"
                 ivar-name (class/get-name this))))))

(class/set-method!
 class-class 'ivar-position
 class/ivar-position)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Code-producing procedure for the definitions associated with a class.
;
(define class-definitions
  (lambda (class-name superclass instvar-names method-names)
    (list 'begin
          (list 'define (class-object-name class-name)
                (list 'make-class
                      (list 'quote class-name)
                      (class-object-name (class/get-name superclass))
                      (list 'quote instvar-names)
                      (list 'quote method-names)))
          (class-procedure-definitions-from-name-super-news
           class-name superclass instvar-names method-names))))

(define class-procedure-definitions-from-name-super-news
  (lambda (class-name superclass new-instvars new-methods)
    (class-procedure-definitions
     class-name
     (append new-instvars
             (map car (class/get-ivar-alist superclass)))
     (append new-methods
             (map car (class/get-method-alist superclass))))))

(define class-procedure-definitions-from-class
  (lambda (class)
    (class-procedure-definitions
     (class/get-name class)
     (map car (class/get-ivar-alist class))
     (map car (class/get-method-alist class)))))

(define class-procedure-definitions
  (lambda (class-name instvar-names method-names)
    (define definition
      (lambda (name name-constructor selector)
        (list 'define (name-constructor class-name name)
              (list selector (class-object-name class-name)
                    (list 'quote name)))))
    (list 'begin
          (list 'define (class-predicate-name class-name)
                (list 'class/predicate (class-object-name class-name)))
          (list 'define (class-instantiator-name class-name)
                (list 'class/instantiator (class-object-name class-name)))
          (cons 'begin
                (map (lambda (ivar-name)
                       (list 'begin
                             (definition ivar-name getter-name 'class/getter)
                             (definition ivar-name setter-name 'class/setter)))
                     instvar-names))
          (cons 'begin
                (map (lambda (name)
                       (list 'begin
                             (definition name method-name 'class/method)
                             (definition name non-overridable-method-name
                               'class/non-overridable-method)))
                     method-names)))))

(define class-object-name
  (lambda (class-name)
    (symbol-append class-name '-class)))

(define class-predicate-name
  (lambda (class-name)
    (symbol-append class-name '?)))

(define class-instantiator-name
  (lambda (class-name)
    (symbol-append 'make- class-name)))

(define getter-name
  (lambda (class-name ivar-name)
    (symbol-append class-name '/get- ivar-name)))

(define setter-name
  (lambda (class-name ivar-name)
    (symbol-append class-name '/set- ivar-name '!)))

(define method-name
  (lambda (class-name method-name)
    (symbol-append class-name '/ method-name)))

(define non-overridable-method-name
  (lambda (class-name method-name)
    (symbol-append class-name '^ method-name)))

(define symbol-append
  (lambda symbols
    (string->symbol
     (apply string-append
            (map symbol->string symbols)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; >>> Portability note:  <<<<
;
; The procedure eval-globally used in 
;    (eval-globally (class-defintions ... ....))
;  is defined below in a relatively portable way, but there are *much* better
;  ways in most Schemes.  For example, in gambit:
;     (define eval-globally eval)
;
; Alternatively, this could all be done very nicely in terms of macros, but
; that has not proven to be very portable in practice.
;
; The final, crude, alternative, is simply to invoke class-definitions from 
; the user interface (read-eval-print loop) and manually cut and paste its
; result back into the input.
;
; One note: the reason why we need eval-globally to pick apart the begins
; and write out each of the individual definitions is because of an
; unfortunate aspect of how MIT Scheme -- and perhaps som other Schemes --
; work, namely that all the names being defined within a begin are first
; made "unassigned", then assigned their new values -- which doesn't work
; for the bootstrapping, where computing the new values relies on the old.
; In a scheme that doesn't have this problem, but has no eval, you could
; just write out the whole exp, begins and all, in a single write -- though
; this is unlikely to save much.   In a scheme without this problem and
; with eval, you can replace the whole thing as shown above for gambit.
; In a scheme with this problem but with eval (like MIT Scheme), if you
; wanted to take advantage of the eval to avoid writing and loading, you
; would need to do the picking apart of begins as below, but then apply
; eval to each (and the user-initial-environment, in MIT Scheme's case),
; rather than writing it out.

(define eval-globally
  (lambda (exp)
    (let ((temp-name "evaltemp.scm"))
      (call-with-output-file temp-name
        (lambda (port)
          (define write-out
            (lambda (exp)
              (if (and (pair? exp)
                       (eq? (car exp) 'begin))
                  (for-each write-out (cdr exp))
                  (write exp port))))
          (write-out exp)))
      (load temp-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Time to finish bootstrapping object-class and class-class.
; By now all the machinery is in place to evaluate the
; class-procedure-definitions for these classes; this will
; overwrite the smattering of getters and setters that were
; provided above for bootstrapping purposes with essentially
; equivalent ones, and will overwrite the temporarily defined
; non-virtual methods like class/predicate with the virtual
; ones.  Additionally (and most importantly) it will fill in
; all the other getters, setters, and methods, and provide
; the instantiators, predicates, and non-overridable methods.
;
(eval-globally (list 'begin
                     (class-procedure-definitions-from-class object-class)
                     (class-procedure-definitions-from-class  class-class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The define-class procedure, which would be the normal,
; most convenient, way to make a class and all the
; conventionally associated interface definitions.
;
(define define-class
  (lambda (class-name superclass instvar-names method-names)
    (eval-globally
     (class-definitions
      class-name superclass instvar-names method-names))))
