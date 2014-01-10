;; This file contains the assembler for use with the SLIM simulator in
;; the application section of chapter 11 of Concrete Abstractions: An
;; Introduction to Computer Science Using Scheme, by Max Hailperin,
;; Barbara Kaiser, and Karl Knight.

;; The assembler is organized around an ADT called an "assembler state",
;; which is universally abbreviated as "as" in the code.  The idea is that
;; the assembler state embodies all the information that is accumulated as
;; the list of assembly language instructions is processed.  (That list may
;; also include labels and allocate-registers directives, not just assembly
;; language instructions per se.)  What we do is start with an initial
;; assembler state, made using make-initial-as, and transform it into
;; a new assembler state as each item in the list is processed.  That is
;; done by using each instruction (or label or allocate-registers directive)
;; to produce a "transformer" procedure, i.e., a procedure that is applied
;; to the current assembler state to produce the new assembler state.
;; The main loop in the assemble procedure takes care of this, by using
;; the procedure assembly-instruction->transformer to get the correct
;; transformer for each instruction (or label or directive) and then
;; applying that transformer to the current assembler state to get the new
;; one.   At the end, after all the instructions (and labels and directives)
;; have been processed, the final assembler state is converted into a
;; vector of  machine language instructions, using the procedure
;; as->instructions.

;; More specifically, the information stored in an assembler state is
;; as follows:
;;  (1) The next available register number for allocate-registers to assign
;;  (2) The next available instruction memory address, for the next
;;      instruction to go into.
;;  (3) A symbol table, which shows for each name that has been used
;;      for a register or a label which it is (register or label) and
;;      what the numeric value is.
;;  (4) A "builder" procedure, which is used to actually construct the
;;      machine language instructions at the end (in the procedure
;;      as->instructions ).  The builder procedure is passed two
;;      arguments:
;;         (a) the final symbol table, with all names from the whole program
;;         (b) the vector into which to store the machine language instructions

;; First we have the constructor and selectors for the assembler state ADT:

(define make-as
  (lambda (next-reg next-addr symbol-table builder)
    (list next-reg next-addr symbol-table builder)))

(define as-next-reg car)

(define as-next-addr cadr)

(define as-symbol-table caddr)

(define as-builder cadddr)

;; To make the machine language instructions, we just make a vector of
;; the right size and ask the builder procedure to fill it in for
;; us. Note that the assembler state tells us how many instructions
;; there are (since that is the next, unused, address -- remember that
;; addresses start at zero), what the final symbol table is, and the
;; builder procedure.

(define as->instructions
  (lambda (as)
    (let ((instructions (make-vector (as-next-addr as))))
      ((as-builder as) (as-symbol-table as) instructions)
      instructions)))

;; Initially we have used no registers or instruction addresses, have
;; an empty symbol table, and a builder procedure that has no machine
;; language instructions to build.

(define make-initial-as
  (lambda ()
    (make-as 0 0 (make-empty-symbol-table)
             (lambda (symbol-table instructions-vector)
               'done))))

;; As explained in the introductory remarks, the main assembly loop
;; finds and deploys the appropriate transformer for each instruction,
;; starting from the initial state and turning the final state into
;; machine language instructions.

(define assemble
  (lambda (program)
    (define loop
      (lambda (tail as)
        (if (null? tail)
            (as->instructions as)
            (loop (cdr tail)
                  ((assembly-instruction->transformer (car tail))
                   as)))))
    (loop program (make-initial-as))))

;; The assembly-instruction->transformer procedure is quite central,
;; since it converts an assembly instruction (which can be a label or
;; an allocate-registers directive -- not just a real instruction)
;; into a transformer procedure for transforming an assembler state
;; into the next one.  This is done primarily by using the
;; pattern/action list mechanism from chapter 7's movie query system.
;; The only exception is that any "instruction" that is a symbol is
;; recognized as being a label, and so rather than being checked
;; against the pattern/action list, it is handled by a separate
;; procedure, label->transformer.

(define assembly-instruction->transformer
  (lambda (instruction)
    (define loop
      (lambda (p/a-list)
        (cond ((null? p/a-list)
               (error "invalid assembly language instruction"
                      instruction))
              ((matches? (pattern (car p/a-list)) instruction)
               (apply (action (car p/a-list))
                      (substitutions-in-to-match
                       (pattern (car p/a-list))
                       instruction)))
              (else (loop (cdr p/a-list))))))
    (cond ((symbol? instruction)
           (label->transformer instruction))
          ((pair? instruction)
           (loop assembly-p/a-list))
          (else
           (error "neither a label nor an assembly language instruction"
                  instruction)))))

;; The label->transformer procedure constructions the appropriate
;; transformer for a given label, namely one that adds the label to
;; the assembler state's symbol table.

(define label->transformer
  (lambda (label)
    (lambda (as)
      (let ((next-reg (as-next-reg as))
            (next-addr (as-next-addr as))
            (symbol-table (as-symbol-table as))
            (builder (as-builder as)))
        (make-as next-reg next-addr
                 (add-label-to-table label next-addr
                                     symbol-table)
                 builder)))))

;; A quite similar transformer (to those built by label->transformer)
;; can be built for a single register name from an allocate-registers
;; directive;  the main difference is that we need to keep track of
;; allocating register numbers, giving an error message when we run out.

(define reg-name->transformer
  (lambda (reg-name)
    (lambda (as)
      (let ((next-reg (as-next-reg as))
            (next-addr (as-next-addr as))
            (symbol-table (as-symbol-table as))
            (builder (as-builder as)))
        (if (>= next-reg reg-bank-size)
            (error "No register left for name" reg-name)
            (make-as (+ next-reg 1) next-addr
                     (add-reg-to-table reg-name next-reg
                                       symbol-table)
                     builder))))))

;; Of course, each allocate-registers directive can contain more than
;; one register name.  To take care of building a transformer for a
;; whole list of names, we just compose together the transformers for
;; the individual names.  For an empty list of names, we use a
;; null-transformer that does nothing.

(define null-transformer
  (lambda (as)
    as))

(define compose-transformers
  (lambda (t1 t2)
    (lambda (as)
      (t1 (t2 as)))))

(define reg-names->transformer
  (lambda (reg-names)
    (if (null? reg-names)
        null-transformer
        (compose-transformers (reg-names->transformer (cdr reg-names))
                              (reg-name->transformer (car reg-names))))))

;; At this point, we've taken care of how to make transformer
;; procedures for the special cases -- labels and allocate-registers
;; directives.  That leaves the normal case of real assembly language
;; instructions.  These are all rather similar to one another.
;; Therefore, we define a higher-order procedure,
;; instruction->transformer which can build the transformer for any
;; instruction.  The specific pieces of information it needs to be
;; told, in order to build the appropriate transformer, are
;;  (1) Which kind of instruction?  This is specified by passing the
;;      machine-language-instruction constructor as the first
;;      argument.  For example, in making the transformer for an li
;;      assembly language instruction, we'd pass the constructor
;;      make-load-immediate-inst.
;;  (2) What are the operand specifiers?  This is specified by
;;      passing as the second argument a list of "operand specifier
;;      fetchers." Each operand specifier fetcher is a procedure that
;;      gets passed the final symbol table (the one passed to the
;;      builder, which contains all names from the whole program).
;;      The operand specifier fetcher returns the numeric operand
;;      value.  This may have been fetched from the symbol table
;;      (hence the name "fetcher"), if the assembly language version
;;      was a symbolic register name or label, rather than a numeral.
;;
;; One important point is that the transformer makes a new assembler
;; state that includes a builder that not only installs this one
;; instruction into the vector of machine language instructions, but
;; also invokes the old builder (from the old assembler state) to
;; install all the other instructions.  This is how we ultimately wind
;; up with one builder that installs all the instructions.

(define instruction->transformer
  (lambda (instruction-constructor operand-specifier-fetchers)
    (lambda (as)
      (let ((next-reg (as-next-reg as))
            (next-addr (as-next-addr as))
            (symbol-table (as-symbol-table as))
            (builder (as-builder as)))
        (make-as next-reg (+ next-addr 1)
                 symbol-table
                 (lambda (final-symbol-table instructions-vector)
                   (vector-set! instructions-vector
                                next-addr
                                (apply instruction-constructor
                                       (map (lambda (osf)
                                              (osf final-symbol-table))
                                            operand-specifier-fetchers)))
                   ;; now use the old builder to finish the job:
                   (builder final-symbol-table instructions-vector)))))))

;; To make a operand-specifier-fetcher procedure from an operand
;; specifier that is a register name or number, we use reg-specifier.
;; Recall that an operand-specifier-fetcher maps a (final) symbol
;; table into a numeric value.

(define reg-specifier
  (lambda (specifier)
    (cond ((symbol? specifier)
           (lambda (symbol-table)
             (get-reg-from-table specifier symbol-table)))
          ((and (integer? specifier)
                (exact? specifier)
                (>= specifier 0)
                (< specifier reg-bank-size))
           (lambda (symbol-table)
             specifier))
          (else
           (error "illegal register specifier" specifier)))))

;; Similarly, to make an operand-specifier-fetcher procedure from an
;; immediate operand specifier -- i.e., either a number or a label name,
;; we use immediate-specifier:

(define immediate-specifier
  (lambda (specifier)
    (cond ((symbol? specifier)
           (lambda (symbol-table)
             (get-label-from-table specifier symbol-table)))
          ((number? specifier)
           (lambda (symbol-table)
             specifier))
          (else
           (error "illegal immediate operand" specifier)))))

;; The pattern/action list will contain an action procedure for each
;; kind of assembly language instruction.  However, in the case of the
;; various alu instructions (add, sub, ...), the action procedures are
;; all essentially the same.  The only difference is which machine
;; language instruction constructor should be used -- make-add-inst,
;; make-sub-inst, etc.  So, we have a higher order procedure that
;; takes the machine language instruction constructor in and returns
;; the corresponding action procedure, for use in the pattern/action
;; list.

(define make-alu-inst-action
  (lambda (machine-language-inst-constructor)
    (lambda (destreg sourcereg1 sourcereg2)
      (instruction->transformer
       machine-language-inst-constructor
       (list (reg-specifier destreg)
             (reg-specifier sourcereg1)
             (reg-specifier sourcereg2))))))

;; Now we have some code from chapter 7 -- the mechanics of
;; pattern/action lists.

(define make-pattern/action
  (lambda (pattern action)
    (cons pattern action)))
    
(define pattern car)
(define action cdr)

(define matches?
  (lambda (pattern question)
    (cond ((null? pattern)  (null? question))
          ((null? question) #f)
          ((list? (car pattern))
           (if (member (car question) (car pattern))
               (matches? (cdr pattern)
                         (cdr question))
               #f))
          ((equal? (car pattern) '...) #t)
          ((equal? (car pattern) '_) 
           (matches? (cdr pattern)
                     (cdr question)))
          ((equal? (car pattern) (car question))
           (matches? (cdr pattern)
                     (cdr question)))
          (else #f))))
    
(define substitutions-in-to-match
  (lambda (pattern question)
    (cond ((null? pattern)
           (if (null? question)
               '()
               (error "substitutions-in-to-match without a match")))
          ((null? question)
           (error "substitutions-in-to-match without a match"))
          ((list? (car pattern))
           (if (member (car question) (car pattern))
               (cons (car question)
                     (substitutions-in-to-match (cdr pattern)
                                                (cdr question)))
               (error "substitutions-in-to-match without a match")))
          ((equal? (car pattern) '...) (list question))
          ((equal? (car pattern) '_)
           (cons (car question)
                 (substitutions-in-to-match (cdr pattern) (cdr question))))
          ((equal? (car pattern) (car question))
           (substitutions-in-to-match (cdr pattern)
                                      (cdr question)))
          (else (error "substitions-in-to-match without a match")))))
    
;; Now we are ready to present the pattern/action list for use by the
;; assembler.  Keep in mind that each action procedure needs to return
;; a transformer procedure.

(define assembly-p/a-list
  (list (make-pattern/action '(allocate-registers ...)
                             reg-names->transformer)
        (make-pattern/action '(add _ _ _)
                             (make-alu-inst-action make-add-inst))
        (make-pattern/action '(sub _ _ _)
                             (make-alu-inst-action make-sub-inst))
        (make-pattern/action '(mul _ _ _)
                             (make-alu-inst-action make-mul-inst))
        (make-pattern/action '(div _ _ _)
                             (make-alu-inst-action make-div-inst))
        (make-pattern/action '(quo _ _ _)
                             (make-alu-inst-action make-quo-inst))
        (make-pattern/action '(rem _ _ _)
                             (make-alu-inst-action make-rem-inst))
        (make-pattern/action '(seq _ _ _)
                             (make-alu-inst-action make-seq-inst))
        (make-pattern/action '(sne _ _ _)
                             (make-alu-inst-action make-sne-inst))
        (make-pattern/action '(slt _ _ _)
                             (make-alu-inst-action make-slt-inst))
        (make-pattern/action '(sgt _ _ _)
                             (make-alu-inst-action make-sgt-inst))
        (make-pattern/action '(sle _ _ _)
                             (make-alu-inst-action make-sle-inst))
        (make-pattern/action '(sge _ _ _)
                             (make-alu-inst-action make-sge-inst))
        (make-pattern/action '(ld _ _)
                             (lambda (destreg addressreg)
                               (instruction->transformer
                                make-load-inst
                                (list (reg-specifier destreg)
                                      (reg-specifier addressreg)))))
        (make-pattern/action '(st _ _)
                             (lambda (sourcereg addressreg)
                               (instruction->transformer
                                make-store-inst
                                (list (reg-specifier sourcereg)
                                      (reg-specifier addressreg)))))
        (make-pattern/action '(li _ _)
                             (lambda (destreg const)
                               (instruction->transformer
                                make-load-immediate-inst
                                (list (reg-specifier destreg)
                                      (immediate-specifier const)))))
        (make-pattern/action '(read _)
                             (lambda (destreg)
                               (instruction->transformer
                                make-read-inst
                                (list (reg-specifier destreg)))))
        (make-pattern/action '(write _)
                             (lambda (sourcereg)
                               (instruction->transformer
                                make-write-inst
                                (list (reg-specifier sourcereg)))))
        (make-pattern/action '(jeqz _ _)
                             (lambda (sourcereg addressreg)
                               (instruction->transformer
                                make-jeqz-inst
                                (list (reg-specifier sourcereg)
                                      (reg-specifier addressreg)))))
        (make-pattern/action '(j _)
                             (lambda (addressreg)
                               (instruction->transformer
                                make-jump-inst
                                (list (reg-specifier addressreg)))))
        (make-pattern/action '(halt)
                             (lambda ()
                               (instruction->transformer
                                make-halt-inst '())))))

;; Finally, we have the symbol table ADT, which we build on top of
;; tables from chapter 9.  We store each value in the table as a
;; table-entry (another ADT), with a kind (register or label) and a
;; numeric value.

(define make-empty-symbol-table
  (lambda ()
    (make-table '() '())))

(define get-reg-from-table
  (lambda (reg-name symbol-table)
    (table-find
     symbol-table reg-name
     (lambda (entry)
       (if (equal? (table-entry-kind entry)
                   'register)
           (table-entry-value entry)
           (error "name used as register name wasn't declared as one"
                  reg-name)))
     (lambda ()
       (error "name used as register name wasn't declared at all"
              reg-name)))))

(define get-label-from-table
  (lambda (label symbol-table)
    (table-find
     symbol-table label
     (lambda (entry)
       (if (equal? (table-entry-kind entry)
                   'label)
           (table-entry-value entry)
           (error "name used as immediate wasn't declared as a label"
                  label)))
     (lambda ()
       (error "name used as immediate wasn't declared at all"
              label)))))

(define add-reg-to-table
  (lambda (name value table)
    (add-to-table name (make-table-entry 'register value) table)))

(define add-label-to-table
  (lambda (name value table)
    (add-to-table name (make-table-entry 'label value) table)))

(define add-to-table
  (lambda (name entry table)
    (table-find
     table name
     (lambda (existing-entry)
       (error "attempt to redeclare the name" name))
     (lambda ()
       (make-table
        (cons name
              (table-keys table))
        (cons entry
              (table-values table)))))))

;; Here is chapter 9's table ADT (with a slight twist, namely the
;; addition of the table-keys and table-values selectors):

(define make-table
  (lambda (keys values)
    (cons keys values)))

(define table-keys car)

(define table-values cdr)

(define table-find
  (lambda (table key what-if-found what-if-not) 
    (define loop
      (lambda (keys values)
        (cond ((null? keys) (what-if-not))
              ((equal? key (car keys))
               (what-if-found (car values)))
              (else
               (loop (cdr keys) (cdr values))))))
    (loop (table-keys table) (table-values table))))

;; Finally, we have a trivial table entry ADT:

(define make-table-entry
  (lambda (kind value)
    (cons kind value)))

(define table-entry-kind car)

(define table-entry-value cdr)
