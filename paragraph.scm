;; This file contains a little supplementary material to make the application
;; section on formatting paragraphs more enjoyable in chapter 12 of
;; Concrete Abstractions: An Introduction to Computer Science Using Scheme,
;; by Max Hailperin, Barbara Kaiser, and Karl Knight.
;;
;; Specifically, in includes two things:
;;  (1) an example paragraph (list of word strings) to format
;;  (2) a procedure for displaying a formatted paragraph


;; Words is a list of word strings, to try formatting; you can use
;; the first-elements-of procedure from chapter 7 to extract a smaller
;; number of words.

(define words
  '("One" "approach" "to" "this" "problem" "is" "to" "first"
    "simplify" "it" "somewhat" "by" "taking" "the" "list" "of" "strings"
    "and" "converting" "it" "into" "a" "list" "of" "numbers," "namely"
    "the" "widths" "of" "the" "strings." "Working" "from" "this" "list"
    "of" "widths," "we" "will" "initially" "produce" "as" "output"
    "simply" "a" "list" "of" "integers," "where" "each" "of" "these"
    "integers" "specifies" "how" "many" "words" "are" "in" "the"
    "corresponding" "line." "Using" "this," "we" "can" "then" "chop"
    "up" "the" "original" "list" "of" "strings" "into" "the" "final"
    "list" "of" "lists" "of" "strings." "In" "other" "words," "our"
    "overall" "formatting" "process" "has" "three" "phases:"
    "preprocessing" "the" "list" "of" "strings" "into" "a" "list" "of"
    "widths," "doing" "the" "main" "decision" "making" "about" "how"
    "many" "words" "to" "put" "on" "each" "line," "and" "then"
    "postprocessing" "to" "get" "a" "list" "of" "lists" "of" "strings."
    "The" "following" "two" "exercises" "take" "care" "of" "the"
    "preprocessing" "and" "postprocessing" "stages."))



;; The display-lines procedure expects to be given a list of lists of word
;; strings.  Each list of strings is displayed on a line, with spaces in
;; between the individual words.  Note that since some Scheme systems
;; expect a newline before each line while others expect one after each
;; line, we provide one before the first line, between each line and the
;; next, and after the last line.  Thus on some systems there will be a blank
;; line before the paragraph, while on other systems there will be a blank
;; line after the paragraph.

(define display-lines
  (lambda (lines)
    (newline)
    (for-each
     (lambda (line)
       (if (null? line)
           'done
           (begin
             (display (car line))
             (for-each (lambda (word) (display " ") (display word))
                 (cdr line))))
       (newline))
     lines)))
