(length '(0 #t #f))

(list 3 4 5)
;; list returns new list each time, (quote may return pointers
;; to same object.

(append '(1 2) '(3 4))
;; shares structure with last list
;; changes in "new" list may affect old list

(reverse '((1 2) (3 4)))

(member 22 '(18 22 #f 200))


