;; This file contains excerpts from the textbook Concrete
;; Abstractions: An Introduction to Computer Science Using Scheme, by
;; Max Hailperin, Barbara Kaiser, and Karl Knight, Copyright (c) 1998
;; by the authors. Full text is available for free at
;; http://www.gustavus.edu/+max/concrete-abstractions.html

;; Chapter 13: Object-based Abstractions

;; 13.5  Binary Search Trees Revisited

(define make-empty-ranked-btree
  (lambda ()
    (let ((tree (make-vector 6)))
      (vector-set! tree 0 #t) ; empty-tree? = true
      (vector-set! tree 2 #f) ; has no parent
      (vector-set! tree 5 0)  ; rank = 0
      tree)))

(define empty-tree?
  (lambda (tree)
    (vector-ref tree 0)))

(define set-empty! ;makes tree empty
  (lambda (tree)
    (vector-set! tree 0 #t)))

(define value
  (lambda (tree)
    (vector-ref tree 1)))

(define set-value!
  (lambda (tree item)
    (vector-set! tree 0 #f) ;not empty
    (vector-set! tree 1 item)))

(define parent
  (lambda (tree)
    (vector-ref tree 2)))

(define root?
  (lambda (tree)
    (not (vector-ref tree 2))))

(define left-subtree
  (lambda (tree)
    (vector-ref tree 3)))

(define set-left-subtree!
  (lambda (tree new-subtree)
    (vector-set! new-subtree 2 tree) ;parent
    (vector-set! tree 3 new-subtree)))

(define right-subtree
  (lambda (tree)
    (vector-ref tree 4)))

(define set-right-subtree!
  (lambda (tree new-subtree)
    (vector-set! new-subtree 2 tree) ;parent
    (vector-set! tree 4 new-subtree)))

(define rank
  (lambda (tree)
    (vector-ref tree 5)))

(define set-rank!
  (lambda (tree rank)
    (vector-set! tree 5 rank)))

(define which-subtree
  (lambda (tree)
    ;; Returns the symbol left if tree is left-subtree of its
    ;; parent and the symbol right if it is the right-subtree
    (cond ((root? tree)
           (error "WHICH-SUBTREE called at root of tree."))
          ((eq? tree (left-subtree (parent tree)))
           'left)
          (else 'right))))

(define sibling
  (lambda (tree)
    (cond ((root? tree)
           (error "SIBLING called at root of tree."))
          ((equal? (which-subtree tree) 'left)
           (right-subtree (parent tree)))
          (else
           (left-subtree (parent tree))))))

(define make-binary-search-tree make-empty-ranked-btree)

(define binary-search-in?
  (lambda (item bs-tree)
    (cond ((empty-tree? bs-tree)
           #f)
          ((= item (value bs-tree))
           #t)
          ((< item (value bs-tree))
           (binary-search-in? item (left-subtree bs-tree)))
          (else
           (binary-search-in? item (right-subtree bs-tree))))))

(define insertion-point
  (lambda (item bs-tree)
    ;; This procedure finds the point at which item should be
    ;; inserted in bs-tree. In other words, it finds the empty
    ;; leaf node where it should be inserted so that the
    ;; binary search condition still holds after it is inserted.
    ;; If item is already in bs-tree, then the insertion
    ;; point will be found by searching to the right so that
    ;; the new copy will occur "later" in bs-tree.
    (cond ((empty-tree? bs-tree) bs-tree)
          ((< item (value bs-tree))
           (insertion-point item (left-subtree bs-tree)))
          (else
           (insertion-point item (right-subtree bs-tree))))))

(define binary-search-insert!
  (lambda (item bs-tree)
    ;; This procedure will insert item into bs-tree at a leaf
    ;; (using the procedure insertion-point), maintaining
    ;; the binary search condition on bs-tree. The return value
    ;; is the subtree that has item at its root.
    ;; If item occurs in bs-tree, another copy of item
    ;; is inserted into bs-tree
    (let ((insertion-tree (insertion-point item bs-tree)))
      (set-value! insertion-tree item)
      (set-left-subtree! insertion-tree
                         (make-binary-search-tree))
      (set-right-subtree! insertion-tree
                          (make-binary-search-tree))
      insertion-tree)))

(define make-red-black-tree make-binary-search-tree)

(define red-black-in? binary-search-in?)

(define promote!
  (lambda (node)
    (set-rank! node (+ (rank node) 1))))

(define exchange-values!
  (lambda (node-1 node-2)
    (let ((value-1 (value node-1)))
      (set-value! node-1 (value node-2))
      (set-value! node-2 value-1))))

(define exchange-left-with-right!
  (lambda (tree-1 tree-2)
    (let ((left (left-subtree tree-1))
          (right (right-subtree tree-2)))
      (set-left-subtree! tree-1 right)
      (set-right-subtree! tree-2 left))))

(define rotate-left!
  (lambda (bs-tree)
    (exchange-left-with-right! bs-tree
                               (right-subtree bs-tree))
    (exchange-left-with-right! (right-subtree bs-tree)
                               (right-subtree bs-tree))
    (exchange-left-with-right! bs-tree
                               bs-tree)
    (exchange-values! bs-tree (left-subtree bs-tree))
    'done))

(define rotate-right!
  (lambda (bs-tree)
    (exchange-left-with-right! (left-subtree bs-tree)
                               bs-tree)
    (exchange-left-with-right! (left-subtree bs-tree)
                               (left-subtree bs-tree))
    (exchange-left-with-right! bs-tree
                               bs-tree)
    (exchange-values! bs-tree (right-subtree bs-tree))
    'done))

(define red-black-insert!
  (lambda (item red-black-tree)
    (define rebalance!
      (lambda (node)
        (cond ((root? node)
               'done)
              ((root? (parent node))
               'done)
              ((< (rank node) (rank (parent (parent node))))
               'done)
              ((= (rank node) (rank (sibling (parent node))))
               (promote! (parent (parent node)))
               (rebalance! (parent (parent node))))
              (else
               (let ((path-from-grandparent
                      (list (which-subtree (parent node))
                            (which-subtree node))))
                 (cond ((equal? path-from-grandparent '(left left))
                        (rotate-right! (parent (parent node))))
                       ((equal? path-from-grandparent '(left right))
                        (rotate-left! (parent node))
                        (rotate-right! (parent (parent node))))
                       ((equal? path-from-grandparent '(right left))
                        (rotate-right! (parent node))
                        (rotate-left! (parent (parent node))))
                       (else ; '(right right)
                        (rotate-left! (parent (parent node))))))))))
    (let ((insertion-node (binary-search-insert! item
                                                 red-black-tree)))
      (set-rank! insertion-node 1)
      (rebalance! insertion-node))
    'done))
