;; Oliver Calder
;; April 2021


;; Returns #t if the given input is a list with three entries, where the last
;; two are lists.  Otherwise, returns #f.
(define is-bst-root?
  (lambda (input)
    (if (and (list? input)
             (not (null? input))
             (not (list? (car input)))
             (list? (cdr input))
             (not (null? (cdr input)))
             (list? (car (cdr input)))
             (list? (cdr (cdr input)))
             (not (null? (cdr (cdr input))))
             (list? (car (cdr (cdr input))))
             (null? (cdr (cdr (cdr input)))))
      #t
      #f)))

;; Returns the value at the root of the given bst.  If bst is an empty list or
;; is not a list containing three entries where the last two are lists, returns
;; #f.
(define entry
  (lambda (bst)
    (if (is-bst-root? bst)
      (car bst)
      #f)))

;; Returns the left subtree of the given bst.  If bst is an empty list or is not
;; a list containing three entries where the last two are lists, returns #f.
(define left
  (lambda (bst)
    (if (is-bst-root? bst)
      (car (cdr bst))
      #f)))

;; Returns the left subtree of the given bst without performing the checks which
;; ensure that the given node is well-formed.  Avoiding these checks saves a
;; great deal of time, and should be used when it is already known that the
;; given tree is well-formed.
(define left-unsafe
  (lambda (bst)
    (car (cdr bst))))

;; Returns the right subtree of the given bst.  If bst is an empty list or is
;; not a list containing three entries where the last two are lists, returns #f.
(define right
  (lambda (bst)
    (if (is-bst-root? bst)
      (car (cdr (cdr bst)))
      #f)))

;; Returns the right subtree of the given bst without performing the checks
;; which ensure that the given node is well-formed.  Avoiding these checks
;; saves a great deal of time, and should be used when it is already known
;; that the given tree is well-formed.
(define right-unsafe
  (lambda (bst)
    (car (cdr (cdr bst)))))


;; Returns a new bst whose root is elt, left subtree is left-bst, and right
;; subtree is right-bst.  If elt is not a number or either left-bst or
;; right-bst are not empty lists or valid bst nodes, then returns #f.
(define make-bst
  (lambda (elt left-bst right-bst)
    (cond ((not (integer? elt)) #f)
          ((not (or (null? left-bst) (is-bst-root? left-bst))) #f)
          ((not (or (null? right-bst) (is-bst-root? right-bst))) #f)
          (else (list elt left-bst right-bst)))))


;; Returns a list containing all values in bst in the order obtained from a
;; preorder traversal (root first, then preorder traversal of left subtree,
;; then preorder traversal of right subtree).
(define preorder
  (lambda (bst)
    (if (null? bst) '()
      (cons (car bst) (append
                        (preorder (left-unsafe bst))
                        (preorder (right-unsafe bst)))))))

;; Returns a list containing all values in bst in the order obtained from an
;; inorder traversal (inorder traversal of the left subtree, then the root,
;; then inorder traversal of the right subtree).
(define inorder
  (lambda (bst)
    (if (null? bst) '()
      (append (inorder (left-unsafe bst))
              (cons (car bst)
                    (inorder (right-unsafe bst)))))))

;; Returns a list containing all values in bst in the order obtained from a
;; postorder traversal (postorder traversal of the left subtree, then
;; postorder traversal of the right subtree, then the root).
(define postorder
  (lambda (bst)
    (if (null? bst) '()
      (append
        (postorder (left-unsafe bst))
        (postorder (right-unsafe bst))
        (cons (car bst) '())))))


;; Returns a new binary search tree identical to bst but with the integer v
;; appearing in its proper location.  The tree pointed to by bst should not
;; change.  If v is already in the tree, returns the original tree without
;; changing it.  Assumes that bst and all subtrees are valid binary search
;; trees.
(define insert
  (lambda (v bst)
    (cond ((null? bst) (cons v '(() ())))
          ((equal? v (car bst)) bst)
          ((< v (car bst)) (list (car bst) (insert v (left-unsafe bst)) (right-unsafe bst)))
          (else (list (car bst) (left-unsafe bst) (insert v (right-unsafe bst)))))))


;; Helper function for building a binary search tree from a list of integers.
(define bst-from-list-helper
  (lambda (lst bst)
    (if (null? lst) bst
      (bst-from-list-helper (cdr lst) (insert (car lst) bst)))))

;; Returns a binary search tree obtained by inserting the integers in lst
;; one-by-one into an initially empty binary search tree.
(define bst-from-list
  (lambda (lst)
    (bst-from-list-helper lst '())))


;; Returns #t if the tree is well-formed, and #f otherwise.
(define proper-tree?
  (lambda (bst)
    (if (null? bst) #t
      (and (is-bst-root? bst)
           (proper-tree? (left-unsafe bst))
           (proper-tree? (right-unsafe bst))))))
