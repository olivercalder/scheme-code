;; Oliver Calder
;; April 2021

;; Takes a three-argument function and returns a curried version of that
;; function.
(define curry3
  (lambda (f3)
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (f3 a b c))))))

;; Takes a curried three-argument function and returns a normal Scheme
;; uncurried version of that function.
(define uncurry3
  (lambda (f)
    (lambda (a b c)
      (((f a) b) c))))

;; Helper function to compute the final output or the intermediate function
;; produced from the given curried function and the list of arguments to which
;; the curried function should be applied.
(define uncurry-helper
  (lambda (f lst)
    (if (null? (cdr lst))
        (f (car lst))
        (uncurry-helper (f (car lst)) (cdr lst)))))

;; Takes a curried function of any number of parameters and returns a normal
;; Scheme uncurried version of that function.
(define uncurry
  (lambda (f)
    (lambda args
      (uncurry-helper f args))))

;; Takes a function which produces a boolean value when applied to elements
;; of the given list, and returns a new list containing all values in the
;; given list for which the function returned #t.
(define my-filter
  (lambda (f lst)
    (cond ((null? lst) '())
          ((f (car lst)) (cons (car lst) (my-filter f (cdr lst))))
          (else (my-filter f (cdr lst))))))

;; Takes two "sets" (unordered lists) and produces the union of those sets.
;; Namely, returns a list containing every element that occurs in S1 or S2
;; or both.  Assumes that S1 and S2 are proper sets (ie. no internal duplicates
;; entries).
(define union
  (lambda (S1 S2)
    (if (null? S1) S2
        (cons (car S1)
              (union (cdr S1) (my-filter
                                (lambda (x) (not (equal? x (car S1)))) S2))))))

;; Takes two "sets" (unordered lists) and produces the intersection of those
;; sets.  Namely, return a list containing every element that occurs in both
;; S1 and S2.
(define intersect
  (lambda (S1 S2)
    (cond ((null? S1) '())
          ((not (null? (my-filter
                         (lambda (x) (equal? x (car S1))) S2)))
           (cons (car S1) (intersect (cdr S1) S2)))
          (else (intersect (cdr S1) S2)))))

;; Takes a boolean function and a list, and returns #t if the function returns
;; #t when applied to at least one item of the list, otherwise returns #f.
(define exists
  (lambda (f lst)
    (not (null? (my-filter f lst)))))


;; Helper function to ensure that the given value is a list.  That is, takes
;; a value and returns it if it is a list, otherwise returns a list containing
;; that value.
(define ensure-list
  (lambda (val)
    (if (list? val) val
        (cons val '()))))

;; Takes a function and a list of values on which to apply the function the
;; function, and returns a list of output values corresponding to the input
;; values from the list, flattening output values which are themselves lists
;; so that the final output is a single list of values.
(define flatmap
  (lambda (f lst)
    (if (null? lst) '()
        (append (ensure-list (f (car lst))) (flatmap f (cdr lst))))))


;; A more efficient (short-circuit) implementation of the exists function,
;; which returns true if there exists an element in the given list for which
;; the function f returns true when applied to that element.
(define exists-efficient
  (lambda (f lst)
    (cond ((null? lst) #f)
          ((f (car lst)) #t)
          (else (exists f (cdr lst))))))

;; A more efficient implementation of the intersect function, which leverages
;; the faster exists-efficient function to quickly prune elements.
(define intersect-efficient
  (lambda (S1 S2)
    (cond ((null? S1) '())
          ((exists-efficient (lambda (x) (equal? x (car S1))) S2)
           (cons (car S1) (intersect-efficient (cdr S1) S2)))
          (else (intersect-efficient (cdr S1) S2)))))

