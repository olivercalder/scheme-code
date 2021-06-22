;; Oliver Calder
;; April 2021

;; This function returns the sum of the values in a given list.
;; The list must only contain numerical values.
(define sum
  (lambda (lst)
    (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst))))))

;; This function returns the length of a given list
(define list-length
  (lambda (lst)
    (if (null? lst)
      0
      (+ 1 (list-length (cdr lst))))))

;; This function returns a list containing the first n values of the given list.
;; Assumes that n is less than or equal to the length of the list.
(define keep-first-n-helper
  (lambda (n lst)
    (if (<= n 0)
      '()
      (cons (car lst) (keep-first-n-helper (- n 1) (cdr lst))))))

;; This function returns a list containing the first n values of the given list.
;; If the list is shorter than n entries long or n <= 0, returns an empty list.
;; Uses the keep-first-n-helper function to reduce the runtime from O(n!) to O(n).
(define keep-first-n
  (lambda (n lst)
    (if (or (<= n 0) (> n (list-length lst)))
      '()
      (keep-first-n-helper n lst))))
