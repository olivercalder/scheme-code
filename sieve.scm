;; Oliver Calder
;; April 2021

;; Returns a lazy list of all integers between first and last, inclusive.
(define seq
  (lambda (first last)
    (if (> first last) #f
      (cons first (lambda () (seq (+ first 1) last))))))

;; Returns a lazy list of integers starting at first.
(define inf-seq
  (lambda (first)
    (cons first (lambda () (inf-seq (+ first 1))))))

;; Returns a list of the first n elements of the given lazy list.
(define first-n
  (lambda (lazy-list n)
    (if (or (equal? lazy-list #f) (< n 1)) '()
      (cons (car lazy-list) (first-n ((cdr lazy-list)) (- n 1))))))

;; Returns the nth element of the given lazy list.
(define nth
  (lambda (lazy-list n)
    (cond ((equal? lazy-list #f) #f)
          ((= n 1) (car lazy-list))
          (else (nth ((cdr lazy-list)) (- n 1))))))

;; Returns a lazy list identical to the given lazy list, but with all multiples
;; of n removed from the lazy list.
(define filter-multiples
  (lambda (lazy-list n)
    (cond ((equal? lazy-list #f) #f)
          ((= (modulo (car lazy-list) n) 0) (filter-multiples ((cdr lazy-list)) n))
          (else (cons (car lazy-list) (lambda () (filter-multiples ((cdr lazy-list)) n)))))))

;; Returns a lazy list of all mutually relatively prime elements of the given
;; lazy list, using the Sieve of Eratosthenes.
(define sieve
  (lambda (lazy-list)
    (cons (car lazy-list)
          (lambda () (sieve (filter-multiples ((cdr lazy-list)) (car lazy-list)))))))

;; Returns a lazy list of all prime numbers.
(define primes
  (lambda ()
    (sieve (inf-seq 2))))

;; Returns the number of elements in the given lazy list which are less than
;; the given limit.  current is a rolling sum which allows TCO, and should
;; be initialized to 0 to compute the total number in a given lazy list.
(define count-lazy-to-limit
  (lambda (lazy-list limit current)
    (if (or (equal? lazy-list #f) (>= (car lazy-list) limit)) current
      (count-lazy-to-limit ((cdr lazy-list)) limit (+ current 1)))))

;; Returns the number of primes with values less than the given limit.
(define count-smaller-primes
  (lambda (limit)
    (count-lazy-to-limit (primes) limit 0)))

;; Returns a lazy list of "twins" which occur in the given lazy list, where
;; two entries are twins if they are adjacent and the latter is equal to the
;; former plus 2.
(define find-twins
  (lambda (lazy-list)
    (cond ((or (equal? lazy-list #f) (equal? ((cdr lazy-list)) #f)) #f)
          ((equal? (+ (car lazy-list) 2) (car ((cdr lazy-list))))
           (cons (cons (car lazy-list) (car ((cdr lazy-list))))
                 (lambda () (find-twins ((cdr lazy-list))))))
          (else (find-twins ((cdr lazy-list)))))))

;; Returns a lazy list of "twin primes", which are adjacent primes where the
;; latter prime is equal to the former prime plus 2.
(define twin-primes
  (lambda ()
    (find-twins (primes))))
