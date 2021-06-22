(define gen-list
  (lambda (start end)
    (if (> start end) '()
      (cons start (gen-list (+ start 1) end)))))

(define pair-sum?
  (lambda (lst val)
    (if (null? (cdr lst)) #f
      (or (= (+ (car lst) (car (cdr lst))) val)
          (pair-sum? (cdr lst) val)))))

(define gen-lazy-list
  (lambda (start stop)
    (if (> start stop) #f
      (cons start (lambda () (gen-lazy-list (+ start 1) stop))))))

(define pair-sum-lazy?
  (lambda (lst val)
    (if (not ((cdr lst))) #f
      (or (= (+ (car lst) (car ((cdr lst)))) val)
          (pair-sum-lazy? ((cdr lst)) val)))))

(define make-lazy
  (lambda (lst)
    (if (null? lst) #f
      (cons (car lst)
            (lambda () (make-lazy (cdr lst)))))))

(define any-sum-lazy-helper
  (lambda (lst val cur)
    (if (not lst) #f
      (or (= (+ cur (car lst)) val) #t
          (any-sum-lazy-helper ((cdr lst)) val cur)
          (any-sum-lazy-helper ((cdr lst)) val (car lst))))))

(define any-sum-lazy
  (lambda (lst val)
    (if (not lst) #f
      (any-sum-lazy-helper ((cdr lst)) val (car lst)))))
