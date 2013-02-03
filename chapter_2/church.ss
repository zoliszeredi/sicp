(define zero
  (let ((identity (lambda (x) x)))
    (lambda (f) identity)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
