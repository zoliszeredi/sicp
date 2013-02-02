(define (make-point x y)
  (cons x y))

(define (point-x p)
  (car p))

(define (point-y p)
  (cdr p))

(define (make-line-seqment-from-points first second)
  (cons first second))

(define (start-line-segment line)
  (car line))

(define (end-line-segment line)
  (cdr line))

(define (mid-point-line-segment line)
  (let ((a (start-line-segment line))
	(b (end-line-segment line))
	(average (lambda (x y) (/ (+ x y)
				  2))))
    (let ((mx (average (point-x a)
		       (point-x b)))
	  (my (average (point-y a)
		       (point-y b))))
      (make-point mx my))))

(let ((a (make-point 2 3))
      (b (make-point 5 7)))
  (let ((line (make-line-seqment-from-points a b)))
    (mid-point-line-segment line))) 
      

