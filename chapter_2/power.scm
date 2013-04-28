(define (power base exp)
  (define (power-iter initial base exp)
    (if (= exp 0)
	initial
	(power-iter (* initial
		 base)
	      base
	      (- exp 1))))
  (power-iter 1 base exp))

(define (pair23 x y)
  (* (power 2 x)
     (power 3 y)))

(define (power-of-factor number factor)
  (define (iter-factor initial number factor)
    (let ((r (remainder number factor))
	  (inc (lambda (x) (+ x 1))))
      (if (= r 0)
	  (iter-factor (inc initial)
		       (/ number factor)
		       factor)
	  initial)))
  (iter-factor 0 number factor))

(define (pair23-car p)
  (power-of-factor p 2))

(define (pair23-cadr p)
  (power-of-factor p 3))
