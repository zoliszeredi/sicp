(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (square-root number . decimal-accuracy)
  (define (try-guess guess)
    (new-if (good-enough? guess)
	guess
	(try-guess (improve guess))))
  
  (define (good-enough? guess)
    (let ((eps (if (null? decimal-accuracy)
		   0.0001
		   (expt 10 (- (car decimal-accuracy)))))
	  (delta (abs (- number
			 (square guess)))))
      (> eps delta)))

  (define (improve guess)
    (average guess 
	     (/ number guess)))

  (try-guess 1.0))

(define (square number)
  (* number number))

(define (average x y)
  (/ (+ x y) 2))

(load "../../util/unittest.ss")

(define (test-square-root)
  (define (try-integers-range start stop)
    (define (next-value x)
      (+ x 1))
    (if (> start stop)
	#t
	(let ((expected (sqrt start))
	      (actual (square-root start 7)))
	  (if (real-equal? expected actual)
	      (try-integers-range (next-value start) stop)
	      (display-format-fail-message actual expected)))))


  (try-integers-range 1 100000))
