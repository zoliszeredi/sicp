(load "../../util/unittest.ss")

(define (prime? number)
  (= number 
     (smallest-divisor number)))

(define (largest-possible-divisor number)
  (inexact->exact (truncate (sqrt number))))
				  

(define (smallest-divisor number)
  (let ((lpd (largest-possible-divisor number)))
    (find-divisor-in-list number 
			  (range 2 lpd))))


(define (find-divisor-in-list number divisor-list)
  (if (null? divisor-list)
      number
      (let ((first (car divisor-list))
	    (rest (cdr divisor-list))
	    (divides-first? (factor-of-number? (car divisor-list)))
	    (divides-number? (factor-of-number? number)))
	(if (divides-number? first)
	    first
	    (find-divisor-in-list number
				  (filter (lambda (x)
					    (not (divides-first? x)))
					  rest))))))

(define (range start end)
  (define (list-builder accumulator position)
    (if (< position start)
	accumulator
	(list-builder (cons position accumulator)
		      (- position 1))))
    (list-builder '() end))

(define (factor-of-number? number)
  (lambda (factor)
    (zero? (remainder number factor))))

