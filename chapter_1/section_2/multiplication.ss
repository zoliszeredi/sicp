(load "../../util/unittest.ss")

(define (multiply first second)
  (if (= second 0)
      0
      (+ first
	 (multiply first
		   (- second 1)))))

(define (multiply-fast first second)
  (define (multiply-fast-iter sum first second)
    (let ((double (lambda (x) (+ x x)))
	  (halve (lambda (x) (/ x 2)))
	  (dec (lambda (x) (- x 1)))
	  (inc (lambda (x) (+ x 1))))
      (cond ((zero? second) sum)
	    ((odd? second) (multiply-fast-iter (+ sum first)
					       first
					       (dec second)))
	    ((even? second) (multiply-fast-iter sum
					       (double first)
					       (halve second)))
	    (else (error "multiply error")))))
  (multiply-fast-iter 0 first second))


(asserteq (multiply-fast 5 11) (* 5 11))
(asserteq (multiply-fast 7 13) (* 7 13))
