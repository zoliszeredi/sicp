(define (expmod base exponent mod)
  (let ((square (lambda (x) (* x x))))
    (cond ((zero? exponent) 1)
	  ((even? exponent)
	   (remainder (square (expmod base 
				      (/ exponent 2) 
				      mod)) 
		      mod))
	  (else 
	   (remainder (* base (expmod base 
				      (- exponent 1) 
				      mod)) 
		      mod)))))

(load "../../util/unittest.ss")
(asserteq (expmod 2 10 3) 
	  (remainder (expt 2 10) 3))
(asserteq (expmod 5 3 11) 
	  (remainder (expt 5 3) 11))
(asserteq (expmod 7 5 13) 
	  (remainder (expt 7 5) 13))
(asserteq (expmod 9 15 7) 
	  (remainder (expt 9 15) 7))
