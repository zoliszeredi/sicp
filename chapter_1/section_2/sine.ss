(define (sine radians)
  (if (< radians 0.1)
    radians
    (let ((one-third (sine (/ radians 3)))
	  (formula (lambda (x) (- (* x 3) 
				  (* (expt x 3) 4)))))
      (formula one-third))))

(load "../../util/unittest.ss")
(define pi 3.14159265)
(asserteq (sine (/ pi 2)) 1.0)
(asserteq (sine (/ pi 3)) (/ (sqrt 3) 2.0))
(asserteq (sine (/ pi 4)) (/ (sqrt 2) 2.0))
(asserteq (sine (/ pi 6)) 0.5)
	
