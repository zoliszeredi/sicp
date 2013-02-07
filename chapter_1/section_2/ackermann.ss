(define (ackermann x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (ackermann (1- x) 
			 (ackermann x (1- y))))))

(load "../../util/unittest.ss")
(asserteq (ackermann 1 10) 1024)
(asserteq (ackermann 2 4) 65536)
(asserteq (ackermann 3 3) 65536)

