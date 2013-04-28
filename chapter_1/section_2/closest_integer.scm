(use-modules (util unittest))

(define  (fibonacci-iter num)
  (define (iter current-term last-term count)
    (if (= count num)
	current-term
	(iter (+ current-term last-term)
	      current-term
	      (1+ count))))
  (iter 0 1 0))

(define (fibonacci-power num)
  (let ((phi (/ (+ 1 (sqrt 5)) 2))
	(psi (/ (- 1 (sqrt 5)) 2)))
    (/ (- (expt phi num)
	  (expt psi num))
       (sqrt 5))))

(define (fibonacci-rough num)
  (let ((phi (/ (+ 1 (sqrt 5)) 2)))
    (/ (expt phi num) (sqrt 5))))


(asserteq (fibonacci-power 15) 
	  (fibonacci-iter 15))
(asserteq (fibonacci-power 10) 
	  (fibonacci-iter 10))
