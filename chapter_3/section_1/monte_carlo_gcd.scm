(define rand
  (let ((max-number (expt 10 7)))
    (lambda ()
      (random max-number))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
  
(define (monte-carlo test trials)
  ;; Returns the fraction of tests that pased
  (define (iter downcounter passed)
    ;; Iterates downcounter times 
    (cond ((zero? downcounter) (/ passed trials))
	  ((test) (iter (- downcounter 1)
			(+ passed 1)))
	  (else (iter (- downcounter 1) passed))))
  (iter trials 0))

(define (aproximate-pi-cesaro)
  (let ((max-trials 500000))
    (sqrt (/ 6.0 (monte-carlo cesaro-test max-trials)))))

(define (main)
  (display (aproximate-pi-cesaro))
  (newline))

(main)
