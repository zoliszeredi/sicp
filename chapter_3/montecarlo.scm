(define (cesaro-test)
  (let ((max-number (expt 10 7)))
    (let ((rand (lambda () (random max-number))))
      (= (gcd (rand) (rand)) 1))))

(define (Point values)
  (lambda (operation)
    (cond ((eq? operation 'x) (car values))
          ((eq? operation 'y) (cadr values))
          (else (error "Ilegal operation on Point")))))
          

(define (circle-test)
  (let ((max-number (expt 10 7))
        (origin (Point 5 7))
        (radius 3))
    (let ((rand (lambda () (random max-number)))
          (formula (lambda (point origin radius)
                     (<= (+ (expt (- (point 'x) (origin 'x)) 2)
                            (expt (- (point 'y) (origin 'y)) 2))
                         (expt radius 2)))))
      (let ((Random-Point (Point (list (rand) (rand)))))
        (formula (Random-Point) origin radius)))))
  
(define (monte-carlo test trials)
  ;; Returns the fraction of tests that pased
  (define (iter downcounter passed)
    ;; Iterates downcounter times 
    (cond ((zero? downcounter) (/ passed trials))
	  ((test) (iter (- downcounter 1)
			(+ passed 1)))
	  (else (iter (- downcounter 1) passed))))
  (iter trials 0))

(define (aproximate-pi)
  (let ((max-trials 500000))
    (sqrt (/ 6.0 (monte-carlo cesaro-test max-trials)))))


(display (aproximate-pi)) (newline)
