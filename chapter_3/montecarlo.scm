(define rand
  (let ((max-number (expt 10 7)))
    (lambda ()
      (random max-number))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (square x)
  (* x x))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (Point values)
  (lambda (operation)
    (cond ((eq? operation 'x) (car values))
          ((eq? operation 'y) (cadr values))
          (else (error "Ilegal operation on Point")))))

(define (Circle origin radius)
  (lambda (operation)
    (cond ((eq? operation 'radius) radius)
          ((eq? operation 'origin) origin)
          (else (error "Ilegal operation on Circle")))))

(define (make-inside-circle-test circle)
  (let ((origin (circle 'origin))
        (radius (circle 'radius))
        (inside-circle?
         (lambda (origin radius point)
           (<= (+ (square (- (point 'x) (origin 'x)))
                  (square (- (point 'y) (origin 'y))))
               (square radius)))))
    (lambda (point)
      (inside-circle? origin radius point))))


(define (Limit values)
  (let ((low (apply min values))
        (high (apply max values)))
    (lambda (operation)
      (cond ((eq? operation 'low) low)
            ((eq? operation 'high) high)
            (else (error "Ilegal operation"))))))


;; FIXME
;; This does not work properly; it gives an error
(define (Random-Point-In-Limits limits)
  (let ((x-limits (car limits))
        (y-limits (cadr limits)))
    (lambda ()
      (Point (random-in-range (x-limits 'low) (x-limits 'high))
             (random-in-range (y-limits 'low) (x-limits 'high))))))
    
  
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

(define (main)
  (display (aproximate-pi))
  (newline))


