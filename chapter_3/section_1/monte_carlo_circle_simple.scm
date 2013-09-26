(use-modules (srfi srfi-1))

(define (random-in-range low high)
  (+ (random (- high low))
     low))

(define rand
  (lambda ()
    (random-in-range -1.0 1.0)))

(define inside-circle? 
  (lambda (x y)
    (>= 1.0 (+ (* x x)
               (* y y)))))

(define circle-test
  (lambda ()
    (inside-circle? (rand) (rand))))

(define (monte-carlo trials test)
  (define (iter downcounter passed-tests)
    (cond ((zero? downcounter) passed-tests)
          ((test) (iter (1- downcounter) (1+ passed-tests)))
          (else (iter (1- downcounter) passed-tests))))
  (/ (iter trials 0)
     trials))

(define main
  (lambda ()
    (let ((ratio (monte-carlo 1000000 circle-test)))
      (display (- 4.0 ratio))
      (newline))))

(main)
