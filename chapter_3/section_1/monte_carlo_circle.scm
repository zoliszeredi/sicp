(use-modules (srfi srfi-1))

(define (squared-euclidian-distance first-point second-point)
  ;; Returns the squared euclidian distance between two n-dimensional points
  (define (square-and-add x a)
    (+ a (expt x 2)))
  (define (diff-list first second)
    (define (subtract-first-two numbers)
      (- (car numbers) (cadr numbers)))
    (let ((zipped (zip first second)))
      (map subtract-first-two zipped)))
  (let ((diff-points (diff-list first-point
                     second-point)))
    (reduce square-and-add 0 diff-points)))

(define (make-circle origin radius)
  (define (part-of-area? point)
    (<= (squared-euclidian-distance point origin) 
        (expt radius 2)))
  (define (dispatch operation)
    (cond ((eq? operation 'part-of-area?) part-of-area?)
          (else '())))
  dispatch)

(define (random-in-range low high)
  (let ((diff (- high low)))
    (if (zero? diff)
        0
        (+ low (random (* 1.0 diff))))))

(define (random-generator-bounds bounds)
  (define (apply-on-range range)
    (apply random-in-range range))
  (map apply-on-range bounds))


(define (random-from-bounds bounds)
  (define (apply-on-range range)
    (apply random-in-range range))
  (map apply-on-range bounds))

(define (make-rectangle first-point second-point)
  (define (bounds)
    (let ((zipped (zip first-point second-point))
          (make-apply-function (lambda (function)
                                 (lambda (values)
                                   (apply function values)))))
      (zip (map (make-apply-function min) zipped)
           (map (make-apply-function max) zipped))))
  (define (area)
    (fold (lambda (range accumulate)
              (* (abs (apply - range))
                 accumulate))
            1.0
            (bounds)))
  (define (dispatch operation)
    (cond ((eq? operation 'bounds) bounds)
          ((eq? operation 'area) area)
          (else '())))
  dispatch)

(define (make-point-in-rectangle-generator rectangle)
  (let ((bounds  ((rectangle 'bounds))))
    (lambda ()
      (random-from-bounds bounds))))

(define (make-test circle rectangle)
  (let ((make-point (make-point-in-rectangle-generator rectangle))
        (point-on-disc? (circle 'part-of-area?)))
    (lambda ()
      (point-on-disc? (make-point)))))

(define (monte-carlo test trials)
  ;; Returns the fraction of tests that pased
  (define (iter downcounter passed)
    ;; Iterates downcounter times
    (cond ((zero? downcounter) (/ passed trials))
	  ((test) (iter (- downcounter 1)
			(+ passed 1)))
	  (else (iter (- downcounter 1)
                      passed))))
  (iter trials 0))

(define (points-in-circle-ratio)
  (let ((circle (make-circle '(0 0) 1))
         (rectangle (make-rectangle '(-1 -1) '(1 1))))
    (* ((rectangle 'area))
       (monte-carlo (make-test circle rectangle) 100000))))

(define (main)
  (display (points-in-circle-ratio)) (newline))

(main)

