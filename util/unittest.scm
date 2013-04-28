(define-module (util unittest)
  #:export (asserteq))

(define (asserteq first second)
  (cond ((integers? first second) (assert-int-eq first second))
	((reals? first second) (assert-real-eq first second))))

(define (integers? first second)
  (and (integer? first)
       (integer? second)))

(define (reals? first second)
  (and (real? first)
       (real? second)))

(define (integer-equal? first second)
  (= first second))

(define (integer-not-equal? first second)
  (not (integer-equal? first second)))

(define (real-equal? first second . decimals-accuracy)
  (let ((epsilon (if (null? decimals-accuracy) 
		     (expt 10 -3) 
		     (expt 10 (- (car decimals-accuracy)))))
	(delta (abs (- first second))))
    (>= epsilon delta)))

(define (real-not-equal? first second . decimals-accuracy)
  (if (null? decimals-accuracy)
      (not (real-equal? first second))
      (not (real-equal? (car decimals-accuracy)))))

(define (assert-int-eq first second)
  (if (integer-not-equal? first second)
      (display-format-fail-message first second)))

(define (assert-real-eq first second)
  (if (real-not-equal? first second)
      (display-format-fail-message first second)))

(define (display-format-fail-message first second)
  (display (format #f
		   "~a is not equal to ~a~%" 
		   (number->string first)
		   (number->string second))))
