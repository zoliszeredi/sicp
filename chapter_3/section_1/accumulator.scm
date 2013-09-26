;; Exercise 3.1

(define (accumulator initial)
  (let ((current initial))
    (lambda (x)
      (begin (set! current (+ current x))
	     current))))

(define (main)
  (let ((A (accumulator 5)))
    (begin (display (A 10))
	   (newline)
	   (display (A 10))
	   (newline))))

(main)
