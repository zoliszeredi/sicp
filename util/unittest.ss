(define (asserteq first second)
  ;; assumes that the inputs are numbers
  (if (not (= first second))
      (let ((msg (format #f 
			 "~a is not equal to ~a~%" 
			 (number->string first)
			 (number->string second))))
	(display msg))))
