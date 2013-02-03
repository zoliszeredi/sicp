(define (reportmsg msg)
  (display msg)
  (newline))
  
(define (reporterr msg)
  (display "Error: ")
  (display msg)
  (newline))

(define (assert msg b)
  (if (not b) (reporterr msg)))

(define (asserteq msg a b)
  (assert msg (> 0.0001 (abs (- a b)))))

