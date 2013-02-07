(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
	 (* 2 (f-rec (- n 2)))
	 (* 3 (f-rec (- n 3))))))


;; FIXME
;; Not yet functional

(define (f-iter sum last second-last third-last)
  (let ((current (+ last
		    (* 2 second-last)
		    (* 3 third-last))))
    (f-iter (+ sum current)
	    current
	    last
	    second-last)))
	  
  

(load "../../util/unittest.ss")
(asserteq 0 (f-rec 0))
(asserteq 4 (f-rec 3))
(asserteq 1892 (f-rec 10))

