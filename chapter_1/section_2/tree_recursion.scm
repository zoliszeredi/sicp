(use-modules (util unittest))

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
	 (* 2 (f-rec (- n 2)))
	 (* 3 (f-rec (- n 3))))))

	
(define (f-iter-helper min mid max count n)
  (if (= count n) 
      min 
      (f-iter-helper mid 
		     max (+ max
			 (* 2 mid)
			 (* 3 min))
		     (+ count 1)
		     n)))
 
(define (f-iter n)
  (f-iter-helper 0 1 2 0 n))

(asserteq (f-rec 25) (f-iter 25))
