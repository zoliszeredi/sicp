(use-modules (util unittest))

(define (fibonacci-rec num)
  (if (< num 2)
      num
      (+ (fibonacci-rec (- num 1))
	 (fibonacci-rec (- num 2)))))

(asserteq (fibonacci-rec 10) 55)
(asserteq (fibonacci-rec 12) 144)

(define  (fibonacci-iter num)
  (define (iter last second-last count)
    (if (= count num)
	last
	(iter (+ last second-last)
	      last
	      (1+ count))))
  (iter 0 1 0))

(asserteq (fibonacci-iter 10) 55)
(asserteq (fibonacci-iter 12) 144)
