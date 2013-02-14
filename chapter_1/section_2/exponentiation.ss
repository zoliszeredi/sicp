(load "../../util/unittest.ss")

(define (power base exponent)
  ;; Computes the 'base' to the power of 'exponent'
  ;; The operation is done in O(n) time
  (define (power-iter combiner counter)
    (cond ((zero? counter) combiner)
	  (else (power-iter (* base combiner)
			    (1- counter)))))
  (power-iter 1 exponent))

(define (power-fast base exponent)
  ;; Computes the 'base' to the power of 'exponent'
  ;; The operation is done in O(lg n) time
  (define (power-fast-iter product base exponent)
    (cond ((zero? exponent) product)
	  ((odd? exponent) (power-fast-iter (* product base)
					    base
					    (- exponent 1)))
	  ((even? exponent) (power-fast-iter product
					     (* base base)
					     (/ exponent 2)))
	  (else (error "power-fast-iter error"))))
  (power-fast-iter 1 base exponent))

(asserteq (power 5 0) (expt 5 0))
(asserteq (power 2 10) (expt 2 10))
(asserteq (power 3 13) (expt 3 13))
(asserteq (power 12 15) (expt 12 15))
(asserteq (power-fast 5 0) (expt 5 0))
(asserteq (power-fast 2 10) (expt 2 10))
(asserteq (power-fast 3 13) (expt 3 13))
(asserteq (power-fast 12 15) (expt 12 15))
