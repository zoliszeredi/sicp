(use-modules (util unittest))

(define (factorial-recursive number)
  (if (= number 0)
      1
      (* (factorial-recursive (- number 1))
	 number)))

(define (factorial-iterative number)
  (define (iter product element)
    (if (= element 0)
	product
	(iter (* product element) (- element 1))))
  (iter 1 number))

(asserteq (factorial-recursive 3) (* 1 2 3))
(asserteq (factorial-recursive 4) (* 1 2 3 4))
(asserteq (factorial-recursive 5) (* 1 2 3 4 5))
(asserteq (factorial-recursive 6) (* 1 2 3 4 5 6))

(asserteq (factorial-iterative 3) (* 1 2 3))
(asserteq (factorial-iterative 4) (* 1 2 3 4))
(asserteq (factorial-iterative 5) (* 1 2 3 4 5))
(asserteq (factorial-iterative 6) (* 1 2 3 4 5 6))
