(define (accumulate start end function next)
  (define (in-range? element)
    (and (>= element start)
      (<= element end)))
  (define (iterate element accumulator)
    (if (in-range? element)
	(iterate (next element) (+ accumulator element))
	accumulator))
  (iterate start 0))

(load "util.ss")

(define (identity x) (x))
(define (inc x) (+ x 1))

(let ((expected 55)
      (actual (accumulate 0 10 identity inc)))
      (asserteq (string-append (number->string expected)
			       " expected but "
			       (number->string actual)
			       " found.")
		expected 
		actual))


