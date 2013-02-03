(define (range start stop)
  (define (range-iter out-list iter)
    (if (= iter 0)
	out-list
	(range-iter (cons iter out-list)
		    (- iter 1))))
  (range-iter '() stop))

(define (reverse in-list)
  (define (reverse-iter in-list out-list)
    (if (null? in-list)
	out-list
	(reverse-iter (cdr in-list)
		      (cons (car in-list)
			    out-list))))
  (reverse-iter in-list '()))
