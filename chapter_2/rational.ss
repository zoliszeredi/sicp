(define (sign x)
  (cond ((< x 0) -1)
	((= x 0) 0)
	(else 1)))

(define (sign-rational r)
  (let ((p (numerator r))
	(q (denominator r)))
    (* (sign p)
       (sign q))))

(define (numerator r)
  (car r))

(define (denominator r)
  (cdr r))

(define (make-rational p q)
  (if (= q 0)
      'NaN
      (cons p q)))

(define (simpl-rational r)
  (let ((a (abs (numerator r)))
	(b (abs (denominator r)))
	(its-sign (sign-rational r)))
    (let ((its-gcd (gcd a b)))
      (let ((p (* its-sign
		  (/ a its-gcd)))
	    (q (/ b its-gcd)))
	(make-rational p q)))))

(define (eq-rational first second)
  (let ((first (simpl-rational first))
	(second (simpl-rational second)))
    (let ((a (numerator first))
	  (b (denominator first))
	  (c (numerator second))
	  (d (denominator second)))
      (and (= a c)
	   (= b d)))))

(define (inv-rational r)
  (let ((p (numerator r))
	(q (denominator r)))
    (make-rational q p)))

(define (neg-rational r)
  (let ((p (numerator r))
	(q (denominator r)))
    (make-rational (- p) q)))

(define (mul-rational first second)
  (let ((a (numerator first))
	(b (denominator first))
	(c (numerator second))
	(d (denominator second)))
    (let ((p (* a c))
	  (q (* b d)))
	  (make-rational p q))))

(define (div-rational first second)
  (mul-rational first 
		(inv-rational second)))

(define (add-rational first second)
  (let ((a (numerator first))
	(b (denominator first))
	(c (numerator second))
	(d (denominator second)))
    (let ((p (+ (* a d)
		(* b c)))
	  (q (* b d)))
      (make-rational p q))))

(define (sub-rational first second)
  (add-rational first
		(neg-rational second)))
