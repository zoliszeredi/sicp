(use-modules (util unittest))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (sqrt-iter guess x)
;;  (new-if (is-guess-good-enough guess x)
  (if (is-guess-good-enough guess x)
      guess
      (sqrt-iter (improve-guess guess x) x)))

(define (improve-guess guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (is-guess-good-enough guess x)
  (< (abs (- x (square guess))) 0.00001))

(define (square x)
  (* x x))

(define (cbrt x)
  (define (cube x)
    (* x x x))
  (define (good-enough y)
    (< (abs (- x (cube y)))
       0.00001))
  (define (improve y)
    (/ (+ (/ x (* y y)) (* 2 y)) 3))
  (define (cbrt-iter guess)
    (if (good-enough guess) 
	 guess
	 (cbrt-iter (improve guess))))
  (cbrt-iter 1.0))

(asserteq (sqrt 2) 1.414213)
(asserteq (cbrt 8) 2.000000)
