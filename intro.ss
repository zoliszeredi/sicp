(define (square x)
  (* x x))


(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (max2 a b)
  (if (< a b) b a))

(define (max3 a b c)
  (max2 (max2 a b) c))

(define (sum-of-squares-of-largest-two a b c)
  (cond ((and (<= a b) (<= a c)) (sum-of-squares b c))
	((and (<= b a) (<= b c)) (sum-of-squares a c))
	((and (<= c a) (<= c b)) (sum-of-squares a b))
	(else "this should never happen")))


;; unittest
(load "util.ss")
(asserteq "square 2" (square 2) 4)
(asserteq "sum-of-squares 4 5" (sum-of-squares 3 4) 25)
(asserteq "f 3" (f 3) 52)
(asserteq "max2 3 5" (max2 3 5) 5)
(asserteq "max2 5 5" (max2 5 5) 5)
(asserteq "max3 3 5 2" (max3 2 3 5) 5)
(asserteq "sum-of-squares-of-largest-two 2 3 5" 
	  (sum-of-squares-of-largest-two 2 3 5) 
	  (sum-of-squares 3 5))
(asserteq "sum-of-squares-of-largest-two 4 4 5" 
	  (sum-of-squares-of-largest-two 4 4 5) 
	  (sum-of-squares 4 5))
(asserteq "sum-of-squares-of-largest-two -2 -1 -3" 
	  (sum-of-squares-of-largest-two -2 -1 -3) 
	  (sum-of-squares -2 -1))
(asserteq "sum-of-squares-of-largest-two 1 1 1" 
	  (sum-of-squares-of-largest-two 1 1 1) 2)