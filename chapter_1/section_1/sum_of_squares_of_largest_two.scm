(use-modules (util unittest))

(define (sum-of-squares-of-largest-two a b c)
  (let ((smallest (min3 a b c)))
    (cond ((= a smallest) (sum-of-squares b c))
	  ((= b smallest) (sum-of-squares a c))
	  ((= c smallest) (sum-of-squares a b)))))

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x)
     (square y)))

(define (min x y)
  (if (< x y) x y))

(define (min3 x y z)
  (min x (min y z)))


(define (test-simple)
  (let ((a 1)
	(b 2)
	(c 3))
    (asserteq (+ (square b)
		 (square c))
	      (sum-of-squares-of-largest-two a b c))))

(define (test-same)
  (let ((a 1)
	(b 2)
	(c 2))
    (asserteq (+ (square b)
		 (square c))
	      (sum-of-squares-of-largest-two a b c))))


(test-simple)
(test-same)

