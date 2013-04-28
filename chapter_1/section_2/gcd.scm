(use-modules (util unittest))

(define (gcd first second)
  (if (zero? second)
      first
      (gcd second 
	   (remainder first 
		      second))))

(asserteq (gcd 12 18) 6)
(asserteq (gcd 16 20) 4)
(asserteq (gcd 35 56) 7)
(asserteq (gcd 206 40) 2)


;; applicative order
;; (gcd 206 40)
;; (gcd 40 6)
;; (gcd 6 4)
;; (gcd 4 2)
;; (gcd 2 0) 
;; 2

;; normal order
;; (gcd 206 40)
