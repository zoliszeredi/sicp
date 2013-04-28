;; This one is really easy, nothing fancy, verify with the interpreter though.

10
;; This will evaluate to the number '10'

(+ 5 3 4)
;; This is simple addition that evaluates to '12'

(- 9 1)
;; 8

(/ 6 2)
;; 3

(+ (* 2 4) (- 4 6))
;; (+ 8 -2)
;; 6

(define a 3)
;; nothing, just binds *a* to 3

(define b (+ a 1))
;; nothing, binds *b* to (+ 3 1) or 4

(+ a b (* a b))
;; (+ 3 4 (* 3 4))
;; (+ 3 4 12)
;; 19

(= a b)
;; #f

(if (and (> b a) (< b (* a b)))
    b
    a)
;; (and (> 4 3) (< 4 (* 3 4)))
;; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; if   a is 4 then 6
;; elif b is 4 then (+ 6 7 3) or 16
;; else 25
;; since *b* is 4, 16

(+ 2 (if (> b a) b a))
;; (+ 2 4)
;; 6

(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))
;; (* b (+ a 1))
;; (* 4 4)
;; 16