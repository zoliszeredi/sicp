(define (p) (p))
;; This is a function that when called, will call itself recurisely and will
;; enter an infinite loop.

(define (test x y)
  (if (= x 0)
      0
      y))

;; Test will behave differently depending on what evaluation the interpreter 
;; uses.
;; The difference in evaluation between normal-order and apllicative-order 
;; evaluation(the latter is also called lazy-evaluation) is that it evaluates
;; expressions only when necessary but at the cost of multiple similair calls.

;; Case 1. Normal-Order (lazy)
;;
;; (test 0 (p))
;; (= 0 0) evaluates to 0 

;; Case 2. Apllicative-Order
;;
;; (test 0 (p))
;; Evaluates all operands before the call. Since 0 is evaluated, the next one 
;; is to evaluate *p* which enters an infinite loop



