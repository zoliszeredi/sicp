(load "../../util/unittest.ss")

(define (+ a b)
  (if (= a 0)
      b
      (1+ (+ (1- a) b))))
(asserteq (+ 4 5) 9)
;; this one is recursive

(define (+ a b)
  (if (= a 0)
      b
      (+ (1- a) (1+ b))))
(asserteq (+ 4 5) 9)
;; this one is iterative
