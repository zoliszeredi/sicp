(define (+ a b)
  (if (= a 0)
      b
      (1+ (+ (1- b)))))

;; this one is recursive

(define (+ a b)
  (if (= a 0)
      b
      (+ (1- a) (1+ b))))

;; this one is iterative
