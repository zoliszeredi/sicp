(load "../../util/unittest.ss")

(define (pascals-triangle-elem depth width)
  ;; 1
  ;; 1 1
  ;; 1 2 1
  ;; 1 3 3 1
  ;; 1 4 6 4 1
  (define (on-the-edge? depth width)
    (or (= width 0)
	(= width depth)))
  (if (on-the-edge? depth width)
      1
      (+ (pascals-triangle-elem (1- depth) width)
	 (pascals-triangle-elem (1- depth) (1- width)))))

(asserteq 1 (pascals-triangle-elem 1 1))
(asserteq 1 (pascals-triangle-elem 4 0))
(asserteq 2 (pascals-triangle-elem 2 1))
(asserteq 6 (pascals-triangle-elem 4 2))
(asserteq 6 (pascals-triangle-elem 4 2))
(asserteq 70 (pascals-triangle-elem 8 4))