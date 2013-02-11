(define (first-denomination kinds-of-coints)
  (cond ((= kinds-of-coints 1) 1)
	((= kinds-of-coints 2) 5)
	((= kinds-of-coints 3) 10)
	((= kinds-of-coints 4) 25)
	((= kinds-of-coints 5) 50)))

(define (cc amount kinds-of-coints)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coints 0)) 0)
	(else (+ (cc amount 
		     (- kinds-of-coints 1))
		 (cc (- amount
			(first-denomination kinds-of-coints))
		     kinds-of-coints)))))

(define (count-change amount)
  (cc amount 5))

;; (load "../../util/unittest.ss")
;; (asserteq (count-change 100) 292)

;; count-change for 11 cents
;; (cc 11 3)
;; -----------------
;; (+ (cc 11 2)
;;    (cc 1 3))
;; -----------------
;; (+ (+ (cc 11 1)
;;       (cc 6 2))
;;    (+ (cc 1 2)
;;       (cc -9 3))) 
;; -----------------
;; (+ (+ (+ (cc 11 0)
;;          (cc 10 1))
;;       (+ (cc 6 1)
;;          (cc 1 2))
;;    (+ (+ (cc 1 1)
;;          (cc -4 2))
;;       0)
;; -----------------
;; (+ (+ (+ 0
;;          (+ (cc 10 0)
;;             (cc 9 1))
;;       (+ (+ (cc 6 0)
;;             (cc 5 1))
;;          (+ (cc 1 1)
;;             (cc -4 2))
;;    (+ (cc 1 0)
;;       (cc 0 1)))
;; ------------------
;; (+ (+ (cc 9 1)
;;       (cc 5 1))
;;    (+ (cc 1 1)
;;       (cc 1 0)))
;; ------------------
;; (+ (+ (cc 8 1)
;;       (cc 4 1)
;;    (+ (cc 0 1)
;;       1))
;; ------------------
;; (+ (+ 1 1)
;;    (+ 1 1))
;; -----------------
;; 4

