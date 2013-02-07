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

(load "../../util/unittest.ss")

(asserteq (count-change 100) 292)
