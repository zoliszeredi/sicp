;; Ex. 3.28
;; Make an or-gate primitive

(define (or-gate a b out)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a) (get-signal b))))
      (after-delay or-gate-delay (lambda ()
				   (set-signal! out new-value))))
    (add-signal-action! a or-action-procedure)
    (add-signal-action! b or-action-procedure))
  'ok)


;; Ex. 3.29 
;; Make an or-gate using and-gate and invertors

(define (or-gate a b out)
  (let ((an (make-wire))
	(bn (make-wire))
	(outn (make-wire)))
    (not-gate a an)
    (not-gate b bn)
    (and-gate an bn outn)
    (not-gate outn out))
  'ok)

;; Ex. 3.30
;; Make a ripple carry adder by chaining full adders
;; Express the time delay in terms of and-gates, or-gates, and inverter.

;; Wire set
;; As <--
;; Bs <--
;; Ss -->
;; C -->

;;  input: a b carry-in
;;  output: sum carry-out
;;
;;  a1 b1 c-in1   a2 b2 c-in2   a3 b3 c-in3
;;  +--------+	 +-------+     	+--------+
;;  |        |	 |       |	|        |
;;  |        |	 |       |	|        |
;;  +--------+	 +-------+	+--------+
;;    c  sum1     c-in1 sum2    c-in2 sum3
;;
;;  (a1 b1 c-in1 sum1 c) --> 
;;  (a2 b2 c-in2 sum2 c-in1) -->
;;  (a3 b3 c-in3 sum3 c-in2) -->
;;   ... -->
;;  (aN bN c-inN sumN c-inN-1)

(define (ripple-carry-adder a-list b-list sum-list c)
  (define (iterate signals)
    (unless (empty? signals)
      (let-values (((a b c-in sum c-out) (values (first signals))))
	(full-adder a b c-in sum c-out))
      (iterate (rest signals))))
  
  (let* ((in-carry-list (map make-wire a-list))
	 (size (length a-list))
	 (out-carry-list (cons c (take in-carry-list (- size 1))))
	 (wires (map list a-list b-list in-carry-list sum-list out-carry-list)))
    (iterate wires)
    (set-signal! (last in-carry-list) 0))
  'ok)
