;; We have the expression:
;; 5 + 4 + (2 - (3 - (6 + 4/5)))
;;-------------------------------
;;       3 (6 - 2) (2 - 7)

;; This is a big division
;; (/ upper-part lower-part)
(/ 
;; The upper-part is a sum (a big hairy)
   (+ 5 
      4 
      (+ (- 2
	    (- 3
	       (+ 6
		  (/ 4 5))))))
   
;; The lower-part is a product
   (* 3
      (+ 6 -2)
      (+ 2 -7)))

;; evaluates to -37/150

      
	    