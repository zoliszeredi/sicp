(defn smallest-divisor [number]
     (defn divides? [divisor]
	  (= (mod number divisor) 0))
     (defn too-large? [divisor]
	  (> (* divisor divisor) number))
     (defn try-divisor [divisor]
       (if (too-large? divisor)
         number
         (if (divides? divisor)
           divisor
           (try-divisor (+ divisor 1)))))
     (try-divisor 2))

(defn prime? [number]
  (and (= number (smallest-divisor number))
       (> number 1)))


(filter prime? (range 1 100000))
      

;; (smallest-divisor 97)
;; should be 9

