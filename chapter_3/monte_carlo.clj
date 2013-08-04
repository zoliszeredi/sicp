(defn random-in-range
  [low high]
  (+ (rand (- high low))
     low))

(defn unit-rectangle-rand 
  []
  (random-in-range -1.0 1.0))

(defn square 
  [x] 
  (* x x))

(defn inside-circle?
  [x y]
  (<= 1.0
      (+ (square x)
         (square y))))

(defn circle-test
  []
  (inside-circle? (unit-rectangle-rand)
                  (unit-rectangle-rand)))

(defn monte-carlo 
  [number-of-trials test-function]
  (loop [passed-trials 0
         trial-counter number-of-trials]
    (cond
     (zero? trial-counter) (/ passed-trials number-of-trials)
     (test-function) (recur (inc passed-trials) (dec trial-counter))
     :else (recur passed-trials (dec trial-counter)))))
                            
(defn aproximate-pi-integration
  [trials]
  (- 4.0
     (monte-carlo trials circle-test)))

(def gcd
  (fn [x y]
    (if (zero? y)
      x
      (recur y (rem x y)))))

(defn cesaro-test
  []
  (= 1 (gcd (rand-int 10000)
            (rand-int 10000))))

(defn aproximate-pi-cesaro
  [trials]
  (Math/sqrt    
   (/ 6.0
      (monte-carlo trials
                   cesaro-test))))))
