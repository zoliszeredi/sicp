(use clojure.contrib.math)

(defn cesaro-test []
  (let [maxint 10000000 first (rand-int maxint) second (rand-int maxint)]
    (= (gcd first second) 1)))

(defn monte-carlo [test trials]
  "Runs the monte-carlo algorithm. Returns the fraction of trials that passed"
  (loop [successul 0 remaining trials]
    (cond 
     (zero? remaining) (/ successul trials)
     (test) (recur (inc successul) (dec remaining))
     :else (recur successul (dec remaining)))))

(defn aproximate-pi []
  (let [result (* 1.0 (monte-carlo cesaro-test 500000))]
    (/ 6.0 (sqrt result))))

(defn -main
  (aproximate-pi))

