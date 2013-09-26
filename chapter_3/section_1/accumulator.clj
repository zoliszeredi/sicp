(defn accumulator [x]
  (let [current (ref 0)]
    (fn [x]
      (dosync
       (ref-set current (+ (deref current ) x)))
      (deref current))))

(defn main []
  (let [A (accumulator 5)]
    (A 10)
    (A 10)))
