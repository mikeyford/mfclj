(ns mfclj.time)


(defn window-average [ts window weights centre]
  "TODO: handle nils for double moving average"
  (let [k (/ (dec window) 2)
        upper (+ centre k)
        lower (- centre k)]
    (if (and (>= lower 0)
             (< upper (count ts)))
      (->> (map #(nth ts %) (range lower (inc upper)))
           (map * weights)
           (reduce +))
      nil)))


(defn sma [ts window]
  "Simple Moving Average"
  (let [even-weights (map #(/ % window) (repeat window 1))]
    (map #(window-average ts window even-weights %) (range (count ts)))))


(defn wma [ts weights]
  "Weighted Moving Average"
  (assert (= (reduce + weights) 1))
  (let [window (count weights)]
    (map #(window-average ts window weights %) (range (count ts)))))
