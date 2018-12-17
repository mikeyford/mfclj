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


(defn nil-operate [a b operator]
  (if (or (= a nil) (= b nil))
    nil
    (operator a b)))

(defn period-avg [detrend mod-map period]
  "TODO fix error , looks like empty seq being passed in"
  (let [indexes (map first (filter #(= period (val %)) mod-map))
        vals-with-nil (map #(nth detrend %) indexes)
        vals (filter #(not (= nil %)) vals-with-nil)]
    {period (/ (reduce + vals) (count vals))}))

(defn remap-avgs [avgs period length]
  (let [base (->> (repeatedly #(range period))
                  (flatten)
                  (take length))]
    (map avgs base)))


(defn decomposition [ts period]
  "TODO take ags and remap to pattern over full ts
  Calculate remainder component
  Return all ts, trend cycle, seasonal and remainder in map."
  (let [trend-cycle (sma ts period)
        detrend (map nil-operate ts trend-cycle -)
        index (range (count ts))
        mods (zipmap index (map #(mod % period) index))
        avgs (reduce conj (map #(period-avg detrend mods %) (range period)))
        seasonal (remap-avgs avgs period (count ts))
        remainder (map nil-operate detrend seasonal -)]
    {:ts ts
     :trend trend-cycle
     :seasonal seasonal
     :remainder remainder}))
