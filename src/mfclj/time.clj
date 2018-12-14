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


(defn nil-subtract [a b]
  (if (or (= a nil) (= b nil))
    nil
    (- a b)))

(defn period-avg [detrend mod-map period]
  (let [indexes (map first (filter #(= period (val %)) mod-map))
        vals-with-nil (map #(nth detrend %) indexes)
        vals (filter #(not (= nil %)) vals-with-nil)]
    {period (/ (reduce + vals) (count vals))}))

(defn decomposition [ts period]
  "TODO take ags and remap to pattern over full ts
  Calculate remainder component
  Return all ts, trend cycle, seasonal and remainder in map."
  (let [trend-cycle (sma ts period)
        detrend (map nil-subtract ts trend-cycle)
        index (range (count ts))
        mods (zipmap index (map #(mod % period) index))
        avgs (reduce conj (map #(period-avg detrend mods %) (range period)))]
    avgs)
  )