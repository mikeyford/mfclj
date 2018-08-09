(ns mfclj.core
  (:gen-class))


(defmacro get-env []
  "Returns map of local bindings, with symbols as keywords"
  (let [b (for [k (keys &env)]
            [(name k) k])]
    (zipmap (map #(keyword (first %)) b)
            (map #(second %) b))))


(defn numeric? [s]
  "Returns true if string input is numeric, or input is number"
  (if (number? s)
    true
    (if-let [s (seq s)]
      (let [s (if (= (first s) \-) (next s) s)
            s (drop-while #(Character/isDigit %) s)
            s (if (= (first s) \.) (next s) s)
            s (drop-while #(Character/isDigit %) s)]
        (empty? s)))))


(defn num-map
  "Returns input map where values are numbers, optional bool arg if func. should return numeric strings"
  ([m] (num-map m false))
  ([m read-strings?]
   (if read-strings?
     (into {} (filter #(numeric? (second %))) m)
     (into {} (filter #(number? (second %))) m))))


(defn sample [n coll]
  "Returns a random n samples or percent of the coll, for integer or float < 1 input respectively"
  (if (integer? n)
    (take n (shuffle coll))
    (take (Math/round (* n (count coll))) (shuffle coll))))


(defn update-val [m f & args]
  "Returns input map with fn applied to each value"
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))


