(ns mfclj.core
  (:gen-class))

(defn num-map [m]
  "Returns input map where values are numbers"
  (into {} (filter #(number? (second %))) m))


(defmacro get-env []
  "Returns map of local bindings, with symbols as keywords"
  (let [b (for [k (keys &env)]
            [(name k) k])]
    (zipmap (map #(keyword (first %)) b)
            (map #(second %) b))))

