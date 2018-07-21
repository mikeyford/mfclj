(ns mfclj.core
  (:gen-class))

(defn num-map [map]
  "Returns input map where values are numbers"
  (into {} (filter (fn [x] (number? (second x))) map)))


(defmacro get-env []
  "Returns map of local bindings, with symbols as keywords"
  (let [bindings (for [k (keys &env)]
                   [(name k) k])]
    (zipmap (map #(keyword (first %)) bindings)
            (map #(second %) bindings))))

