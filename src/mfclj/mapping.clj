(ns mfclj.mapping)

(defn num-map [map]
  "Returns input map where values are numbers"
  (into {} (filter (fn [x] (number? (second x))) map)))

