(ns mfclj.mapping-test
  (:require [clojure.test :refer :all]
            [mfclj.mapping :refer :all]))

(deftest a-test
  (testing
    (is (= {:a 1, :b -0.1} (num-map {:a 1, :b -0.1, :c "not a number"})))))
