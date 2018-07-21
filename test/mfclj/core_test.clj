(ns mfclj.core-test
  (:require [clojure.test :refer :all]
            [mfclj.core :refer :all]))

(deftest num-map-test
  (testing
    (is (= {:a 1, :b -0.1} (num-map {:a 1, :b -0.1, :c "not a number"})))))


(deftest get-env-test
  (let [x 1
        y "string"]
    (def let-bindings (get-env)))
  (testing
    (is (= (:x let-bindings) 1))
    (is (= (:y let-bindings) "string"))))

