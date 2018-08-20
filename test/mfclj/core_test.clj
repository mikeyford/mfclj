(ns mfclj.core-test
  (:require [clojure.test :refer :all]
            [mfclj.core :refer :all]))

(deftest num-map-test
  (let [example {:a 1, :b -0.1, :c "4.5", :d "not a number"}]
  (testing
    (is (= {:a 1, :b -0.1} (num-map example)))
    (is (= {:a 1, :b -0.1, :c "4.5"} (num-map example true))))))


(deftest get-env-test
  (let [x 1
        y "string"]
    (def let-bindings (get-env)))
  (testing
    (is (= (:x let-bindings) 1))
    (is (= (:y let-bindings) "string"))))


(deftest numeric?-test
  (testing
    (is (= true (numeric? "01")))
    (is (= true (numeric? "-443.3141")))
    (is (= true (numeric? "-0.0001")))
    (is (= false (numeric? "-1341.41.41")))
    (is (= false (numeric? "not a number")))))


(deftest update-vals-test
  (testing
    (is (= {:a 1} (update-vals {:a 2} #(/ % 2))))
    (is (= {:b 2 :c 3} (update-vals {:b 1 :c 2} inc)))))
