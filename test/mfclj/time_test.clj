(ns mfclj.time-test
  (:require [clojure.test :refer :all]
            [mfclj.time :refer :all]))



(deftest ma-test
  (let [ts '(1 2 3)]
    (testing
    (is (= (window-average ts 3 [1/3 1/3 1/3] 1) 2)))))


(deftest sma-test
  (let [ts '(1 2 3 4 5)]
    (testing
      (is (= (sma ts 3) '(nil 2 3 4 nil))))))

(deftest sma-test
  (let [ts '(1 2 3 4 5)]
    (testing
      (is (= (sma ts 3) '(nil 2 3 4 nil))))))


(deftest sma-test
  (let [ts '[1 2 3 4 5 6 7 8]]
    (testing
      (is (= (wma ts '(1/8 1/4 1/4 1/4 1/8)) '(nil nil 3 4 5 6 nil nil))))))