(ns advent-of-code.2021.07
  "Day 7: The Treachery of Whales"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [s] (mapv #(Long/parseLong %) (str/split (str/trim s) #",")))

(def input (parse-input (slurp (io/resource "2021/07.txt"))))

(defn alignment-cost [xs cost-fn]
  (apply min (map (fn [i] (reduce + (map (cost-fn i) xs)))
                  (range (apply min xs) (apply max xs)))))

(defn part-one [xs] (alignment-cost xs (fn [i] (fn [x] (Math/abs (- x i))))))

(defn sum-of-ints [n] (/ (* n (+ n 1)) 2))

(defn part-two [xs] (alignment-cost xs (fn [i] (fn [x] (sum-of-ints (Math/abs (- x i)))))))

(comment
  (part-one (parse-input "16,1,2,0,4,2,7,1,2,14"))
  (part-one input)
  (part-two (parse-input "16,1,2,0,4,2,7,1,2,14"))
  (part-two input)
  )
