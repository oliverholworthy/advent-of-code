(ns advent-of-code.2021.01
  "Day 1: Sonar Sweep"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (map #(Long/parseLong %) (str/split-lines (slurp (io/resource "2021/01.txt")))))

(defn part-one [xs]
  "Count the number of increases of pairs in a sequence"
  (count (filter (fn [[a b]] (> b a)) (partition 2 1 xs))))

(defn part-two [xs]
  "Count the number of increases for sums of a three-measurement
  sliding window over the input sequence XS"
  (part-one (map (partial reduce +) (partition 3 1 xs))))

(comment
  (part-one input)
  (part-two input)
  )
