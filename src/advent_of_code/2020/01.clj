(ns advent-of-code.2020.01
  "Day 1: Report Repair"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/01/input.txt"))))

(defn numbers-sum-to [numbers target-sum]
  (let [xs (into #{} numbers)]
    (filter (fn [x] (contains? xs (- target-sum x))) xs)))

(defn part-one [numbers target-sum]
  (apply * (numbers-sum-to numbers target-sum)))

(defn part-two [numbers target-sum]
  (let [xs (into #{} numbers)]
    (->> xs
         (map (fn [x] (sort (cons x
                                 (numbers-sum-to (disj xs x)
                                                 (- target-sum x))))))
         (filter (fn [ys] (> (count ys) 1)))
         (distinct)
         (flatten)
         (apply *))))

(comment
  (def input-numbers (map #(Long/parseLong %) input))
  (part-one input-numbers 2020)
  (part-two input-numbers 2020)
  )
