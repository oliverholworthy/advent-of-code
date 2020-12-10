(ns advent-of-code.2020.10
  "Day 10: Adapter Array"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (map #(Long/parseLong %)
       (str/split-lines
        (slurp (io/resource "2020/10/input.txt")))))

(def input-sample
  (map #(Long/parseLong %)
       (str/split-lines
        (slurp (io/resource "2020/10/sample.txt")))))

(defn joltage-differences [xs]
  "Returns joltage differences between list of adapter values"
  (concat (->> (partition 2 1 (cons 0 (sort xs)))
               (map (fn [[a b]] (- b a))))
          [3]))

(defn tribonacci [i]
  "Returns i'th Tribonacci number"
  (last (nth (iterate (fn [[t1 t2 t3]] [t2 t3 (+ t1 t2 t3)])
                      [0 1 1])
             i)))

(defn adapter-combinations [xs]
  "How many combinations are there for a sequence of 1s allowing a
  maximum separation of 3"
  (if (= (first xs) 1)
    (tribonacci (dec (count xs)))
    1))

(defn adapter-arangements [xs]
  "The total number of distinct ways you can arrange the adapters"
  (->> (joltage-differences xs)
       (partition-by #(= 1 %))
       (map adapter-combinations)
       (reduce *)))

(comment
  ;; Part One
  (apply * (vals (frequencies (joltage-differences input))))

  ;; Part Two
  (adapter-combinations [1 1 1])
  (adapter-arangements input-sample)
  (adapter-arangements input)
  )
