(ns advent-of-code.2022.03
  "Day 3: Rucksack Reorganization"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (str/split-lines (slurp (io/resource "2022/03.txt"))))

(defn compartment-shared [items]
  (let [items-one (subs items 0 (/ (count items) 2))
        items-two (subs items (/ (count items) 2) (count items))]
    (first (set/intersection (set items-one) (set items-two)))))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def item-priority
  (->> (concat (char-range \a \z) (char-range \A \Z))
       (map-indexed (fn [i c] [c (inc i)]))
       (into {})))

(defn part-one [input]
  "Sum of the priorities of items in both rucksack compartments"
  (reduce + (map (comp item-priority compartment-shared) input)))

(defn part-two [input]
  "Sum of priorities of items shared between groups of elves"
  (->> input
       (partition 3) ;; group size
       (map (fn [group] (first (apply set/intersection (map set group)))))
       (map item-priority)
       (reduce +)))

(comment
  (part-one input)
  (part-two input)
  )
