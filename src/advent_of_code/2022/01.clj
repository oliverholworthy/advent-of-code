(ns advent-of-code.2022.01
  "Day 1: Calorie Counting"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "2022/01.txt"))))

(defn elf-calories [input-lines]
  (->> input-lines
       (partition-by empty?)
       (map (fn [xs] (map #(Long/parseLong %) (remove empty? xs))))
       (remove empty?)
       (map #(reduce + %))))

(defn part-one [input-lines]
  "Calories carried by the Elf carrying the most"
  (->> input-lines
       (elf-calories)
       (reduce max)))

(defn part-two [input-lines]
  "Total Calories carried by the top three Elves carrying the most"
  (->> input-lines
       (elf-calories)
       (sort >)
       (take 3)
       (reduce +)))

(comment
  (part-one input)
  ;; => 66616

  (part-two input)
  ;; => 199172
  )
