(ns advent-of-code.2020.03
  "Day 3: Toboggan Trajectory"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/03/input.txt"))))

(defn coords [start step]
  "get sequence of coords starting at start and incrementing by step"
  (let [[sy sx] step]
    (iterate (fn [[y x]] [(+ y sy) (+ x sx)]) start)))

(defn coord-vals [geo step]
  "Find values in geo map from step incrementing by step.
   With geo map repeating on the x axis infinitely"
  (let [max-x (count (first geo))]
    (reduce (fn [acc [y x]]
              (if-let [v (get-in geo [y (mod x max-x)])]
                (conj acc v)
                (reduced acc)))
            []
            (coords step step))))

(defn count-trees [geo step]
  "Count trees in map represented with a #"
  (get (frequencies (coord-vals geo step)) \#))

(comment
  ;; Part One
  (count-trees input [1 3])

  ;; Part Two
  (apply * (map (partial count-trees input) [[1 1] [1 3] [1 5] [1 7] [2 1]]))
  6708199680
  )

