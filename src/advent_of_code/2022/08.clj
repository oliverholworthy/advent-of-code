(ns advent-of-code.2022.08
  "Day 8: Treetop Tree House"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "2022/08.txt"))))
(def input-sample (str/split-lines (slurp (io/resource "2022/08.sample.txt"))))

(defn parse-grid [lines] (mapv (fn [line] (mapv (fn [c] (Long/parseLong (str c))) line)) lines))

(defn v+
  "Adds two grid coordinates together"
  [v1 v2]
  (mapv + v1 v2))

(defn coords
  "returns all coordinates in a 2D grid"
  [grid]
  (let [y (count grid)
        x (count (get grid 0 []))]
    (for [y (range y)
          x (range x)]
      [y x])))

(def max-height
  "maximum height "
  (memoize
   (fn
     [grid coord dir]
     (if-let [x (get-in grid coord)]
       (max x
            (max-height grid (v+ coord dir) dir))
       0))))

(defn visible?
  "Returns bool value indicating whether or not the tree is visible from
  outside the grid."
  [grid coord]
  (let [tree-height (get-in grid coord)
        grid-max-y (dec (count grid))
        grid-max-x (dec (count (get grid 0 [])))]
    (or (let [[y x] coord]
          (or (= y 0) (= x 0)
              (= y grid-max-y)
              (= x grid-max-x)))
        (reduce #(or %1 %2)
                (map (fn [dir]
                       (> tree-height (max-height grid (v+ coord dir) dir)))
                     [[0 1] [0 -1] [1 0] [-1 0]])))))

(defn part-one
  "Number of trees visible from outside the grid"
  [input]
  (let [grid (parse-grid input)]
    (count (filter true? (map (partial visible? grid) (coords grid))))))

(defn viewing-distance-from-height
  "The viewing distance at a grid coordinate in a direction from a height."
  [grid coord dir viewing-height]
  (if-let [coord-height (get-in grid coord)]
    (if (> viewing-height coord-height)
      (inc (viewing-distance-from-height grid (v+ coord dir) dir viewing-height))
      1)
    0))

(defn viewing-distance
  "The viewing distance at a grid coordinate in a direction."
  [grid coord dir]
  (viewing-distance-from-height grid (v+ coord dir) dir (get-in grid coord)))

(defn scenic-score
  "Returns a tree's scenic score. Found by multiplying together its
   viewing distance in each of the four directions."
  [grid coord]
  (reduce * (map (fn [dir] (viewing-distance grid coord dir))
                 [[0 1] [0 -1] [1 0] [-1 0]])))

(defn part-two
  "The highest scenic score possible for any tree in the grid"
  [input]
  (let [grid (parse-grid input)]
    (reduce max (map (partial scenic-score grid) (coords grid)))))

(comment
  (part-one input-sample)
  (part-one input)
  (part-two input-sample)
  (part-two input)
  )
