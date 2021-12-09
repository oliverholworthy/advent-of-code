(ns advent-of-code.2021.09
  "Day 9: Smoke Basin"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [s] (mapv (fn [line] (mapv (fn [c] (Long/parseLong (str c))) line)) (str/split-lines s)))
(def input (parse-input (slurp (io/resource "2021/09.txt"))))
(def input-sample (parse-input (slurp (io/resource "2021/09.sample.txt"))))

(defn coords [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    [y x]))

(defn neighbours [grid [y x]]
  "Find immediate neigbours "
  (reduce (fn [acc coord]
            (if-let [node (get-in grid coord)]
              (conj acc [coord node])
              acc))
          []
          [[y (dec x)]
           [(dec y) x]
           [(inc y) x]
           [y (inc x)]]))

(defn low-points [grid]
  (filter (fn [coord]
            (let [c-height (get-in grid coord)]
              (every? (fn [[_ n-height]] (< c-height n-height))
                      (neighbours grid coord))))
          (coords grid)))

(defn part-one [grid]
  (reduce + (map (fn [coord] (inc (get-in grid coord))) (low-points grid))))

(defn basin [grid low-point]
  ((fn explore [explored coord frontier]
     (lazy-seq
      (let [frontier
            (distinct (remove (fn [coord] (contains? explored coord))
                              (concat frontier
                                      (map first (remove (fn [[coord height]] (= height 9))
                                                         (neighbours grid coord))))))]
        (if-let [next (first frontier)]
          (cons coord
                (explore (conj explored coord) next (rest frontier)))
          (list coord)))))
   #{}
   low-point
   '()))

(defn part-two [grid]
  (let [basins (map (partial basin grid) (low-points grid))
        top-three (take 3 (sort-by count > basins))]
    (reduce * (map count top-three))))

(comment
  (part-one input)
  (part-two input)
  )
