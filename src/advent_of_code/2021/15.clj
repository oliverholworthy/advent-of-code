(ns advent-of-code.2021.15
  "Day 15: Chiton"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse-input [s] (mapv (fn [line] (mapv (fn [c] (Long/parseLong (str c))) line)) (str/split-lines s)))
(def input (parse-input (slurp (io/resource "2021/15.txt"))))
(def input-sample (parse-input (slurp (io/resource "2021/15.sample.txt"))))

(defn coords [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    [y x]))

(defn neighbours [grid [y x]]
  (reduce (fn [acc coord]
            (if-let [node (get-in grid coord)]
              (conj acc [coord node])
              acc))
          []
          [[y (dec x)]
           [(dec y) x]
           [(inc y) x]
           [y (inc x)]]))

(defn merge-frontier
  [val-in-result val-in-latter]
  (first (sort-by first < [val-in-result val-in-latter])))

(defn paths
  [grid start]
  ((fn explore [explored frontier]
     (lazy-seq
      (when-let [[v [total-cost previous-vertex]] (peek frontier)]
        (let [path (conj (explored previous-vertex []) v)]
          (cons [v total-cost path]
                (explore (assoc explored v path)
                         (merge-with
                          merge-frontier
                          (pop frontier)
                          (into {}
                                (for [[u coord-risk] (remove (comp explored first) (neighbours grid v))]
                                  {u [(+ total-cost coord-risk) v]})))))))))
   {}
   (priority-map start [0])))

(defn lowest-risk-path [grid start end]
  (first (drop-while (fn [[c _ _]] (not= c end)) (paths grid start))))

(defn lowest-risk-path-top-bottom [grid]
  (let [n (count grid)
        end [(dec n) (dec n)]
        start [0 0]]
    (second (lowest-risk-path grid start end))))

(defn transpose [grid]
  (apply mapv vector grid))

(defn tile-rows [grid n]
  (mapv (fn [row]
          (vec (mapcat (fn [i]
                         (map (fn [x] (let [v (+ x i)]
                                       (if (> v 9)
                                         (mod v 9)
                                         v)))
                              row)) (range n))))
        grid))

(defn tile [grid n]
  (-> grid
      (tile-rows n)
      (transpose)
      (tile-rows n)
      (transpose)))

(comment
  (lowest-risk-path-top-bottom input)
  (lowest-risk-path-top-bottom (tile input 5))
  )
