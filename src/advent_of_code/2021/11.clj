(ns advent-of-code.2021.11
  "Day 11: Dumbo Octopus"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [s] (mapv (fn [line] (mapv (fn [c] (Long/parseLong (str c))) line)) (str/split-lines s)))
(def input (parse-input (slurp (io/resource "2021/11.txt"))))
(def input-sample (parse-input (slurp (io/resource "2021/11.sample.txt"))))

(defn coords [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    [y x]))

(defn neighbours [grid [y x]]
  "Find immediate neigbours "
  (reduce (fn [acc coord]
            (if-let [node (get-in grid coord)]
              (conj acc coord)
              acc))
          []
          [[(dec y) (dec x)]
           [(inc y) (inc x)]
           [(inc y) (dec x)]
           [(dec y) (inc x)]
           [y (dec x)]
           [(dec y) x]
           [(inc y) x]
           [y (inc x)]]))

(defn increment-coords [grid inc-coords]
  (reduce (fn [{:keys [grid flashes]} coord]
            (let [energy-level (get-in grid coord)
                  new-energy-level (inc energy-level)]
              {:grid (assoc-in grid coord new-energy-level)
               :flashes (if (= energy-level 9) (conj flashes coord) flashes)}))
          {:grid grid :flashes []}
          inc-coords))

(defn zero-coords [grid]
  (reduce (fn [acc coord] (if (zero? (get-in grid coord))
                           (conj acc coord)
                           acc))
          [] (coords grid)))

(defn step [grid]
  (let [{:keys [grid flashes]} (increment-coords grid (coords grid))]
    (loop [grid grid
           frontier flashes
           flashed flashes]
      (if (empty? frontier)
        (reduce (fn [acc coord] (update-in acc coord (fn [v] (if (> v 9) 0 v)))) grid (coords grid))
        (let [{:keys [grid flashes]} (increment-coords grid (neighbours grid (first frontier)))]
          (recur grid
                 (concat (rest frontier) flashes)
                 (concat frontier flashes)))))))

(defn part-one [grid]
  "total flashes after 100 steps"
  (reduce + (map (comp count zero-coords) (take (inc 100) (iterate step grid)))))

(defn part-two [grid]
  "the first step during which all octopuses flash"
  (let [n (count (coords grid))]
    (ffirst (drop-while (fn [[i grid]] (not (= n (count (zero-coords grid)))))
                       (map-indexed vector (iterate step grid))))))

(comment
  (part-one input)
  (part-two input)
  )
