(ns advent-of-code.2018.22
  "Day 22: Mode Maze"
  (:require [clojure.string :as str]))

(declare erosion-level)

(defn geologic-index [coord depth]
  (let [[y x] coord]
    (cond
      (= [0 0] [y x]) 0
      (zero? y) (* x 16807)
      (zero? x) (* y 48271)
      :else
      (* (erosion-level [y (dec x)] depth)
         (erosion-level [(dec y) x] depth)))))

(def erosion-level
  (memoize
   (fn [coord depth]
     (mod (+ (geologic-index coord depth) depth) 20183))))

(defn region-type [coord depth]
  (case (mod (erosion-level coord depth) 3)
    0 :rocky
    1 :wet
    2 :narrow))

(defn get-grid [target-coord depth]
  (let [[ty tx] target-coord]
    (mapv (fn [y] (mapv (fn [x]
                         (cond (= target-coord [y x]) :target
                               (= [0 0] [y x]) :mouth
                               :else
                               (region-type [y x] depth)))
                       (range (inc tx))))
          (range (inc ty)))))

(defn risk-level [area]
  (reduce +
          (mapcat (fn [row]
                    (map (fn [region] (case region :rocky 0 :wet 1 :narrow 2 0))
                         row))
                  area)))

(defn print-grid [grid]
  (println
   (str/join
    "\n"
    (for [row grid]
      (str/join
       (for [region row]
         (case region
           :rocky "."
           :wet "="
           :narrow "|"
           :target "T"
           :mouth "M")))))))

(comment
  (print-grid (get-grid [10 10] 510))
  (risk-level (get-grid [734 13] 7305))
  )
