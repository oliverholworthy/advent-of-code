(ns advent-of-code.2018.10
  "Day 10: The Stars Align"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/10/input.txt"))))

(def input-lines-sample
  (str/split-lines
   (slurp (io/resource "2018/10/input-sample.txt"))))

(defn parse-line [line]
  (let [[_ px py vx vy]
        (re-find #"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>"
                 line)]
    [[(Long/parseLong px) (Long/parseLong py)]
     [(Long/parseLong vx) (Long/parseLong vy)]]))

(defn get-position [^long time
                    [[^long px ^long py]
                     [^long vx ^long vy]]]
  [(+ px (* vx time))
   (+ py (* vy time))])

(defn grid [points time]
  (map (partial get-position time) points))

(defn grid-bounds [grid]
  (let [[min-x max-x] (apply (juxt min max) (map first grid))
        [min-y max-y] (apply (juxt min max) (map second grid))]
    [[min-x min-y]
     [max-x max-y]]))

(defn grid-dim [grid]
  (let [[[min-x min-y] [max-x max-y]]
        (grid-bounds grid)]
    [(- max-x min-x)
     (- max-y min-y)]))

(defn grid-summary [points time]
  (let [g (grid points time)
        [w h] (grid-dim g)]
    {:time time :width w :height h :grid g}))

(defn find-message [points]
  (loop [time 0
         min-w nil
         min-h nil]
    (let [g (grid points time)
          [w h] (grid-dim g)]
      (cond (zero? time) (recur (inc time) w h)
            (and (> h min-h) (> w min-w)) (grid-summary points (dec time))
            :else (recur (inc time) (min min-w w) (min min-h h))))))

(defn print-grid [grid]
  (let [grid-points (set grid)
        [[min-x min-y] [max-x max-y]] (grid-bounds grid)]
    (str/join "\n"
              (for [y (range (dec min-y) (inc (inc max-y)))]
                (str/join
                 (for [x (range (dec min-x) (inc (inc max-x)))]
                   (if (contains? grid-points [x y])
                     "#"
                     ".")))))))

(comment
  (def points-sample (mapv parse-line input-lines-sample))
  (time (def res-sample (find-message points-sample)))
  (print (print-grid (:grid res-sample)))

  (def points (mapv parse-line input-lines))
  (time (def res (find-message points)))
  (print (print-grid (:grid res)))
  )
