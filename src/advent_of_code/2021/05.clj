(ns advent-of-code.2021.05
  "Day 5: Hydrothermal Venture"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "2021/05.txt"))))
(def input-sample (str/split-lines (slurp (io/resource "2021/05.sample.txt"))))

(defn coerce-long [s] (Long/parseLong s))

(defn parse-input [lines]
  (map (fn [line] (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)]
                   {:x1 (coerce-long x1) :x2 (coerce-long x2)
                    :y1 (coerce-long y1) :y2 (coerce-long y2)}))
       lines))

(defn coord-range [x1 x2]
  (range x1
         (if (> x1 x2) (dec x2) (inc x2))
         (if (> x1 x2) -1 1)))

(defn line-coords [{:keys [x1 x2 y1 y2]}]
  (let [y-diff (Math/abs (- y2 y1))
        x-diff (Math/abs (- x2 x1))
        grad (when (> x-diff 0)
               (/ (- y2 y1) (- x2 x1)))
        xs (if (>= x-diff y-diff)
             (coord-range x1 x2)
             (map (fn [y] (if grad (+ (/ y grad) x1) x1)) (coord-range y1 y2)))
        ys (if (>= y-diff x-diff)
             (coord-range y1 y2)
             (map (fn [x] (if grad (+ (* x grad) y1) y1)) (coord-range x1 x2)))]
    (map vector xs ys)))

(defn is-straight? [{:keys [x1 x2 y1 y2]}]
   (or (= x1 x2) (= y1 y2)))

(defn part-one [input]
  (let [vent-coords (parse-input input)
        vent-coords (filter is-straight? vent-coords)
        coords (map line-coords vent-coords)]
    (count (filter (fn [[coord freq]] (>= freq 2))
                   (frequencies (apply concat coords))))))

(defn part-two [input]
  (let [vent-coords (parse-input input)
        coords (map line-coords vent-coords)]
    (count (filter (fn [[coord freq]] (>= freq 2))
                   (frequencies (apply concat coords))))))

(comment
  (part-one input-sample)
  (part-one input)
  (part-two input-sample)
  (part-two input)
  )
