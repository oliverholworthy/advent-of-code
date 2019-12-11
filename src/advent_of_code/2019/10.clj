(ns advent-of-code.2019.10
  "Day 10: Monitoring Station"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (slurp (io/resource "2019/10/input.txt")))

(defn parse-input [input]
  (mapv (fn [line] (mapv (fn [c] (str c)) line)) (str/split-lines input)))

(defn atan2 [[y1 x1] [y2 x2]]
  (mod (Math/atan2 (- x2 x1) (- y1 y2))
       (* 2 Math/PI)))

(defn v-op [op]
  (fn [v1 v2]
    (let [[y1 x1] v1
          [y2 x2] v2]
      [(op y1 y2) (op x2 x1)])))

(def v-sub (v-op -))

(defn visible-count [asteroid-coords coord]
  (->> asteroid-coords
       (remove #{coord})
       (map (partial atan2 coord))
       (distinct)
       (count)))

(defn best-position [asteroid-coords]
  (first (sort-by second > (map (juxt identity (fn [coord] (visible-count asteroid-coords coord))) asteroid-coords))))

(defn asteroid-coords [m]
  (filter (fn [c] (= (get-in m c) "#")) (map-coords m)))

(defn distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- y2 y1)) (Math/abs (- x2 x1))))

(defn part-two [input]
  (let [asteroids (asteroid-coords (parse-input input))
        [center _] (best-position asteroids)
        groups (group-by (partial atan2 center) asteroids)
        sweeped (map (fn [[_ cs]]
                       (first (sort-by (partial distance center) > cs)))
                     groups)
        asteroids
        (->> sweeped
             (sort-by (fn [b] (atan2 center b))))
        [y x] (nth asteroids 199)]
    (+ y (* 100 x))))

(comment
  (best-position (asteroid-coords (parse-input input)))
  (part-two input)
  )
