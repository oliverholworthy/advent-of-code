(ns advent-of-code.2018.23
  "Day 23: Experimental Emergency Teleportation"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code.2018.z3 :as z3]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/23/input.txt"))))

(def input-lines-sample
  (str/split-lines
   (slurp (io/resource "2018/23/input-sample.txt"))))

(defn parse-line [line]
  (let [[_ & xs]
        (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)"
                 line)
        [px py pz r] (mapv #(Long/parseLong %) xs)]
    {:p [px py pz]
     :r r}))

(defn manhattan-distance
  [[^long x1 ^long y1 ^long z1]
   [^long x2 ^long y2 ^long z2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))
     (Math/abs (- z2 z1))))

(defn part-one [lines]
  (let [points (mapv parse-line lines)
        strongest (first (sort-by :r > points))]
    (count
     (filter (fn [p]
               (<= (manhattan-distance (:p p) (:p strongest))
                   (:r strongest)))
             points))))

(defn part-two [nanobots]
  (z3/with-context [:model :true]
    (let [x (z3/Int "x")
          y (z3/Int "y")
          z (z3/Int "z")
          in-range-count (z3/Int "sum")
          in-ranges
          (mapv (fn [[i _]]
                  (z3/Int (str "in_range_" i)))
                (map-indexed vector nanobots))
          exprs
          (vec
           (map-indexed
            (fn [i {:keys [p r]}]
              (let [[px py pz] p]
                (z3/EQ (get in-ranges i)
                    (z3/ITE (z3/LE (z3/Plus (z3/zabs (z3/Minus x (z3/Int px)))
                                   (z3/zabs (z3/Minus y (z3/Int py)))
                                   (z3/zabs (z3/Minus z (z3/Int pz))))
                             (z3/Int r))
                         (z3/Int 1)
                         (z3/Int 0)))))
            nanobots))
          exprs (conj exprs (z3/EQ in-range-count
                                (apply z3/Plus in-ranges)))
          dist-from-zero (z3/Int "dist")
          exprs (conj exprs (z3/EQ dist-from-zero
                                (z3/Plus (z3/zabs x)
                                      (z3/zabs y)
                                      (z3/zabs z))))
          opt (apply z3/optimizer exprs)
          mx (z3/maximize opt in-range-count)
          my (z3/minimize opt dist-from-zero)
          status (.Check opt nil)]
      (println opt))))

(comment
  (part-one input-lines)
  (part-two (mapv parse-line input-lines))
  )
