(ns advent-of-code.2021.22
  "Day 22: Reactor Reboot"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [s]
  (mapv (fn [line]
          (let [[_ on-off x1 x2 y1 y2 z1 z2]
                (re-matches #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)"
                            line)
                parse-coord (fn [c] (mapv #(Long/parseLong %) c))]
            {:toggle (keyword on-off)
             :x (parse-coord [x1 x2])
             :y (parse-coord [y1 y2])
             :z (parse-coord [z1 z2])}))
        (str/split-lines (str/trim s))))

(def input-sample (parse-input (slurp (io/resource "2021/22.sample.txt"))))
(def input (parse-input (slurp (io/resource "2021/22.txt"))))

(defn coord-range [[a b]] (range (max -50 a) (inc (min 50 b))))

(defn cuboid [{:keys [x y z]}]
  (for [c1 (coord-range x)
        c2 (coord-range y)
        c3 (coord-range z)]
    [c1 c2 c3]))

(defn next-step [cubes step]
  (let [target-cubes (into #{} (cuboid step))]
    (if (= (:toggle step) :on)
      (set/union cubes target-cubes)
      (set/difference cubes target-cubes))))

(defn part-one [steps]
  (count (reduce (fn [cubes step] (next-step cubes step)) #{} steps)))

(defn coord-diff [[x1 x2] [x3 x4]]
  (let [d1 (max x1 x3) d2 (min x2 x4)]
    (when (<= d1 d2)
      [d1 d2])))

(defn difference [a b]
  (let [x (coord-diff (:x a) (:x b))
        y (coord-diff (:y a) (:y b))
        z (coord-diff (:z a) (:z b))]
    (if (and x y z)
      {:toggle (if (= (:toggle b) :on) :off :on) :z z :x x :y y})))

(defn differences [cuboids step]
  (reduce (fn [acc cuboid]
            (if-let [diff (difference step cuboid)]
              (conj acc diff)
              acc))
          [] cuboids))

(defn volume [{:keys [x y z toggle]}]
  (let [[x1 x2] x [y1 y2] y [z1 z2] z]
    (* (if (= toggle :on) 1 -1)
       (- (inc x2) x1) (- (inc y2) y1) (- (inc z2) z1))))

(defn part-two [steps]
  (->> steps
       (reduce (fn [cuboids step]
                 (concat (cond->> cuboids
                           (= (:toggle step) :on)
                           (cons step))
                         (differences cuboids step)))
               '())
       (map volume)
       (reduce +)))

(comment
  (time (part-one input))
  (time (part-two input))
  )
