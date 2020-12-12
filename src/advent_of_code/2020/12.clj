(ns advent-of-code.2020.12
  "Day 12: Rain Risk"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/12/input.txt"))))

(defn parse-line [line]
  (let [[_ action value] (re-matches #"([A-Z])(\d+)" line)]
    {:action action :value (Long/parseLong value)}))

(defn v-op [op]
  (fn [v1 v2]
    (let [[x1 y1] v1
          [x2 y2] v2]
      [(op x1 x2) (op y1 y2)])))

(def v+ (v-op +))
(def v- (v-op -))
(def v* (v-op *))

(defn rotate [coord angle]
  (let [[y x] coord
        r (Math/toRadians angle)]
    [(+ (* x (Math/sin r)) (* y (Math/cos r)))
     (- (* x (Math/cos r)) (* y (Math/sin r)))]))

(defn move [coord angle value]
  (let [[y x] coord
        r (Math/toRadians angle)]
    [(+ (* -1 value (Math/cos r)) y)
     (+ (* value (Math/sin r)) x)]))

(defn manhattan [v1 v2]
  (let [[x1 y1] v1
        [x2 y2] v2]
    (+ (Math/abs (- x2 x1))
       (Math/abs (- y2 y1)))))

(defn step [{:keys [dir coord]} {:keys [action value]}]
  "Actions indicate how to move a ship."
  (case action
      "N" {:coord (v+ coord [(* -1 value) 0]) :dir dir}
      "S" {:coord (v+ coord [(* 1 value) 0]) :dir dir}
      "E" {:coord (v+ coord [0 (* 1 value)]) :dir dir}
      "W" {:coord (v+ coord [0 (* -1 value)]) :dir dir}
      "L" {:coord coord :dir (- dir value)}
      "R" {:coord coord :dir (+ dir value)}
      "F" {:coord (move coord dir value) :dir dir}
      {:coord coord :dir dir}))

(defn part-one [input]
  (->> [{:dir 90 :coord [0 0]} (map parse-line input)]
       (iterate (fn [[acc actions]]
                  [(step acc (first actions)) (rest actions)]))
       (map first)
       (drop (count input))
       (first)
       (:coord)
       (manhattan [0 0])))

(defn step-waypoint [{:keys [ship waypoint]} {:keys [action value]}]
  "Actions indicate how to move a waypoint which is relative to the ship's position"
  (case action
      "N" {:waypoint (v+ waypoint [(* -1 value) 0]) :ship ship}
      "S" {:waypoint (v+ waypoint [(* 1 value) 0]) :ship ship}
      "E" {:waypoint (v+ waypoint [0 (* 1 value)]) :ship ship}
      "W" {:waypoint (v+ waypoint [0 (* -1 value)]) :ship ship}
      "L" {:waypoint (rotate waypoint (* -1 value)) :ship ship}
      "R" {:waypoint (rotate waypoint value) :ship ship}
      "F" {:waypoint waypoint :ship (v+ ship (v* [value value] waypoint))}
      {:waypoint waypoint :ship ship}))

(defn part-two [input]
  (->> [{:waypoint [-1 10] :ship [0 0]} (map parse-line input)]
       (iterate (fn [[acc actions]]
                  [(step-waypoint acc (first actions)) (rest actions)]))
       (map first)
       (drop (count input))
       (first)
       (:ship)
       (manhattan [0 0])))

(comment
  (part-one input)
  (part-two input)
  )
