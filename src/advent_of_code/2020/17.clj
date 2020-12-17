(ns advent-of-code.2020.17
  "Day 17: Conway Cubes"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :refer [selections]]))

(def input
  "#......#
##.#..#.
#.#.###.
.##.....
.##.#...
##.#....
#####.#.
##.#.###")

(def input-sample
  ".#.
..#
###")

(defn parse-input [input]
  (remove nil?
          (mapcat (fn [[y line]]
                    (map-indexed (fn [x c] (when (= c \#) [x y])) line))
                  (map-indexed vector (str/split-lines input)))))

(defn print-active [active-coords]
  (let [active-coords-set (set active-coords)
        [x-max y-max z-max] (reduce (fn [acc v] (mapv max acc v)) active-coords)
        [x-min y-min z-min] (reduce (fn [acc v] (mapv min acc v)) active-coords)]
    (doseq [z (range z-min (inc z-max))]
      (println (format "z=%d" z))
      (println
       (str/join 
        "\n"
        (for [y (range y-min (inc y-max))]
          (str/join
           (for [x (range x-min (inc x-max))]
             (if (contains? active-coords-set [x y z])
               "#"
               "."))))))
      (println))))

(defn v-op [op]
  (fn [v1 v2]
    (vec (map op v1 v2))))

(def v-add (v-op +))

(defn neighbours [dimension v]
  "immediate neighbours to coordinate V in DIMENSION"
  (let [zero-coord (vec (repeat dimension 0))]
    (set
     (mapv (fn [d] (v-add v d))
           (remove #(= % zero-coord)
                   (selections [-1 1 0] dimension))))))

(defn step [dim active-coords]
  "Run boot cycle in dimension DIM on active coordinates"
  (let [check-coords
        (reduce (fn [acc coord] (set/union acc (conj (neighbours dim coord) coord)))
                #{}
                active-coords)]
    (reduce (fn [acc coord]
           (let [neighbour-active-count
                 (reduce (fn [active-count neighbour]
                           (if (contains? active-coords neighbour)
                             (inc active-count)
                             active-count))
                         0
                         (neighbours dim coord))]
             (cond (and (contains? active-coords coord)
                        (#{2 3} neighbour-active-count))
                   (conj acc coord)
                   (and (not (contains? active-coords coord))
                        (= 3 neighbour-active-count))
                   (conj acc coord)
                   :else acc)))
            #{}
            check-coords)))

(defn run [dim input]
  "Run the six-cycle boot process on INPUT in dimension DIM"
  (let [active-coords
        (set (map (fn [v] (vec (concat v (repeat (- dim 2) 0))))
                                (parse-input input)))]
    (nth (iterate (partial step dim) active-coords) 6)))

(comment
  ;; Part One
  (count (run 3 input-sample))
  (count (run 3 input))

  ;; Part Two
  (count (run 4 input-sample))
  (count (run 4 input))
  )
