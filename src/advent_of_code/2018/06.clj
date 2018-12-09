(ns advent-of-code.2018.06
  "Day 6: Chronal Coordinates"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/06/input.txt"))))

(defn parse-coord [s]
  (let [[x y] (str/split s #", ")]
    [(Long/parseLong x) (Long/parseLong y)]))

(def coords (map parse-coord input-lines))

(defn manhattan-distance [[^long x1 ^long y1] [^long x2 ^long y2]]
  (+ (Math/abs (- y2 y1))
     (Math/abs (- x2 x1))))

(defn idx-to-char [i]
  (get {0 "a"
        1 "b"
        2 "c"
        3 "d"
        4 "e"
        5 "f"} i i))

(defn get-coords-by-id [xs]
  (reduce (fn [acc [i v]] (assoc acc (idx-to-char i) v)) {}
          (map-indexed vector xs)))

(defn closest-coord [v coords-by-id]
  (let [min-dists (sort-by :dist <
                           (map (fn [[coord-id c]]
                                  {:coord-id coord-id
                                   :dist (manhattan-distance v c)})
                                coords-by-id))]
    (when (not= (:dist (first min-dists))
                (:dist (second min-dists)))
      (:coord-id (first min-dists)))))

(defn infinite-coords [coords-by-id buf]
  (let [[min-x min-y] (reduce-coords min (vals coords-by-id))
        [max-x max-y] (reduce-coords max (vals coords-by-id))
        [min-x min-y] [(- min-x buf) (- min-y buf)]
        [max-x max-y] [(+ max-x buf) (+ max-y buf)]]
    (reduce
     (fn [acc v]
       (let [closest-coord-id (closest-coord v coords-by-id)]
         (cond-> acc closest-coord-id (conj closest-coord-id))))
     #{}
     (mapcat identity
             (concat (for [x (range min-x max-x)]
                       [[x min-y] [x max-y]])
                     (for [y (range (inc min-y) (dec max-y))]
                       [[min-x y] [max-x y]]))))))

(defn grid-coords [coords-by-id]
  (let [[min-x min-y] (reduce-coords min (vals coords-by-id))
        [max-x max-y] (reduce-coords max (vals coords-by-id))]
    (for [x (range min-x max-x)
          y (range min-y max-y)]
      [x y])))

(defn region-sizes [coords-by-id]
  (reduce
   (fn [acc v]
     (let [closest-coord-id (closest-coord v coords-by-id)]
       (cond-> acc
         closest-coord-id
         (update closest-coord-id (fn [s] (inc (or s 0)))))))
   {}
   (grid-coords coords-by-id)))

(defn part-one [coords]
  (let [buf-size 10
        coords-by-id (get-coords-by-id coords)
        regions (region-sizes coords-by-id)
        inf-coords (infinite-coords coords-by-id buf-size)]
    (->> regions
         (remove (fn [[coord-id _]] (contains? inf-coords coord-id)))
         (sort-by (fn [[coord-id size]] size) >)
         (first)
         (second))))

(def sample-coords
  [[1, 1]
   [1, 6]
   [8, 3]
   [3, 4]
   [5, 5]
   [8, 9]])

(comment
  (time (def res-sample (part-one sample-coords)))
  (time (def res-part-one (part-one coords)))
  )

;; -------------------------------------------------------------------------

(defn total-distance [v coords]
  (reduce + (map (fn [c] (manhattan-distance c v)) coords)))

(defn part-two [coords max-dist]
  (reduce
   (fn [acc v]
     (cond-> acc
       (< (total-distance v coords) max-dist)
       (inc)))
   0
   (grid-coords (get-coords-by-id coords))))

(comment
  (time (def res-part-two (part-two coords 10000)))
  )
