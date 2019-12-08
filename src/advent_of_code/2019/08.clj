(ns advent-of-code.2019.08
  "Day 8: Space Image Format"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (str/trim (slurp (io/resource "2019/08/input.txt"))))

(def layers (mapv vec (partition 6 (mapv vec (partition 25 input)))))

(defn count-in-layer [layer char] (count (filter (fn [c] (= c char)) (flatten layer))))

(defn count-zero [layer] (count-in-layer layer \0))

(defn checksum [layers]
  (let [layer (first (sort-by count-zero < layers))]
    (* (count-in-layer layer \1) (count-in-layer layer \2))))

(defn decode [layers]
  (reduce (fn [acc layer] (mapv (fn [row1 row2] (mapv (fn [x1 x2] (if (= x1 \2) x2 x1)) row1 row2)) acc layer)) layers))

(defn to-str [image]
  (str/join "\n" (map (fn [row] (str/join (map (fn [x] (if (= x \1) "#" " ")) row))) image)))

(comment
  (checksum layers)
  (to-str (decode layers))
  )
