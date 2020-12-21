(ns advent-of-code.2020.20
  "Day 20: Jurassic Jigsaw"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (map str/split-lines (str/split (slurp (io/resource "2020/20/input.txt")) #"\n\n")))

(defn parse-tile [tile-lines]
  (let [[_ tile-id] (re-matches #"Tile (\d+):" (first tile-lines))]
    {:tile/id (Long/parseLong tile-id)
     :tile/vec (mapv vec (rest tile-lines))}))

(def tiles (map parse-tile input))

(defn edges [v]
  (let [el [(first v)
            (last v)
            (mapv first v)
            (mapv last v)]
        rl (map (comp vec reverse) el)]
    (concat el rl)))

(defn corners [tiles]
  (let [edge-freq (frequencies (mapcat (comp edges :tile/vec) tiles))]
    (filter (fn [tile] (= (get (frequencies (map edge-freq (edges (:tile/vec tile))))
                              1)
                         4))
            tiles)))

(comment
  ;; Part One
  (apply * (map :tile/id (corners tiles)))

  ;; Part Two
  
  )
