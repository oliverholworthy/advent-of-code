(ns advent-of-code.2018.03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/03/input.txt"))))

(defn parse-long [x] (Long/parseLong x))

(defn parse-claim [claim]
  (let [[_ id l t w h]
        (re-find #"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$" claim)]
    {:id id
     :left (parse-long l)
     :top (parse-long t)
     :width (parse-long w)
     :height (parse-long h)}))

(defn grid-coords [{:keys [left top width height]}]
  (for [x (range left (+ left width))
        y (range top (+ top height))]
    [x y]))

(defn coord-freqs [claims]
  (->> claims
       (mapcat grid-coords)
       (frequencies)))

(defn part-one [xs]
  (->> xs
       (map parse-claim)
       (coord-freqs)
       (vals)
       (filter #(> % 1))
       (count)))

(part-one input-lines)

;; -------------------------------------------------------------------------

(defn part-two [xs]
  (let [claims (map parse-claim xs)
        fq (coord-freqs claims)]
    (filter (fn [claim] (every? (fn [coord] (= (get fq coord 0) 1))
                               (grid-coords claim)))
            claims)))

(part-two input-lines)
