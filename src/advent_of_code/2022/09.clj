(ns advent-of-code.2022.09
  "Day 9: Rope Bridge"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "2022/09.txt"))))
(def input-sample (str/split-lines (slurp (io/resource "2022/09.sample.txt"))))

(defn parse-motion [s]
  (let [[move-dir move-size] (str/split s #"\s+")]
    {:dir (get {"L" [0 -1] "R" [0 1] "U" [-1 0] "D" [1 0]} move-dir)
     :step (Long/parseLong move-size)}))

(defn parse-motions [lines]
  (reduce (fn [acc {:keys [dir step]}] (reduce conj acc (repeat step dir))) [] (map parse-motion lines)))

(defn v+ [[y1 x1] [y2 x2]] [(+ y1 y2) (+ x1 x2)])
(defn v- [[y1 x1] [y2 x2]] [(- y1 y2) (- x1 x2)])

(defn distance [v1 v2]
  (let [[y x] (v- v1 v2)]
    (max (Math/abs y) (Math/abs x))))

(defn truncate [x n]
  (let [neg? (< x 0)]
    (cond-> (min (Math/abs x) n)
      neg? (* -1))))

(defn tail-move [head tail]
  (if (> (distance head tail) 1)
    (let [[y x] (v- head tail)
          tail-move [(truncate y 1) (truncate x 1)]]
      (v+ tail tail-move))
    tail))

(defn move-rope [rope-positions head-dir]
  (reduce (fn [new-rope-positions i]
            (let [[head tail] (subvec new-rope-positions i (+ i 2))
                  new-tail (tail-move head tail)]
              (assoc new-rope-positions (inc i) new-tail)))
          (update rope-positions 0 v+ head-dir)
          (range (dec (count rope-positions)))))

(defn simulate [rope-length input]
  (reduce (fn [{:keys [positions tail-visited]} dir]
            (let [new-positions (move-rope positions dir)]
              {:positions new-positions
               :tail-visited (conj tail-visited (last new-positions))}))
          {:positions (vec (repeat rope-length [0 0])) :tail-visited #{}}
          (parse-motions input)))

(def part-one (partial simulate 2))
(def part-two (partial simulate 10))

(comment
  (time (count (:tail-visited (part-one input))))
  (time (count (:tail-visited (part-two input))))
  )
