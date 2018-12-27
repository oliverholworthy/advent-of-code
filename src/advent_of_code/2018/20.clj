(ns advent-of-code.2018.20
  "Day 20: A Regular Map"
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [advent-of-code.2018.15 :refer [shortest-paths IGraph]]))

(defrecord MazeGraph [maze]
  IGraph
  (neighbors [_ v]
    (let [[x y] v]
      (reduce (fn [acc c]
                (conj acc c))
              []
              (filter (fn [c] (contains? maze #{v c}))
                      [[(dec x) y]
                       [x (dec y)]
                       [x (inc y)]
                       [(inc x) y]]))))
  (cost [_ v1 v2] 1))

(def input-regex
  (slurp (io/resource "2018/20/input.txt")))

(defn read-regex [{:keys [pos starts ends stack maze] :as acc} c]
  (cond
    (= \^ c) acc;; start
    (#{\N \W \S \E} c)
    (let [direction (get {\N [1 0]
                          \E [0 1]
                          \W [0 -1]
                          \S [-1 0]}
                         c)]
      (assoc acc
             :maze
             (reduce (fn [m p]
                       (let [p2 (mapv + p direction)]
                         (conj m #{p p2})))
                     maze pos)
             :pos
             (set (for [p pos] (mapv + p direction)))))
    (= \( c)
    (assoc acc
           :stack (conj stack [starts ends])
           :starts pos
           :ends #{})
    (= \| c)
    (assoc acc
           :pos starts
           :ends (set/union pos ends))
    (= \) c)
    (let [pos (set/union pos ends)
          [starts ends] (peek stack)]
      (assoc acc
             :pos pos
             :stack (pop stack)
             :starts starts
             :ends ends))
    (= \$ c)
    (reduced acc) ;; end
    ))

(defn maze-paths [input-regex]
  (let [res (reduce read-regex
                    {:pos #{[0 0]}
                     :stack []
                     :starts #{[0 0]}
                     :ends #{}
                     :maze #{}}
                    input-regex)
        maze (:maze res)]
    (shortest-paths (MazeGraph. maze)
                    [0 0])))

(defn part-one [input-regex]
  (->> input-regex
       (maze-paths)
       (reverse)
       (first)
       (second)))

(defn part-two [input-regex]
  (->> input-regex
       (maze-paths)
       (filter (fn [[_ d _]] (>= d 1000)))
       (count)))

(comment
  (part-one "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
  (part-one input-regex)
  (part-two input-regex)
  )
