(ns advent-of-code.2019.11
  "Day 11: Space Police"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [advent-of-code.2019.05 :refer [run init-state]]))

(def program (str/split (slurp (io/resource "2019/11/input.txt")) #","))

(defn empty-map [n] (mapv (fn [y] (mapv (fn [x] ".") (range n))) (range n)))

(defn turn-right [[y x]] [(* -1 x) y])
(defn turn-left [[y x]] [x (* -1 y)])

(defn v-op [op]
  (fn [v1 v2]
    (let [[y1 x1] v1
          [y2 x2] v2]
      [(op y1 y2) (op x2 x1)])))

(def v-add (v-op +))

(defn paint [program starting-cell]
  (let [n 100
        pos [50 50]
        m (empty-map n)
        m (assoc-in m pos starting-cell)
        dir [-1 0] ;; pointing up
        state (init-state program [0])]
    (loop [m m
           state state
           dir dir
           pos pos
           i 0
           cells #{}]
      (let [inputs [(if (= (get-in m pos) ".") 0 1)]
            new-state (run (assoc state :inputs inputs))]
        (if (:halted new-state)
          {:state new-state
           :pos pos
           :dir dir
           :m m
           :i i
           :cells cells}
          (let [[a b] (:outputs new-state)
                new-dir (if (zero? b) (turn-left dir) (turn-right dir))]
            (recur (assoc-in m pos (if (zero? a) "." "#"))
                   (assoc new-state :awaiting-input false :outputs [])
                   new-dir
                   (v-add new-dir pos)
                   (inc i)
                   (conj cells pos))))))))

(comment
  (time (def part-one (paint (concat program (repeat 600 "0")) ".")))
  (count (:cells part-one)) ;; => 1909
  (time (def part-two (paint (concat program (repeat 600 "0")) "#")))
  (doseq [line (:m part-two)]
    (println (str/join (reverse line))))
  )
