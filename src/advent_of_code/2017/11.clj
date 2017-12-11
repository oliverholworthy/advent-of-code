(ns advent-of-code.2017.11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def move-dir {:nw [-1  1  0] :n [0  1 -1] :ne [1  0 -1]
               :sw [-1  0  1] :s [0 -1  1] :se [1 -1  0]})

(defn manhattan [pos] (/ (reduce + (mapv #(Math/abs %) pos)) 2))

(defn positions [dirs] (reductions #(mapv + %1 %2) [0 0 0] dirs))

;; -----------------------------------------------------------------------------

(def input
  ((comp #(map (comp move-dir keyword) %) #(str/split % #",") str/trim slurp io/resource)
   "2017/11/input.txt"))

(comment

  ;; Part One

  (manhattan (apply mapv + input))
  ;; => 720


  ;; Part Two

  (apply max (map manhattan (positions input)))
  ;; => 1485

  )
