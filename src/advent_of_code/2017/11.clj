(ns advent-of-code.2017.11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def move-dirs
  {"ne" [ 1  0 -1]
   "n"  [ 0  1 -1]
   "nw" [-1  1  0]
   "sw" [-1  0  1]
   "s"  [ 0 -1  1]
   "se" [ 1 -1  0]})

(defn move-dir [move] (get move-dirs move))
(defn manhattan [pos] (/ (reduce + (mapv #(Math/abs %) pos)) 2))
(defn apply-dirs [dirs] (apply mapv + dirs))
(defn apply-dir [pos dir] (mapv + pos dir))
(defn positions [dirs] (reductions apply-dir [0 0 0] dirs))

;; -----------------------------------------------------------------------------

(def input (str/split ((comp str/trim slurp io/resource) "2017/11/input.txt") #","))

(comment

  ;; Part One

  (manhattan (apply-dirs (map move-dir input)))
  ;; => 720


  ;; Part Two

  (apply max (map manhattan (positions (map move-dir input))))
  ;; => 1485

  )
