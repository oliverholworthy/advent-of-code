(ns advent-of-code.2017.5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> (slurp (io/resource "2017/5/input.txt"))
       (str/trim)
       (str/split-lines)
       (mapv #(Long/parseLong %))))

(defn maze-steps [offset-fn]
  (fn [maze]
    (let [maze-size (count maze)
          outside-maze? (fn [i] (or (< i 0) (>= i maze-size)))]
      (loop [pos 0
             steps 0
             maze maze]
        (let [offset (get maze pos)
              new-pos (+ pos offset)]
          (if (outside-maze? new-pos)
            (inc steps)
            (recur new-pos
                   (inc steps)
                   (assoc maze pos (offset-fn offset)))))))))

;; Part One

(def maze-inc
  (maze-steps inc))

;; Part Two

(def maze-inc-dec
  (maze-steps (fn [offset]
                (if (>= offset 3)
                  (dec offset)
                  (inc offset)))))
