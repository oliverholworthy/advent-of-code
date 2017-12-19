(ns advent-of-code.2017.19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [resource]
  (let [lines (str/split-lines (slurp resource))
        width (apply max (map count lines))
        height (count lines)]
    (reduce (fn [network line]
              (let [nodes
                    (mapv (fn [c]
                            (case c
                              \space nil
                              \| :up-down
                              \+ :edge
                              \- :across
                              (str c))) line)]
                (conj network
                      (into nodes (repeat (- width (count nodes)) nil)))))
            []
            lines)))

(defn right [[x y]]
  [(+ (* 0 x) (* 1 y))
   (+ (* -1 x) (* 0 y))])

(defn left [[x y]]
  [(+ (* 0 x) (* -1 y))
   (+ (* 1 x) (* 0 y))])

(defn walk-network [network initial-pos initial-dir]
  (let [width (count (first network))
        height (count network)
        get-val (fn [[x y]]
                  (let [[i j] [x (* -1 y)]]
                    (when (and (>= i 0) (>= j 0)
                               (< i width) (< j height))
                      (get-in network [j i]))))]
    (loop [pos initial-pos
           dir initial-dir
           letters []
           states []]
      (let [left-pos (mapv + pos (left dir))
            right-pos (mapv + pos (right dir))
            node (get-val pos)
            new-pos
            (cond
              (= :edge node) (if (get-val left-pos) left-pos right-pos)
              (#{:up-down :across} node) (mapv + pos dir)
              (string? node) (mapv + pos dir)
              :else pos)
            new-dir
            (cond (= new-pos left-pos) (left dir)
                  (= new-pos right-pos) (right dir)
                  :else dir)]
        (if (= pos new-pos)
          {:states states
           :letters letters}
          (recur new-pos
                 new-dir
                 (if (string? node) (conj letters node) letters)
                 (conj states [pos dir])))))))

(defn initial-state [network]
  {:pos [(ffirst (filter second (map-indexed (fn [i v] [i (= v :up-down)]) (first network)))) 0]
   :dir [0 -1]})

;; -----------------------------------------------------------------------------

(comment
  (def sample
    (let [network (read-input (io/resource "2017/19/input-sample.txt"))
          {:keys [pos dir]} (initial-state network)]
      (when (and pos dir) (walk-network network pos dir))))

  (def res
    (let [network (read-input (io/resource "2017/19/input.txt"))
          {:keys [pos dir]} (initial-state network)]
      (when (and pos dir) (walk-network network pos dir))))

  ;; Part One

  (str/join (:letters res))
  "DWNBGECOMY"


  ;; Part Two

  (count (:states res))
  ;; => 17228
  )
