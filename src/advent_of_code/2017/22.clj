(ns advent-of-code.2017.22
  "Sporifica Virus"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [resource]
  (mapv (fn [line] (mapv (fn [c] (if (= c \.) 0 1))
                        line))
        (str/split-lines (slurp resource))))

(defn right [[x y]]
  [(+ (* 0 x) (* 1 y))
   (+ (* -1 x) (* 0 y))])

(defn left [[x y]]
  [(+ (* 0 x) (* -1 y))
   (+ (* 1 x) (* 0 y))])

(defn flip-v [m] (mapv vec (reverse m)))

(defn extend-grid [{:keys [grid pos] :as grid-state}]
  (let [[j i] pos
        n (count grid)
        m (count (first grid))
        [new-grid new-pos]
        (cond (< i 0)
              [(into [(vec (repeat m 0))] grid)
               [j (inc i)]]
              (< j 0)
              [(mapv #(into [0] %) grid)
               [(inc j) i]]
              (> i (dec n))
              [(into grid [(vec (repeat m 0))])
               pos]
              (> j (dec m))
              [(mapv #(into % [0]) grid)
               pos]
              :else [grid pos])]
    (assoc grid-state :grid new-grid :pos new-pos)))

(defn burst [{:keys [grid pos dir infection-count]}]
  (let [[j i] pos
        current-node (get-in grid [i j])
        infected? (= 1 current-node)
        new-dir (if infected?
                  (right dir)
                  (left dir))
        new-grid (if infected?
                   (assoc-in grid [i j] 0)
                   (assoc-in grid [i j] 1))
        new-pos (mapv + pos new-dir)]
    (extend-grid
     {:grid new-grid
      :pos new-pos
      :dir new-dir
      :infection-count (cond-> infection-count
                         (not infected?)
                         inc)})))

(defn burst-2 [{:keys [grid pos dir infection-count]}]
  (let [[j i] pos
        current-node (get-in grid [i j])
        new-dir (get {0 (left dir)
                      :W dir
                      1 (right dir)
                      :F (mapv #(* -1 %) dir)}
                     current-node)
        new-grid (assoc-in grid [i j]
                           (get {0 :W
                                 :W 1
                                 1 :F
                                 :F 0}
                                current-node))
        new-pos (mapv + pos new-dir)]
    (extend-grid
     {:grid new-grid
      :pos new-pos
      :dir new-dir
      :infection-count (cond-> infection-count
                         (= :W current-node)
                         inc)})))

(defn shape [m] [(count m) (count (first m))])

(defn middle-pos [grid]
  (let [[n m] (shape grid)]
    [(dec (/ (inc m) 2)) (dec (/ (inc n) 2))]))

(defn sporifica [n initial-grid burst-fn]
  (update
   (reduce (fn [acc i]
             (when (zero? (mod i 1e6))
               (println i))
             (burst-fn acc))
           {:grid (flip-v initial-grid)
            :pos (middle-pos initial-grid)
            :dir [0 1]
            :infection-count 0}
           (range n))
   :grid flip-v))

(defn print-grid [grid]
  (str/join "\n" (map #(str/join "" %) grid)))

;; -----------------------------------------------------------------------------

(comment

  (def grid (read-input (io/resource "2017/22/input.txt")))

  ;; Example

  (:infection-count
   (sporifica 10000
              [[0 0 1]
               [1 0 0]
               [0 0 0]]
              burst))


  ;; Part One

  (:infection-count
   (sporifica 10000
              grid
              burst))
  ;; => 5575


  ;; Part Two

  (time
   (def part-two
     (sporifica 10e6
                grid
                burst-2)))

  (:infection-count part-two)
  ;; => 2511991
  )
