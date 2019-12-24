(ns advent-of-code.2019.19
  "Day 19: Tractor Beam"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [advent-of-code.2019.05 :refer [run init-state]]))

(def program-instructions (str/split (str/trim (slurp (io/resource "2019/19/input.txt"))) #","))

(defn empty-grid [n]
  (mapv (fn [y] (mapv (fn [x] ".") (range n))) (range n)))

(defn grid-coords [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    [y x]))

(defn print-grid [grid]
  (println (str/join "\n"
                     (mapv (fn [row] (str/join row))
                           grid))))

(defn run-program [x y]
  (first (:outputs (run (init-state (concat program-instructions (repeat 40 "0")) [x y])))))

(defn part-one [n]
  (let [grid (empty-grid n)]
    (reduce (fn [acc coord]
              (let [[y x] coord
                    res (run-program x y)]
                (assoc-in acc coord (if (zero? res) "." "#"))))
            grid
            (grid-coords grid))))

(defn find-slope [x-min y-min]
  (let [y 20000
        [_ x2] (ffirst (drop-while (comp zero? second) (map (fn [x] [[y x] (run-program x y)]) (range x-min (* 2 y)))))
        m2 (/ y x2)
        [_ x1] (ffirst (drop-while (comp (complement zero?) second) (map (fn [x] [[y x] (run-program x y)]) (range x2 (* 4 y)))))
        m1 (/ y x1)]
    [m2 m1]))

(defn find-coords [m1 m2]
  (let [x2 (/ (+ (* m1 99) 99) (- m2 m1))
        y1 (- (* m2 x2) 99)]
    [x2 y1]))

(defn peek-at [coord n]
  (let [[y1 x1] coord
        d (int (/ n 2))
        grid (empty-grid n)]
    (reduce (fn [acc coord]
              (let [[y x] coord
                    x (- (+ x1 x) d)
                    y (- (+ y1 y) d)
                    res (run-program x y)]
                (assoc-in acc coord (if (= [y x] [y1 x1])
                                      (if (zero? res) "c" "C")
                                      (if (zero? res) "." "#")))))
            grid
            (grid-coords grid))))

(comment
  (get (frequencies (flatten (part-one 50))) "#")

  ;; Part Two

  ;; 1) Find Slopes of upper and lower part of beam
  (time (def slopes (find-slope 20000 18000)))

  ;; 2) Find coordinates where a 100x100 grid would fit
  (let [[m2 m1] slopes
        [x2 y1] (find-coords m1 m2)]
    [(Math/round (double x2)) (Math/round (double y1))])
  ;; => [1306 1047]

  ;; 3) Inspect Grid
  ;; Beam isn't perfect. So need to check visually for match

  (let [[x2 y1] [1306 1047]]
    (println)
    (print-grid (peek-at [y1 (+ x2 99)] 16)))

  (let [[x2 y1] [1306 1047]]
    (println)
    (print-grid (peek-at [(+ 99 y1) x2] 16)))

  ;; 4) Compute answer
  (let [[x2 y1] [1306 1047]
        shift-y 2
        shift-x 2
        [x2 y1] [(+ shift-x x2) (+ shift-y y1)]]
    (+ y1 (* x2 10000)))
  ;; => 13081049
  )
