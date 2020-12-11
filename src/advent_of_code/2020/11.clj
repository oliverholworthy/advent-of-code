(ns advent-of-code.2020.11
  "Day 11: Seating System"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (mapv (fn [line] (mapv str line))
        (str/split-lines
         (slurp (io/resource "2020/11/input.txt")))))

(def input-sample
  (mapv (fn [line] (mapv str line))
        (str/split-lines
         (slurp (io/resource "2020/11/sample.txt")))))

(defn coords [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    [y x]))

(defn v-op [op]
  (fn [v1 v2]
    (let [[x1 y1] v1
          [x2 y2] v2]
      [(op x1 x2) (op y1 y2)])))

(def v-add (v-op +))

(defn find-neighbour-dir [grid start-coord dir]
  (reduce (fn [neighbour coord]
            (let [v (get-in grid coord)]
              (if (= v ".")
                [coord v]
                (reduced (if v [coord v] neighbour)))))
   nil
   (rest (iterate (fn [c] (v-add c dir)) start-coord))))

(defn neighbours-dir [grid coord]
  "Find closest neighbours in all directions that are not empty"
  (reduce (fn [acc dir]
            (if-let [v (find-neighbour-dir grid coord dir)]
              (conj acc v)
              acc))
          []
          [[-1 -1]
           [1 1]
           [1 -1]
           [-1 1]
           [-1 0]
           [0 -1]
           [1 0]
           [0 1]]))

(defn neighbours [grid [y x]]
  "Find immediate neigbours "
  (reduce (fn [acc coord]
            (if-let [node (get-in grid coord)]
              (conj acc [coord node])
              acc))
          []
          [[(dec y) (dec x)]
           [(inc y) (inc x)]
           [(dec y) (inc x)]
           [(inc y) (dec x)]
           [y (dec x)]
           [(dec y) x]
           [(inc y) x]
           [y (inc x)]]))

(defn print-grid [grid]
  (println (str/join "\n" (map str/join grid))))

(defn update-seat [seat-layout seat-coord seat-tolerance neighbours-fn]
  (let [seat-value (get-in seat-layout seat-coord)
        seat-neighbours (map second (neighbours-fn seat-layout seat-coord))]
    (cond
      ;; Seat is empty (L) and there are no occupied seat adjacent to it -> beomces occupied
      (and (= seat-value "L") (not (contains? (set seat-neighbours) "#")))
      "#"
      ;; Seat is occupied
      (and (= seat-value "#") (>= (get (frequencies seat-neighbours) "#" 0) seat-tolerance))
      "L"

      :else seat-value)))

(defn change-seats [seat-tolerance neighbours-fn seat-layout]
  (reduce (fn [acc coord]
            (assoc-in acc coord
                      (update-seat seat-layout coord
                                   seat-tolerance neighbours-fn)))
          seat-layout (coords seat-layout)))

(defn find-stable-state [seat-layout seat-tolerance neighbours-fn]
  (ffirst (drop-while (fn [[a b]] (not= a b))
                      (partition 2 1 (iterate (partial change-seats
                                                       seat-tolerance neighbours-fn)
                                              seat-layout)))))

(defn find-stable-state-a [seat-layout]
  (find-stable-state seat-layout 4 neighbours))

(defn find-stable-state-b [seat-layout]
  (find-stable-state seat-layout 5 neighbours-dir))

(comment
  ;; Part One
  (get (frequencies (flatten (find-stable-state-a input))) "#")
  (get (frequencies (flatten (find-stable-state-a input-sample))) "#")

  ;; Part Two
  (get (frequencies (flatten (find-stable-state-b input))) "#")
  (get (frequencies (flatten (find-stable-state-b input-sample))) "#")
  )
