(ns advent-of-code.2017.14
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code.2017.10 :refer [knot-hash]]
            [advent-of-code.2017.12 :refer [groups]]))

(defn binary [n]
  (let [s (str "0000" (Integer/toString n 2))]
    (subs s (- (count s) 4))))

(defn hex->int [hex]
  (Integer/parseInt (str hex) 16))

(defn to-matrix [rows]
  (mapv #(mapv (fn [c] (Long/parseLong (str c))) %) rows))

(defn shape [matrix] [(count matrix) (count (first matrix))])

(defn index-connections [i j matrix]
  (let [[n m] (shape matrix)]
    (->> [[(inc i) j] [(dec i) j]
          [i (inc j)] [i (dec j)]]
         (filter
          (fn [[i j]]
            (and (>= i 0) (>= j 0)
                 (< i n) (< j m)
                 (= (get-in matrix [i j]) 1)))))))

(defn matrix-connections [matrix]
  (let [[n m] (shape matrix)]
    (->> (for [i (range n)
               j (range m)]
           (when (= 1 (get-in matrix [i j]))
             (let [coords (set (index-connections i j matrix))]
               [[i j] coords])))
         (remove nil?)
         (into {}))))

(defn rand-matrix [n]
  (mapv (fn [i] (mapv (fn [j] (if (> (rand) 0.75) 1 0)) (range n)))
        (range n)))

(comment
  (def hashes
    (doall (let [input "wenycdww"]
             (map (fn [i] (println i) (knot-hash (str input "-" i)))
                  (range 128)))))
  (def grid (map (fn [hash] (str/join (map (fn [c] (binary (hex->int c)))
                                          hash)))
                 hashes))

  ;; Part One

  (get (frequencies (str/join grid)) \1)
  ;; => 8226

  ;; Part Two

  (count (groups (matrix-connections (to-matrix grid))))
  ;; => 1128
  )
