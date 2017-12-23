(ns advent-of-code.2017.21
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn transpose [m] (apply mapv vector m))

(defn flip [m] (mapv (comp vec reverse) m))

(defn rotate [m] (flip (transpose m)))

(defn variations [grid]
  (set (mapcat (juxt flip identity)
               [grid
                (rotate grid)
                (rotate (rotate grid))
                (rotate (rotate (rotate grid)))])))

(defn read-pattern [pattern]
  (mapv (fn [unit] (mapv (fn [c] (if (= (str c) "#")
                                 1 0))
                        unit))
        (str/split pattern #"/")))

(defn read-input [resource]
  (map (fn [line]
         (let [[source target] (str/split line #" => ")]
           [(read-pattern source) (read-pattern target)]))
       (str/split-lines (slurp resource))))

(defn split-grid [grid n]
  (let [m (/ (count grid) n)]
    (for [i (range m)]
      (for [j (range m)]
        (mapv #(subvec (get grid (+ (* n i) %))
                       (* n j)
                       (+ (* n j) n))
              (range n))))))

(defn split-apply-rules [rules grid n]
  (if (= n (count grid))
    (get rules grid)
    (vec
     (mapcat (fn [row] (apply mapv (comp vec concat) (map #(get rules %) row)))
             (split-grid grid n)))))

(defn read-rules [resource]
  (into {} (mapcat
            (fn [[source target]]
              (mapv (fn [variation] [variation target])
                    (variations source)))
            (read-input resource))))

(defn fractal-grid [rules]
  (let [step (fn [grid]
               (cond
                 (zero? (mod (count grid) 2))
                 (split-apply-rules rules grid 2)
                 (zero? (mod (count grid) 3))
                 (split-apply-rules rules grid 3)))
        grid [[0 1 0]
              [0 0 1]
              [1 1 1]]]
    (iterate step grid)))

(defn count-ones [grid] (get (frequencies (flatten grid)) 1))

;; -----------------------------------------------------------------------------

(comment

  ;; Part One

  (count-ones
   (nth (fractal-grid (read-rules (io/resource "2017/21/input.txt")))
        5))

  ;; Part Two

  (count-ones
   (nth (fractal-grid (read-rules (io/resource "2017/21/input.txt")))
        18))
  )
