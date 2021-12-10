(ns advent-of-code.2021.10
  "Day 10: Syntax Scoring"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "2021/10.txt"))))
(def input-sample (str/split-lines (slurp (io/resource "2021/10.sample.txt"))))

(def closing-to-opening
  {\} \{
   \> \<
   \) \(
   \] \[})

(def opening-to-closing
  {\{ \}
   \< \>
   \( \)
   \[ \]})

(def syntax-error-score
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def completion-score
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn parse-line [line]
  (reduce (fn [acc c]
            (if (contains? #{\[ \{ \( \<} c)
              (conj acc c)
              (if (= (peek acc) (get closing-to-opening c))
                (pop acc)
                (reduced c))))
          []
          line))

(defn first-illegal-character [line]
  (let [res (parse-line line)]
    (when (char? res)
      res)))

(defn part-one [lines]
  (reduce + (remove nil? (map (comp syntax-error-score first-illegal-character) lines))))

(defn closing-characters [line]
  (let [res (parse-line line)]
    (when (vector? res)
      (map opening-to-closing (reverse res)))))

(defn closing-score [closing-chars-seq]
  (reduce (fn [acc c] (+ (* acc 5) (get completion-score c)))
   0
   closing-chars-seq))

(defn part-two [lines]
  (let [scores (remove zero? (map (comp closing-score closing-characters) lines))
        sorted-scores (sort scores)
        n (count sorted-scores)]
    (nth sorted-scores (/ n 2))))

(comment
  (part-one input-sample)
  (part-one input)
  (part-two input-sample)
  (part-two input)
  )
