(ns advent-of-code.2018.02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/02/input.txt"))))

(defn part-one [xs]
  (->> xs
       (mapcat (fn [line] (distinct (vals (frequencies line)))))
       (frequencies)
       (filter (fn [[k v]] (contains? #{2 3} k)))
       (map second)
       (reduce *)))

(comment
  (part-one input-lines)
  )

;; -------------------------------------------------------------------------

(defn char-diff [s1 s2]
  (get (frequencies (map (fn [c1 c2] (= c1 c2)) s1 s2))
       false
       0))

(defn common-chars [s1 s2]
  (->> (map (fn [c1 c2] [c1 c2]) s1 s2)
       (filter (fn [[c1 c2]] (= c1 c2)))
       (map first)
       (str/join)))

(defn find-pair [xs]
  (reduce (fn [acc s]
            (let [diffs (map (fn [s1] {:diff (char-diff s1 s)
                                      :id s1})
                             acc)]
              (if (contains? (set (map :diff diffs)) 1)
                (reduced [s (:id (first (filter #(= (:diff %) 1) diffs)))])
                (rest acc))))
          (rest xs)
          xs))

(defn part-two [xs]
  (apply common-chars (find-pair xs)))

(comment
  (part-two input-lines)
  )
