(ns advent-of-code.2020.23
  "Day 23: Crab Cups"
  (:require [clojure.string :as str]))

(defn pick-up [cycle x n]
  (:res (reduce (fn [acc i]
                  (let [next (get cycle (:x acc))]
                    {:res (conj (:res acc) next)
                     :x next})) {:res [] :x x}
                (range n))))

(defn move [{:keys [cups current min-val max-val] :as state}]
  (let [picks (pick-up cups current 3)
        destination
        (loop [dest (dec current)]
          (if (and (not (< dest min-val))
                   (not (contains? (set picks) dest)))
            dest
            (recur (if (< dest min-val)
                     max-val
                     (dec dest)))))
        new-cups
        (-> cups
            (assoc current (get cups (last picks)))
            (assoc (last picks) (get cups destination))
            (assoc destination (first picks)))]
    (assoc state
           :cups
           new-cups
           :current
           (get new-cups current))))

(defn to-cycle [xs]
  (zipmap xs (take (count xs) (drop 1 (cycle xs)))))

(defn init [xs]
  {:cups (to-cycle xs)
   :current (first xs)
   :min-val (apply min xs)
   :max-val (apply max xs)})

(comment
  ;; Part One
  (let [xs [9 6 2 7 1 3 8 5 4]]
    (str/join (pick-up (:cups (nth (iterate move (init xs)) 100))
                       1
                       (dec (count xs)))))

  ;; Part Two
  (time
   (def part-two
     (let [xs [9 6 2 7 1 3 8 5 4]
           xs (concat xs (range (inc (apply max xs)) (inc 1e6)))]
       (pick-up (:cups (nth (iterate move (init xs)) 10e6))
                1
                2))))
  (apply * part-two)
  )
