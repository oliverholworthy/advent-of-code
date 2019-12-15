(ns advent-of-code.2019.13
  "Day 13: Care Package"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [advent-of-code.2019.05 :refer [run init-state]]))

(def program (str/split (str/trim (slurp (io/resource "2019/13/input.txt"))) #","))

(defn empty-grid [n]
  (mapv (fn [y] (mapv (fn [x] "") (range n))) (range n)))

(defn reduce-grid-state
  [acc [y x tile-id]]
  (if (or (< y 0) (< x 0))
    (assoc acc :score tile-id)
    (-> acc
        (assoc-in [:grid y x] (case (int tile-id) 0 " " 1 "#" 2 "@" 3 "_" 4 "o"))
        (cond-> (= (int tile-id) 3) (assoc :paddle [y x])
                (= (int tile-id) 4) (assoc :ball [y x])))))

(defn get-grid [outputs]
  (reduce
   reduce-grid-state
   {:grid (empty-grid 50) :score nil}
   (partition 3 outputs)))

(defn part-one [program]
  (let [grid (get-grid (:outputs (run (init-state (concat program (repeat 100 "0")) []))))]
    (get (frequencies (flatten (:grid grid))) "@")))

(defn print-grid [g]
  (println
   (str/join "\n"
             (map str/join
                  g))))

(defn play [program]
  (loop [state (run (init-state (concat (assoc program 0 "2") ;; Play for free
                                        (repeat 100 "0")) [0]))
         grid-state (get-grid (:outputs state))
         steps 0]
    (if (:halted program)
      {:state state :grid-state grid-state :steps steps}
      (let [[a b] [(first (:ball grid-state)) (first (:paddle grid-state))]
            paddle (cond (> a b) 1 (< a b) -1 :else 0)
            state (run (cond-> (assoc state :inputs [paddle])
                         (zero? (mod (count (:outputs state)) 3))
                         (assoc :outputs [])))
            grid-state (if (zero? (mod (count (:outpus state)) 3))
                         (reduce reduce-grid-state grid-state (partition 3 (:outputs state)))
                         grid-state)]
        (recur state
               grid-state
               (inc steps))))))

(comment
  ;; Part One
  (part-one program)

  ;; Part Two
  (time (def res (play program)))
  (:score (:grid-state res))
  )
