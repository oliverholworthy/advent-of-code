(ns advent-of-code.2018.22
  "Day 22: Mode Maze"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

(declare erosion-level)

(defn geologic-index [coord depth]
  (let [[y x] coord]
    (cond
      (= [0 0] [y x]) 0
      (zero? y) (* x 16807)
      (zero? x) (* y 48271)
      :else
      (* (erosion-level [y (dec x)] depth)
         (erosion-level [(dec y) x] depth)))))

(def erosion-level
  (memoize
   (fn [coord depth]
     (mod (+ (geologic-index coord depth) depth) 20183))))

(defn region-type [coord depth]
  (case (mod (erosion-level coord depth) 3)
    0 :rocky
    1 :wet
    2 :narrow))

(defn get-grid [target-coord depth & [{:keys [buffer] :or {buffer 0}}]]
  (let [[ty tx] target-coord]
    (mapv (fn [y] (mapv (fn [x]
                         (if (= target-coord [y x])
                           :rocky
                           (region-type [y x] depth)))
                       (range (+ (inc tx) buffer))))
          (range (+ buffer (inc ty))))))

(defn risk-level [area]
  (reduce +
          (mapcat (fn [row]
                    (map (fn [region] (case region :rocky 0 :wet 1 :narrow 2 0))
                         row))
                  area)))

(defn print-grid [grid]
  (println
   (str/join
    "\n"
    (for [row grid]
      (str/join
       (for [region row]
         (case region
           :rocky "."
           :wet "="
           :narrow "|"
           :target "T"
           :mouth "M")))))))

(def valid-gear
  {:rocky #{:climbing-gear :torch}
   :wet #{:climbing-gear :neither}
   :narrow #{:torch :neither}})

(defn get-neighbours [area v]
  (let [[y x g] v
        v-type (get-in area [y x])
        new-g (first (disj (get valid-gear v-type) g))]
    (reduce (fn [acc c]
              (conj acc c))
            []
            (filter (fn [c]
                      (let [[cy cx cg] c
                            n-type (get-in area [cy cx])]
                        (and (>= cx 0) (>= cy 0)
                             (contains? (set/intersection (get valid-gear v-type)
                                                          (get valid-gear n-type))
                                        cg))))
                    [[y x new-g]
                     [(dec y) x g]
                     [y (dec x) g]
                     [y (inc x) g]
                     [(inc y) x g]]))))

(defprotocol IGraph
  (neighbors [g v])
  (cost [g v1 v2]))

(defrecord RegionGraph [area target-coord]
  IGraph
  (neighbors [_ v]
    (get-neighbours area v))
  (cost [_ v1 v2]
    (let [[y1 x1 t1] v1
          [y2 x2 t2] v2]
      (if (= [y1 x1] [y2 x2])
        7
        1))))

(defn merge-fn [val-in-result val-in-latter]
  (first (sort-by first [val-in-result val-in-latter])))

(defn shortest-paths [g start]
  ((fn explore [explored frontier]
     (lazy-seq
      (when-let [[v [total-cost previous-vertex]] (peek frontier)]
        (let [path (conj (explored previous-vertex []) v)]
          (cons [v total-cost path]
                (explore (assoc explored v path)
                         (merge-with
                          merge-fn
                          (pop frontier)
                          (into {}
                                (for [n (remove explored (neighbors g v))]
                                  (let [n-cost (cost g v n)]
                                    [n [(+ total-cost n-cost) v]]))))))))))
   {}
   (priority-map start [0])))

(defn part-two [depth target-coord]
  (let [a (get-grid target-coord depth {:buffer 100})
        g (RegionGraph. a target-coord)]
    (->> (shortest-paths g [0 0 :torch])
         (drop-while (fn [[n _ _]] (not= (conj target-coord :torch) n)))
         (first)
         (second))))

(comment
  ;; Part One
  (print-grid (get-grid [10 10] 510 {:buffer 10}))
  (risk-level (assoc-in (get-grid [734 13] 7305) [734 13] :rocky))
  ;; Part Two
  (part-two 510 [10 10])
  (time (def res-two (part-two 7305 [734 13])))
  )
