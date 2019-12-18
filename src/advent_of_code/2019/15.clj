(ns advent-of-code.2019.15
  "Day 15: Oxygen System"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]
            [advent-of-code.2019.05 :refer [run init-state]]))

(def program-instructions (str/split (str/trim (slurp (io/resource "2019/15/input.txt"))) #","))

(def dir-to-input
  {:north 1
   :south 2
   :west 3
   :east 4})

(defn get-neighbours-program [area v]
  (let [[x y] v
        computer (get-in area [x y :computer])
        path (get-in area [x y :path] [])]
    (reduce (fn [{:keys [neighbours area]} [coord dir]]
              (let [node (get-in area coord)
                    input (get dir-to-input dir)
                    res (when node
                          (run (assoc computer
                                      :inputs [input]
                                      :awaiting-input false)))
                    node-type (case (peek (:outputs res)) 0 :wall 1 :open 2 :oxygen :unknown)
                    node (assoc node :type node-type :computer res :path (conj path input))]
                {:area (assoc-in area coord node)
                 :neighbours
                 (cond-> neighbours
                   (and node (#{:open :oxygen} (:type node)))
                   (conj coord))}))
            {:area area :neighbours []}
            [[[(dec x) y] :north]
             [[x (dec y)] :west]
             [[x (inc y)] :east]
             [[(inc x) y] :south]])))

(defn get-neighbours-filled [area v]
  (let [[x y] v]
    (reduce (fn [{:keys [neighbours area]} coord]
              (let [node (get-in area coord)]
                {:area area
                 :neighbours
                 (cond-> neighbours
                   (and node (#{:open} (:type node)))
                   (conj coord))}))
            {:area area :neighbours []}
            [[(dec x) y]
             [x (dec y)]
             [x (inc y)]
             [(inc x) y]])))

(defn shortest-paths [area-init start neighbours-fn]
  ((fn explore [explored frontier area]
     (lazy-seq
      (when-let [[v [total-cost previous-vertex]] (peek frontier)]
        (let [path (conj (explored previous-vertex []) v)
              {:keys [area neighbours]} (neighbours-fn area v)
              node (get-in area v)]
          (cons [(assoc node :coord v) total-cost path]
                (explore (assoc explored v path)
                         (merge-with
                          (fn [val-in-result val-in-latter]
                            (first (sort-by second [val-in-result val-in-latter])))
                          (pop frontier)
                          (into {}
                                (for [n (remove explored neighbours)]
                                  {n [(+ total-cost 1) v]})))
                         area))))))
   {}
   (priority-map start [0])
   area-init))

(defn empty-grid [n]
  (mapv (fn [y] (mapv (fn [x] {:type :wall}) (range n))) (range n)))

(defn print-grid [g]
  (println (str/join "\n"
                     (mapv (fn [row] (str/join (map (fn [n] (case (:type n)
                                                            :home "H"
                                                            :oxygen "O"
                                                            :open "."
                                                            :wall "#"
                                                            "-")) row)))
                           g))))

(defn reduce-grid [acc [n _ _]] (assoc-in acc (conj (:coord n) :type) (:type n)))

(defn v-op [op]
  (fn [v1 v2]
    (let [[x1 y1] v1
          [x2 y2] v2]
      [(op x1 x2) (op y1 y2)])))

(def v-sub (v-op -))

(def vec-to-dir
  {[-1 0] :north
   [1 0] :south
   [0 1] :east
   [0 -1] :west})

(defn path-to-inputs [path]
  (mapv (comp dir-to-input vec-to-dir (fn [[v1 v2]] (v-sub v2 v1))) (partition 2 1 path)))

(defn part-one []
  "Find distance from start to oxygen"
  (let [grid-size 50
        start-coords [(- grid-size 25) (- grid-size 25)]
        computer (init-state (concat program-instructions (repeat 100 "0")) [])
        grid (-> (empty-grid grid-size)
                 (assoc-in (conj start-coords :computer) computer)
                 (assoc-in (conj start-coords :type) :home))
        paths-from-start (shortest-paths grid start-coords get-neighbours-program)
        [_ cost _] (first (drop-while (fn [[n _ _]] (not= (:type n) :oxygen)) paths-from-start))]
    cost))

(defn part-two []
  "Find time to fill with oxygen. (Furthest point from oxygen)"
  (let [grid-size 50
        start-coords [(- grid-size 25) (- grid-size 25)]
        computer (init-state (concat program-instructions (repeat 100 "0")) [])
        grid (-> (empty-grid grid-size)
                 (assoc-in (conj start-coords :computer) computer)
                 (assoc-in (conj start-coords :type) :home))
        paths-from-start (shortest-paths grid start-coords get-neighbours-program)
        oxygen-coords (:coord (ffirst (filter (fn [[n cost path]] (= :oxygen (:type n))) paths-from-start)))
        filled-grid (reduce reduce-grid grid paths-from-start)
        paths-from-oxygen (shortest-paths filled-grid oxygen-coords get-neighbours-filled)
        max-dist (reduce (fn [acc [n cost path]] (max acc cost)) 0 paths-from-oxygen)]
    max-dist))

(comment
  (reduce (fn [acc d] (run (assoc acc :inputs [d] :awaiting-input false)))
          (init-state (concat program-instructions (repeat 100 "0")) [])
          [1 1 2 1 2])
  (part-one)
  (part-two)
  )
