(ns advent-of-code.2018.08
  "Day 8: Memory Maneuver"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/08/input.txt"))))

(defn parse-line [line] (mapv #(Long/parseLong (str %)) (str/split line #"\s+")))
(def input-data (parse-line (first input-lines)))
(def input-data-sample (parse-line "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))

(defn handle-step [{:keys [current-node-id nodes path tree node-count] :as acc} x]
  (let [node (get nodes current-node-id)
        child-nodes (:child-nodes node)
        metadata (:metadata node)]
    (cond
      ;; Header - Child Count
      (nil? (:child-count node))
      (assoc-in acc [:nodes current-node-id :child-count] x)

      ;; Header - Metadata Count
      (nil? (:metadata-count node))
      (assoc-in acc [:nodes current-node-id :metadata-count] x)

      ;; Fill Child Node
      (> (:child-count node) (count child-nodes))
      (let [new-node-id (inc node-count)
            new-path (conj path new-node-id)]
        (-> acc
            (assoc :current-node-id new-node-id
                   :node-count new-node-id
                   :path new-path
                   :tree (assoc-in tree new-path {})
                   :nodes (assoc-in nodes [new-node-id :child-count] x))))

      ;; Metadata - Child nodes complete
      (and (= (:child-count node) (count child-nodes))
           (> (:metadata-count node) (count metadata)))
      (update-in acc [:nodes current-node-id :metadata] (fn [m] (conj (or m []) x)))

      ;; Complete Node
      (and (= (count child-nodes) (:child-count node))
           (= (count metadata) (:metadata-count node)))
      (let [new-path (pop path)
            parent-node-id (peek new-path)]
        (handle-step (assoc acc
                            :path new-path
                            :current-node-id parent-node-id
                            ;; Add complete note to child-nodes of parent
                            :nodes (update-in nodes [parent-node-id :child-nodes]
                                              (fn [c] (conj (or c []) current-node-id))))
                     x)))))

(defn construct-tree [xs]
  (select-keys
   (reduce
    handle-step
    {:current-node-id 0
     :node-count 0
     :nodes {}
     :path [0]
     :tree {}}
    xs)
   [:nodes :tree]))

(defn part-one [xs]
  (let [g (construct-tree xs)]
    (reduce + (mapcat :metadata (vals (:nodes g))))))

(comment
  (part-one input-data-sample) ;; => 138
  (part-one input-data)
  41555
  )

;; -------------------------------------------------------------------------

(defn node-value [{:keys [child-nodes metadata] :as node} nodes]
  (if (empty? child-nodes)
    (reduce + metadata)
    (reduce +
            (map (fn [m]
                   (if (<= m (count child-nodes))
                     (let [child-node (get nodes (get child-nodes (dec m)))]
                       (node-value child-node nodes))
                     0))
                 metadata))))

(defn part-two [xs]
  (let [g (construct-tree xs)
        root-node-id (ffirst (:tree g))
        root-node (get (:nodes g) root-node-id)]
    (node-value root-node (:nodes g))))

(comment
  (part-two input-data-sample)
  (part-two input-data)
  )
