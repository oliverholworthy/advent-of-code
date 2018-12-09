(ns advent-of-code.2018.07
  "Day 7: The Sum of Its Parts"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/07/input.txt"))))

(def input-lines-sample
  (str/split-lines
   (slurp (io/resource "2018/07/input-sample.txt"))))

(defn parse-line [line]
  (let [[_ x y]
        (re-find #"Step ([A-Z]) must be finished before step ([A-Z]) can begin."
                 line)]
    [x y]))

(defn ordering [edges]
  (reduce
   (fn [acc [x y]]
     (update acc x (fn [s] (assoc (or s {}) y {}))))
   {}
   edges))

(defn in-freq [edges]
  (reduce (fn [acc [x y]]
            (-> acc
                (update y (fn [c] (inc (or c 0))))
                (update x (fn [c] (or c 0)))))
          {}
          edges))

(defn root-nodes [edges]
  (map first (filter (comp zero? second) (sort-by second < (in-freq edges)))))

(defn construct-graph [tree k]
  (reduce (fn [t [node sub-t]]
            (assoc t node (construct-graph tree node)))
          {}
          (get tree k)))

(defn dag [edges]
  (let [t (ordering edges)]
    (reduce
     (fn [acc n]
       (assoc acc n (construct-graph t n)))
     {}
     (root-nodes edges))))

(defn next-node [{:keys [graph]}]
  "Traverse graph and find next node available in alphabetical order"
  (let [k (first (sort (keys graph)))]
    {:graph (merge (dissoc graph k) (get graph k))
     :node k}))

(defn part-one [g]
  (str/join
   (reverse
    (:res
     (reduce
      (fn [{:keys [seen res]} s]
        {:res (if (contains? seen s) res (conj res s))
         :seen (conj seen s)})
      {:seen #{}
       :res []}
      (reverse (map :node
                    (take-while :node
                                (rest (iterate next-node
                                               {:graph g}))))))))))

(comment
  (part-one (dag (map parse-line input-lines-sample)))
  (part-one (dag (map parse-line input-lines)))
  "BETUFNVADWGPLRJOHMXKZQCISY"
  )

;; -------------------------------------------------------------------------

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn step-times [min-time]
  (let [chars (map str (char-range \A \Z))]
    (zipmap chars (map (fn [x] (+ (inc x) min-time))
                       (range (count chars))))))

(defn next-worker-step [workers]
  (let [sorted-workers (sort-by :work-time < (filter :node workers))
        done (when sorted-workers (first sorted-workers))]
    {:done done
     :workers (map (fn [{:keys [work-time] :as w}]
                     (let [new-time (max 0 (- work-time (:work-time done 0)))]
                       (cond-> (assoc w :work-time new-time)
                         (zero? new-time)
                         (dissoc :node))))
                   workers)}))

(defn add-work [workers work]
  (let [workers-indexed (map-indexed vector workers)
        idx (ffirst (remove (fn [[i w]] (:node w)) workers-indexed))]
    (mapv (fn [[i w]] (if (= idx i) work w)) workers-indexed)))

(defn next-nodes [graph]
  (sort (keys graph)))

(defn next-graph [graph node]
  (merge (dissoc graph node) (get graph node)))

(defn all-keys [m]
  (reduce-kv (fn [m k v] (concat (cons k m) (all-keys v)))
             '()
             m))

(defn next-step [{:keys [graph done-nodes total-time workers work-time-by-node] :as acc}]
  (let [worker-step (next-worker-step workers)
        done-node (:node (:done worker-step))
        next-graph (cond-> graph done-node (next-graph done-node))
        next-workers (:workers worker-step)
        worker-nodes (set (map :node (filter :node next-workers)))
        node-freqs (frequencies (all-keys next-graph))
        free-worker-count (count (remove :node next-workers))]
    {:work-time-by-node work-time-by-node
     :graph next-graph
     :done-nodes (cond-> done-nodes done-node (conj done-node))
     :total-time (+ total-time (:work-time (:done worker-step) 0))
     :workers
     (reduce (fn [acc node]
               (add-work acc {:node node :work-time (get work-time-by-node node)}))
             next-workers
             (take free-worker-count
                   (remove (fn [node]
                             (or (contains? worker-nodes node)
                                 (> (get node-freqs node 0) 1)))
                           (next-nodes next-graph))))}))

(defn part-two [edges worker-count min-time]
  (let [g (dag edges)
        workers (mapv (fn [i] {:work-time 0}) (range worker-count))]
    (first (drop-while (fn [m] (not (empty? (:graph m))))
                       (iterate next-step {:graph g
                                           :work-time-by-node (step-times min-time)
                                           :done-nodes []
                                           :total-time 0
                                           :workers workers})))))

(comment
  (:total-time (part-two (map parse-line input-lines-sample)
                         2
                         0))
  ;; => 15
  (:total-time (part-two (map parse-line input-lines)
                         5
                         60))
  ;; => 848
  )
