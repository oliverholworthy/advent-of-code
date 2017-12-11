(ns advent-of-code.2017.07
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [clojure.set :as set]))

(defn read-input [resource]
  (->> (slurp resource)
       (str/split-lines)
       (map (fn [line]
              (let [[_ name weight names-above]
                    (re-find #"([a-z]+) \((\d+)\)(.*)?" line)
                    [_ names]
                    (re-find #" -> (.*)" names-above)
                    above (when names (map str/trim (str/split names #",")))]
                [name
                 {:weight (Long/parseLong weight)
                  :above above}])))
       (into {})))

(def input (read-input (io/resource "2017/7/input.txt")))
(def input-sample (read-input (io/resource "2017/07/input-sample.txt")))


;; Part One

(defn root-node [responses]
  (first (set/difference (set (keys responses))
                         (into #{} (mapcat :above (vals responses))))))


;; Part Two

(defn construct-tree* [name responses]
  (if-let [node (get responses name)]
    (if-let [above (get node :above)]
      (let [above-nodes (map #(construct-tree* % responses) above)]
        (assoc node
               :above above-nodes
               :weight-total (+ (:weight node) (reduce + (map :weight-total above-nodes)))
               :name name))
      (assoc node :name name :weight-total (:weight node)))
    responses))

(defn construct-tree [responses]
  (construct-tree* (root-node responses) responses))

(defn find-unbalanced-program-correct-weight
  "Find the first unbalanced node in the tree"
  [tree]
  (let [w (atom nil)]
    (walk/postwalk (fn [x]
                     (when (map? x)
                       (when-let [above (:above x)]
                         (when (apply not= (map :weight-total above))
                           (let [weight-freq (frequencies (map :weight-total above))
                                 unbalanced-program
                                 (->> above
                                      (filter #(= 1 (get weight-freq (:weight-total %))))
                                      (first))]
                             (when unbalanced-program
                               (let [unbalanced-weight (:weight-total unbalanced-program)
                                     correct-weight
                                     (first (disj (set (keys weight-freq))
                                                  unbalanced-weight))]
                                 (when-not @w
                                   (reset! w
                                           {:name (:name unbalanced-program)
                                            :weight (:weight unbalanced-program)
                                            :correct-weight
                                            (+ (:weight unbalanced-program)
                                               (-  correct-weight unbalanced-weight))}))))))))
                     x)
                   tree)
    @w))

(comment
  (find-unbalanced-program-correct-weight (construct-tree input-sample))
  (find-unbalanced-program-correct-weight (construct-tree input)))
