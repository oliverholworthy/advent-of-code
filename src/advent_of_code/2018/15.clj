(ns advent-of-code.2018.15
  "Day 15: Beverage Bandits"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn get-sample [i]
  (str/split-lines (slurp (io/resource (str "2018/15/input-sample-" i ".txt")))))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/15/input.txt"))))


(defn parse-input [lines]
  (vec (map-indexed (fn [y line]
                      (vec (map-indexed (fn [x c] (case (str c)
                                                   "#" {:type :wall}
                                                   "." {:type :open}
                                                   "E" {:type :unit
                                                        :coord [y x]
                                                        :unit-type :elf :target-unit-type :goblin
                                                        :attack-power 3 :hit-points 200}
                                                   "G" {:type :unit
                                                        :coord [y x]
                                                        :unit-type :goblin :target-unit-type :elf
                                                        :attack-power 3 :hit-points 200}))
                                        line)))
                    lines)))

(defprotocol IGraph
  (neighbors [g v])
  (cost [g v1 v2]))

(defrecord AreaGraph [area]
  IGraph
  (neighbors [_ v]
    (let [[x y] v]
      (reduce (fn [acc [n c]]
                (cond-> acc
                  (and n (= :open (:type n)))
                  (conj c)))
              []
              (map (fn [c] [(get-in area c) c])
                   [[(dec x) y]
                    [x (dec y)]
                    [x (inc y)]
                    [(inc x) y]]))))
  (cost [_ v1 v2] 1))

(defn shortest-paths [g start]
  ((fn explore [explored frontier]
     (lazy-seq
      (when-let [[v [total-cost previous-vertex]] (peek frontier)]
        (let [path (conj (explored previous-vertex []) v)]
          (cons [v total-cost path]
                (explore (assoc explored v path)
                         (merge-with
                          (fn [val-in-result val-in-latter]
                            ;; (println val-in-result val-in-latter)
                            (first (sort-by second [val-in-result val-in-latter])))
                          (pop frontier)
                          (into {}
                                (for [n (remove explored (neighbors g v))]
                                  {n [(+ total-cost (cost g v n)) v]})))))))))
   {}
   (priority-map start [0])))

(defn unit-name [t]
  (case t :elf "E" :goblin "G"))

(defn unit-summary [u]
  (str (unit-name (:unit-type u)) "(" (:hit-points u) ")"
       " " (str (:coord u))))

(defn print-area [area]
  (doseq [row area]
    (println
     (str
      (str/join
       (map (fn [x] (case (:type x)
                     :wall "#"
                     :open "."
                     :unit (case (:unit-type x) :elf "E" :goblin "G")))
            row))
      " "
      (str/join ", "
                (map (fn [x] (unit-summary x))
                     (filter (fn [x] (= :unit (:type x))) row)))))))

(defn find-attack-target [attack-targets unit]
  (let [attack-target (first (sort-by (juxt :hit-points :coord) attack-targets))]
    attack-target))

(defn neighbour-coords [[y x]]
  [[(inc y) x]
   [(dec y) x]
   [y (inc x)]
   [y (dec x)]])

(defn attack [{:keys [area unit-coords unit] :as acc}]
  (let [targets (filter (fn [u] (= (:unit-type u)
                                  (:target-unit-type unit)))
                        (map (fn [c] (get-in area c))
                             (neighbour-coords (:coord unit))))]
    (if-let [attack-target (find-attack-target targets unit)]
      ;; Found target
      (let [new-target-hit-points (- (:hit-points attack-target)
                                     (:attack-power unit))]
        (if (<= new-target-hit-points 0)
          ;; Target dies
          {:area (assoc-in area (:coord attack-target) {:type :open})
           :unit-coords (disj unit-coords (:coord attack-target))
           :unit unit}
          ;; Target loses hit points
          {:area (assoc-in area (conj (:coord attack-target) :hit-points) new-target-hit-points)
           :unit-coords unit-coords
           :unit unit}))
      ;; No attack Target
      acc)))

(defn shortest-to-target [area unit target]
  (->> (shortest-paths (AreaGraph. area)
                       (:coord unit))
       (filter (fn [[coord dist path]] (= (:coord target) coord)))))



(defn move [{:keys [area unit-coords unit] :as acc}]
  (if (seq (filter (fn [c] (= (:unit-type (get-in area c))
                             (:target-unit-type unit)))
                   (neighbour-coords (:coord unit))))
    acc
    (let [targets (filter (fn [u] (= (:target-unit-type unit)
                                    (:unit-type u)))
                          (map (fn [coord] (get-in area coord))
                               unit-coords))
          g (AreaGraph. area)
          target-neighbour-coords
          (set (mapcat (fn [t] (neighbors g (:coord t))) targets))
          target-neighbour-paths
          (->> (shortest-paths g (:coord unit))
               (filter (fn [[coord dist path]] (contains? target-neighbour-coords coord))))]
      (if (not (empty? target-neighbour-paths))
        (let [[coord dist path] (->> target-neighbour-paths
                                     (sort-by (juxt second first))
                                     (first))
              new-unit-coord (second path)
              new-unit (assoc unit :coord new-unit-coord)]
          {:area (-> area
                     (assoc-in (:coord unit) {:type :open})
                     (assoc-in new-unit-coord new-unit))
           :unit-coords (-> unit-coords
                            (disj (:coord unit))
                            (conj new-unit-coord))
           :unit new-unit})
        acc))))

(defn turn [{:keys [area unit-coords unit] :as acc}]
  (let [targets (filter (fn [u] (= (:target-unit-type unit)
                                  (:unit-type u)))
                        (map (fn [coord] (get-in area coord))
                             unit-coords))]
    (if (empty? targets)
      ;; No targets - End of Combat
      (do
        (println unit)
        :end-of-combat)
      ;; In range of a target - Attack
      (->>  acc
            (move)
            (attack)))))

(defn round [{:keys [area unit-coords] :as acc}]
  (loop [res acc
         sorted-unit-coords (sort unit-coords)]
    (if-let [unit-coord (first sorted-unit-coords)]
      (let [x (get-in (:area res) unit-coord)]
        (if (= :unit (:type x))
          (let [after-turn (turn (assoc res :unit x))]
            (if (= after-turn :end-of-combat)
              (assoc res
                     :end-of-combat true
                     :full (empty? (rest sorted-unit-coords)))
              (recur after-turn (rest sorted-unit-coords))))
          (recur res (rest sorted-unit-coords))))
      res)))

(defn get-unit-coords [area]
  (remove nil?
          (for [[y row] (map-indexed vector area)
                [x n] (map-indexed vector row)]
            (when (= (:type n) :unit)
              [y x]))))

(defn part-one [input]
  (let [area (parse-input input)
        unit-coords (set (get-unit-coords area))]
    (println "initial state")
    (println)
    (print-area area)
    (println)
    (let [res (loop [acc {:unit-coords unit-coords
                          :area area}
                     i 1
                     complete-rounds 0]
                (let [r (round acc)]
                  (println "round" i)
                  ;; (print-area (:area r))
                  (if (:end-of-combat r)
                    (do
                      (println "END OF COMBAT DURING ROUND" i)
                      (assoc r :full-rounds-complete complete-rounds))
                    (recur r (inc i) (inc complete-rounds)))))
          hit-points-total (reduce + (map :hit-points (map (fn [c] (get-in (:area res) c)) (:unit-coords res))))
          outcome (* (:full-rounds-complete res)
                     hit-points-total)]
      (println "full" (:full res))
      (println "round count" (:full-rounds-complete res))
      (println "hit points" hit-points-total)
      (println "finished area")
      (println)
      (print-area (:area res))
      (println)
      outcome)))

(comment
  (assert (= (part-one (get-sample 1)) 27730))
  (assert (= (part-one (get-sample 2)) 36334))
  (assert (= (part-one (get-sample 3)) 39514))
  (assert (= (part-one (get-sample 4)) 27755))
  (assert (= (part-one (get-sample 5)) 28944))
  (assert (= (part-one (get-sample 6)) 18740))

  (def res (part-one input-lines))
  ;; still a bug here somewehere
  ;; Part One - 191216
  ;; Part Two - 48050
  )
