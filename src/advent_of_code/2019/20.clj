(ns advent-of-code.2019.20
  "Day 20: Donut Maze"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]))

(def input (str/split-lines (slurp (io/resource "2019/20/input.txt"))))

(defn get-portal* [grid coord]
  (let [[y x] coord]
    (reduce (fn [acc c]
              (if (re-matches #"[A-Z]" (str (get-in grid c)))
                {:coord c :char (get-in grid c)}
                acc))
            {}
            [[y (dec x)]
             [(dec y) x]
             [(inc y) x]
             [y (inc x)]])))

(defn get-portal [grid coord]
  "Return the portal name if there is one for this coordinate otherwise nil"
  (when (= \. (get-in grid coord))
    (let [c1 (get-portal* grid coord)
          char1 (:char c1)
          coord1 (:coord c1)
          c2 (when coord1 (get-portal* grid coord1))
          char2 (:char c2)
          coord2 (:coord c2)]
      (when (and char1 char2)
        (str char1 char2)))))

(comment
  (mapv #(get-portal input [2 %]) (range 31 36))
  ;; => [nil nil "XS" nil nil]
  )

(defn grid-coords [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    [y x]))

(defn get-portal-mapping [grid]
  (let [portals
        (reduce (fn [acc coord]
                  ;; Assuming the ordering doesn't matter in portal names
                  ;; e.g. AL = LA
                  (let [portal (set (get-in grid (conj coord :portal)))]
                    (cond-> acc
                      (seq portal)
                      (update portal (fn [xs] (conj (or xs []) coord))))))
                {}
                (grid-coords grid))]
    (reduce (fn [acc k] (let [v (get acc k)]
                             (if (= (count v) 2)
                               (assoc acc k (into {} [v (vec (reverse v))]))
                               (dissoc acc k)))) portals (keys portals))))


(defn parse-grid
  [input]
  (let [y-max (count input)
        x-max (count (first input))
        edge-width 4
        grid (mapv
              (fn [[y row]]
                (mapv (fn [[x c]]
                        (let [portal (get-portal input [y x])]
                          (cond-> {:coord [y x] :char c}
                            portal
                            (assoc :portal portal
                                   :portal-position
                                   (if (and (> y edge-width) (< y (- y-max edge-width))
                                            (> x edge-width) (< x (- x-max edge-width)))
                                     :inner
                                     :outer)))))
                      (map-indexed vector row)))
              (map-indexed vector input))
        portals (get-portal-mapping grid)]
    (reduce (fn [acc coord] (let [portal (set (:portal (get-in grid coord)))
                                 portal-to (get-in portals [portal coord])]
                             (cond-> acc
                               portal-to
                               (assoc-in (conj coord :portal-to) portal-to))))
            grid (grid-coords grid))))

(defn get-neighbours [area v]
  (let [[y x] v
        portal-to (:portal-to (get-in area v))
        explore-coords [[y (dec x)]
                        [(dec y) x]
                        [(inc y) x]
                        [y (inc x)]]
        explore-coords (cond-> explore-coords portal-to (conj portal-to))]
    (reduce (fn [{:keys [neighbours area]} coord]
              (let [node (get-in area coord)]
                {:area area
                 :neighbours
                 (cond-> neighbours
                   (and node (#{\.} (:char node)))
                   (conj coord))}))
            {:area area :neighbours []}
            explore-coords)))

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

(defn get-neighbours-recursive [area v]
  (let [[level y x] v
        node (get-in area [y x])
        portal-to (:portal-to node)
        portal-level (case (:portal-position node) :inner (inc level) :outer (dec level) nil)
        explore-coords (mapv (fn [c] {:coord c :level level})
                            [[y (dec x)]
                             [(dec y) x]
                             [(inc y) x]
                             [y (inc x)]])
        ;; _ (when (and portal-to portal-level (>= portal-level 0) (= (:portal-position node) :outer))
        ;;     (println "outer" "level" level "->" portal-level))
        explore-coords (cond-> explore-coords
                         (and portal-to portal-level (>= portal-level 0))
                         (conj {:coord portal-to :level portal-level}))]
    (reduce (fn [neighbours {:keys [coord] :as neighbour}]
              (let [node (get-in area coord)]
                (cond-> neighbours
                  (and node (#{\.} (:char node)))
                  (conj [(:level neighbour) (first coord) (second coord)]))))
            []
            explore-coords)))

(defn merge-frontier
  [val-in-result val-in-latter]
  (first (sort-by second [val-in-result val-in-latter])))

(defn shortest-paths-recursive
  [area-init start neighbours-fn]
  ((fn explore [explored frontier area]
     (lazy-seq
      (when-let [[v [total-cost previous-vertex]] (peek frontier)]
        (let [path (conj (explored previous-vertex []) v)
              neighbours (neighbours-fn area v)
              node (get-in area (subvec v 1 3))]
          (cons [(assoc node :loc v) total-cost path]
                (explore (assoc explored v path)
                         (merge-with
                          merge-frontier
                          (pop frontier)
                          (into {}
                                (for [n (remove explored neighbours)]
                                  {n [(+ total-cost 1) v]})))
                         area))))))
   {}
   (priority-map start [0])
   area-init))

(defn print-grid [g]
  (println (str/join "\n"
                     (mapv (fn [row] (str/join
                                     (map (fn [n] (case (:portal-position n)
                                                   :inner "?"
                                                   :outer "@"
                                                   (:char n))) row)))
                           g))))

(comment
  ;; Part One
  (time
   (def res-one
     (let [grid (parse-grid input)
           start (first (filter (fn [coord] (= "AA" (:portal (get-in grid coord)))) (grid-coords grid)))
           end (first (filter (fn [coord] (= "ZZ" (:portal (get-in grid coord)))) (grid-coords grid)))]
       (first
        (filter (fn [[{:keys [coord]} _ _]] (= coord end))
                (shortest-paths grid start get-neighbours))))))

  (print-grid (parse-grid input))

  ;; Part Two
  (time
   (def res-two
     (let [grid (parse-grid input)
           start (first (filter (fn [coord] (= "AA" (:portal (get-in grid coord)))) (grid-coords grid)))
           end (first (filter (fn [coord] (= "ZZ" (:portal (get-in grid coord)))) (grid-coords grid)))]
       (first
        (filter (fn [[i [{:keys [loc]} _ _]]]
                  (and (= 0 (first loc))
                       (= end (subvec loc 1 3))))
                (map-indexed vector (shortest-paths-recursive grid
                                                              [0 (first start) (second start)]
                                                              get-neighbours-recursive)))))))
  )
