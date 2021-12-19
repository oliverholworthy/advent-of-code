(ns advent-of-code.2021.19
  "Day 19: Beacon Scanner"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse-beacon [line]
  (mapv (fn [s] (Long/parseLong s)) (str/split line #",")))

(defn parse-scanner [lines]
  (when (seq lines)
    (when-let [[_ n] (re-matches #"--- scanner (\d+) ---" (first lines))]
      {:number (Long/parseLong n)
       :beacons (mapv parse-beacon (rest lines))})))

(defn parse-input [s]
  (reduce (fn [scanners lines]
            (if-let [{:keys [number beacons]} (parse-scanner lines)]
              (assoc scanners number beacons) scanners))
          {}
          (partition-by empty? (str/split-lines s))))

(def input (parse-input (slurp (io/resource "2021/19.txt"))))
(def input-sample (parse-input (slurp (io/resource "2021/19.sample.txt"))))

(defn rotation-x [theta]
  [[1 0 0]
   [0 (int (Math/cos theta)) (* -1 (int (Math/sin theta)))]
   [0 (int (Math/sin theta)) (int (Math/cos theta))]])

(defn rotation-y [theta]
  [[(int (Math/cos theta)) 0 (int (Math/sin theta))]
   [0 1 0]
   [(* -1 (int (Math/sin theta))) 0 (int (Math/cos theta))]])

(defn rotation-z [theta]
  [[(int (Math/cos theta)) (* -1 (int (Math/sin theta))) 0]
   [(int (Math/sin theta)) (int (Math/cos theta)) 0]
   [0 0 1]])

(defn shape [X] [(count X) (count (first X))])

(defn mm [A B]
  (let [[m n] (shape A)
        [nb p] (shape B)]
    (assert (= n nb))
    (mapv (fn [i]
            (mapv (fn [j]
                    (reduce (fn [acc k] (+ acc
                                          (* (get-in A [i k])
                                             (get-in B [k j]))))
                            0 (range n)))
                  (range p)))
          (range m))))

(defn rotations-24 []
  (let [rotate-4
        (fn [m] (for [i (range 4)]
                 (mm m (rotation-x (* i (/ (Math/PI) 2))))))]
    (concat
     (rotate-4 (rotation-y 0))
     (rotate-4 (rotation-y Math/PI))
     (rotate-4 (rotation-y (/ Math/PI 2)))
     (rotate-4 (rotation-y (* -1 (/ Math/PI 2))))
     (rotate-4 (rotation-z (/ Math/PI 2)))
     (rotate-4 (rotation-z (* -1 (/ Math/PI 2)))))))

(defn basis-vector [beacons1 beacons2]
  "returns vector difference between beacons of scanners if overlap by least 12 beacons"
  (:res (reduce (fn [{:keys [freqs]} [b1 b2]]
                  (let [s (mapv - b1 b2)
                        c (get freqs s 0)]
                    (if (= c 11)
                      (reduced {:freqs freqs :res s})
                      {:freqs (assoc freqs s (inc c))})))
                {}
                (for [b1 beacons1 b2 beacons2] [b1 b2]))))

(defn intersection [beacons1 beacons2]
  "Returns tuple of vector and rotation between two sets of scanner beacons if they overlap"
  (reduce (fn [acc rotation]
            (if-let [v (basis-vector beacons1 (mm beacons2 rotation))]
              (reduced [v rotation])
              acc))
          nil
          (rotations-24)))

(defn update-scanner [scanners [i {:keys [basis rotation]}]]
  (update scanners i (fn [beacons]
                       (mapv (fn [b] (mapv + b basis))
                             (mm beacons rotation)))))

(defn find-scanner-locations [scanners]
  (loop [frontier (priority-map 0 1)
         visited #{}
         scanners-0 (select-keys scanners [0])
         scanner-locations {}]
    (let [current-scanner (first (peek frontier))]
      (if (nil? current-scanner)
        scanner-locations
        (let [basis-vectors
              (reduce (fn [acc i]
                        (if-let [[v rotation]
                                 (intersection (get scanners-0 current-scanner)
                                               (get scanners i))]
                          (assoc acc i {:basis v :rotation rotation})
                          acc))
                      {}
                      (remove scanners-0 (keys scanners)))
              new-scanners-0
              (reduce (fn [acc [i {:keys [basis rotation]}]]
                        (assoc acc i (mapv (fn [b] (mapv + b basis))
                                           (mm (get scanners i) rotation))))
                      scanners-0
                      basis-vectors)]
          (recur (reduce (fn [acc i] (cond-> acc
                                      (and (not= i current-scanner)
                                           (not (contains? visited i)))
                                      (assoc i 1)))
                         (pop frontier)
                         (keys basis-vectors))
                 (conj visited current-scanner)
                 new-scanners-0
                 (merge scanner-locations basis-vectors)))))))

(defn align [scanners]
  (reduce update-scanner scanners (find-scanner-locations scanners)))

(defn part-one [scanners]
  "How many beacons are there?"
  (count (distinct (apply concat (vals (align scanners))))))

(defn manhattan [v1 v2]
  (reduce + (map (fn [p q] (Math/abs (- p q))) v1 v2)))

(defn part-two [scanners]
  "What is the largest Manhattan distance between any two scanners?"
  (let [scanner-locations (find-scanner-locations scanners)]
    (apply max (for [a (vals loc)
                     b (vals loc)]
                 (manhattan (:basis a) (:basis b))))))

(comment
  (def a (part-one input))
  (def b (part-two input))
  )
