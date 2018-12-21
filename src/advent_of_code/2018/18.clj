(ns advent-of-code.2018.18
  "Day 18: Settlers of The North Pole"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines-sample
  (str/split-lines
   (slurp (io/resource "2018/18/input-sample.txt"))))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/18/input.txt"))))


(defn parse-map [lines]
  (reduce (fn [g line]
            (conj g
                  (reduce (fn [r c] (conj r
                                         (case (str c)
                                           "." :open "|" :trees "#" :lumberyard)))
                          []
                          line)))
          []
          lines))

(defn print-map [g]
  (doseq [row g]
    (println
     (str/join (map (fn [c] (case c :open "." :trees "|" :lumberyard "#"))
                    row)))))

(defn adjacent-cells [g [y x]]
  (reduce (fn [acc [fy fx]]
            (if-let [t (get-in g [(fy y) (fx x)])]
              (conj acc t)
              acc))
   []
   [[dec dec]
    [dec identity]
    [dec inc]
    [identity dec]
    [identity inc]
    [inc dec]
    [inc identity]
    [inc inc]]))

(defn step [g]
  (reduce (fn [acc coord]
            (let [t (get-in g coord)
                  adj-freq (frequencies (adjacent-cells g coord))]
              (cond-> acc
                (and (= :open t) (>= (:trees adj-freq 0) 3))
                (assoc-in coord :trees)
                (and (= :trees t) (>= (:lumberyard adj-freq 0) 3))
                (assoc-in coord :lumberyard)
                (and (= :lumberyard t)
                     (not (and (>= (:lumberyard adj-freq 0) 1)
                               (>= (:trees adj-freq 0) 1))))
                (assoc-in coord :open))))
          g
          (for [y (range (count g))
                x (range (count (first g)))]
            [y x])))

(defn resource-value [g]
  (let [freqs (frequencies (flatten g))]
    (* (:trees freqs 0) (:lumberyard freqs 0))))

(defn part-one [lines n]
  (->> (parse-map lines)
       (iterate step)
       (drop n)
       (first)
       (resource-value)))

(defn find-repeated-subseq [v n]
  (let [min-seq-len n]
    (loop [i 0
           fqs {}
           prev-freq 0
           same-freq-count 0
           xs v
           repeat-start 0
           repeat-end 0]
      (if (and
           (> repeat-start 0)
           (> repeat-end 0))
        [repeat-start repeat-end]
        (when-let [x (first xs)]
          (let [freq (inc (get fqs x 0))
                freq-seq
                (if (and (> freq 0) (= freq prev-freq))
                  (inc same-freq-count)
                  0)]
            (recur (inc i)
                   (update fqs x (fn [c] freq))
                   freq
                   freq-seq
                   (rest xs)
                   (if (and (= freq 2) (>= freq-seq min-seq-len))
                     (- i freq-seq)
                     repeat-start)
                   (if (and (= freq 3) (>= freq-seq min-seq-len))
                     (- i freq-seq)
                     repeat-end))))))))

(defn part-two [input-lines mins resource-count repeat-count]
  (let [resource-values
        (->> (parse-map input-lines)
             (iterate step)
             (take resource-count)
             (map resource-value)
             (vec))
        [s e] (find-repeated-subseq resource-values repeat-count)]
    (when (and s e)
      (let [repeated-seq (subvec resource-values s e)]
        (get repeated-seq (mod (- mins s) (- e s)))))))

(comment
  (print-map (parse-map input-lines-sample))
  (part-one input-lines 10)
  (part-two input-lines
            1000000000 ;; mins
            600 ;; max iterations to search
            20) ;; min repeated freq count
  )

