(ns advent-of-code.2021.20
  "Day 20: Trench Map"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [s]
  (let [[image-enhancement-algorithm _ image] (partition-by empty? (str/split-lines s))]
    {:enhancement (str/join image-enhancement-algorithm)
     :background \.
     :pixels (let [img (vec image)]
               (reduce (fn [acc [i j]]
                         (if (= (get-in img [i j]) \#)
                           (conj acc [i j])
                           acc))
                      #{}
                      (for [i (range (count img))
                            j (range (count (first img)))]
                        [i j])))}))

(def input (parse-input (slurp (io/resource "2021/20.txt"))))
(def input-sample (parse-input (slurp (io/resource "2021/20.sample.txt"))))

(defn pixel-range [pixels]
  (reduce (fn [{:keys [min-x min-y max-x max-y]} [x y]]
            {:min-x (min x min-x) :max-x (max x max-x)
             :min-y (min y min-y) :max-y (max y max-y)})
          {:min-x 0 :max-x 0 :min-y 0 :max-y 0}
          pixels))

(defn is-background? [[x y] {:keys [min-x min-y max-x max-y]}]
  (or (> x max-x) (< x min-x)
      (> y max-y) (< y min-y)))

(defn coords [pixels n bg]
  (let [{:keys [min-x min-y max-x max-y] :as pixel-min-max}
        (pixel-range pixels)]
    (for [x (range (- min-x n) (+ max-x n))
          y (range (- min-y n) (+ max-y n))]
      {:coord [x y]
       :value (if (contains? pixels [x y])
                \#
                (if (is-background? [x y] pixel-min-max)
                  bg
                  \.))})))

(defn print-image [{:keys [pixels background]}]
  (->> (coords pixels 3 background)
       (partition-by (comp first :coord) )
       (mapv (fn [row] (str/join (mapv (fn [{:keys [coord value]}] value) row))))
       (str/join "\n")
       (println)))

(defn pixel-filter [[i j]]
  [[(dec i) (dec j)] [(dec i) j] [(dec i) (inc j)]
   [i (dec j)] [i j] [i (inc j)]
   [(inc i) (dec j)] [(inc i) j] [(inc i) (inc j)]])

(defn get-filter-value [coord pixels pixel-min-max bg]
  (Integer/parseInt (str/join
                     (map (fn [c]
                            (if (or (contains? pixels c)
                                    (and (is-background? c pixel-min-max)
                                         (= bg \#)))
                              1 0))
                          (pixel-filter coord)))
                    2))

(defn enhance [{:keys [pixels background enhancement]}]
  (let [pixel-min-max (pixel-range pixels)]
    {:pixels (reduce (fn [acc {:keys [value coord]}]
                      (let [i (get-filter-value coord pixels pixel-min-max background)]
                        (cond-> acc
                          (= (nth enhancement i) \#)
                          (conj coord))))
                    #{}
                    (coords pixels 3 background))
     :background (if (= background \# (nth enhancement 0))
                   (nth enhancement 511)
                   (nth enhancement 0))
     :enhancement enhancement}))

(defn count-pixels-enchancement [image n]
  (count (:pixels (nth (iterate enhance image) n))))

(comment
  (print-image (enhance (enhance input-sample)))
  (count-pixels-enchancement input-sample 2)
  (count-pixels-enchancement input 2)
  (count-pixels-enchancement input 50)
  )
