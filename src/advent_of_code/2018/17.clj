(ns advent-of-code.2018.17
  "Day 17: Reservoir Research"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/17/input.txt"))))

(defn parse-input [lines]
  (let [x-ys-re #"x=(\d+), y=(\d+)..(\d+)"
        y-xs-re #"y=(\d+), x=(\d+)..(\d+)"]
    (reduce (fn [acc line]
              (cond (re-matches x-ys-re line)
                    (let [[_ x y1 y2] (re-find x-ys-re line)
                          [x y1 y2] (mapv #(Long/parseLong %) [x y1 y2])]
                      (conj acc {:x [x x] :y [y1 y2]}))

                    (re-matches y-xs-re line)
                    (let [[_ y x1 x2] (re-find y-xs-re line)
                          [y x1 x2] (mapv #(Long/parseLong %) [y x1 x2])]
                      (conj acc {:x [x1 x2] :y [y y]}))))
            []
            lines)))

(defn max-coords [coords]
  (reduce (fn [acc {:keys [x y]}]
            (let [update-fn (fn [vs f] (fn [v] (apply f (cond-> vs v (conj v)))))]
              (-> acc
                  (update :x-min (update-fn x min))
                  (update :x-max (update-fn x max))
                  (update :y-min (update-fn y min))
                  (update :y-max (update-fn y max)))))
          {}
          coords))

(defn get-grid [input-coords]
  (let [{:keys [x-min x-max y-min y-max]} (max-coords input-coords)]
    (vec (for [y (range 0 (inc y-max))]
           (vec (for [x (range (dec x-min) (inc x-max))]
                  {:cell/type :sand}))))))

(defn coord-range [{:keys [x y]}]
  (let [[x1 x2] x [y1 y2] y]
    (for [y (range y1 (inc y2))
          x (range x1 (inc x2))]
      [y x])))

(defn fill-grid [grid input-coords]
  (let [{:keys [x-min]} (max-coords input-coords)]
    (reduce (fn [acc coord]
              (reduce (fn [g [y x]]
                        (when (= 1831 y)
                          (println [y x]))
                        (assoc-in g (conj [y (- x x-min)]
                                          :cell/type)
                                  :clay))
                      acc
                      (coord-range coord)))
            grid
            input-coords)))

(defn print-grid [grid]
  (println
   (str/join "\n"
             (for [row grid]
               (str/join (for [cell row]
                           (case (:cell/type cell)
                             :sand "."
                             :clay "#"
                             :spring "+")))))))

(comment
  (let [input-coords (parse-input input-lines)
        grid (get-grid input-coords)
        grid (fill-grid grid input-coords)
        grid (assoc-in grid [0 500 :cell/type] :spring)]
    (print-grid grid))
  )
