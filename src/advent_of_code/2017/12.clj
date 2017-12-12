(ns advent-of-code.2017.12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn program-group [program pipes]
  (if-let [programs (get pipes program)]
    (conj (set (mapcat #(program-group % (dissoc pipes program)) programs))
          program)
    #{program}))

(defn groups [pipes]
  (loop [gs []
         pipes pipes]
    (if-let [x (first pipes)]
      (let [g (program-group (first x) pipes)]
        (recur (conj gs g)
               (apply dissoc pipes g)))
      gs)))

;; -----------------------------------------------------------------------------

(defn read-input [resource]
  (->> (str/split-lines (str/trim (slurp resource)))
       (map (fn [line]
              (let [[source targets] (str/split line #" <-> ")
                    targets (set (map str/trim (str/split targets #",")))]
                [source targets])))
       (into {})))

(def input (read-input (io/resource "2017/12/input.txt")))
(def input-sample (read-input (io/resource "2017/12/input-sample.txt")))

(comment
  ;; Part One
  (count (program-groups "0" input))

  ;; Part Two
  (count (groups input))
  )
