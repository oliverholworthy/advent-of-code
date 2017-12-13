(ns advent-of-code.2017.13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [resource]
  (map (fn [line] (let [[a b] (str/split line #":")]
                   {:depth (Long/parseLong (str/trim a))
                    :range (Long/parseLong (str/trim b))}))
       (str/split-lines (slurp resource))))

(defn severity [delay layer]
  (if (caught? delay layer)
    (* (:range layer) (:depth layer))
    0))

(defn caught? [delay layer]
  (zero? (mod (+ delay (:depth layer))
              (- (* (:range layer) 2) 2))))

(defn safe? [delay firewalls]
  (every? #(not (caught? delay %)) firewalls))

;; -----------------------------------------------------------------------------

(def input (read-input (io/resource "2017/13/input.txt")))
(def input-sample (read-input (io/resource "2017/13/input-sample.txt")))

(comment

  ;; Part One

  (reduce + (map #(severity 0 %) input))
  ;; => 1632


  ;; Part Two

  (let [firewalls input]
    (->> (range)
         (map-indexed (fn [i v] {:delay i :caught? (not (safe? v firewalls))}))
         (drop-while :caught?)
         (first)
         (:delay)))
  ;; => 3834136
  )
