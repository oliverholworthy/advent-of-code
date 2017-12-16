(ns advent-of-code.2017.12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn group* [seen node connections]
  (if (contains? @seen node)
    '()
    (do (swap! seen conj node)
        (cons node
              (mapcat #(group* seen % (dissoc connections node))
                      (remove @seen (get connections node)))))))

(defn node-group [node connections]
  (group* (atom #{}) node connections))

(defn groups [connections]
  (loop [gs []
         connections connections]
    (if-let [x (first connections)]
      (let [g (node-group (first x) connections)]
        (recur (conj gs g)
               (apply dissoc connections g)))
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
  (count (node-group "0" input))


  ;; Part Two
  (count (groups input))
  )
