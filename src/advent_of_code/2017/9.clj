(ns advent-of-code.2017.9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def input (str/trim (slurp (io/resource "2017/9/input.txt"))))

(defn read-stream [stream]
  (reduce (fn [acc char]
            (let [{:keys [res path]} acc]
              (cond
                (= char \{) (assoc acc
                                   :res (if res (update-in res path (fn [x] (if x (conj x []) []))) [])
                                   :path (conj path 0))
                (= char \,) (assoc acc
                                   :path (update path (dec (count path)) inc))
                (= char \}) (assoc acc
                                   :path (pop path)))))
          {:path []}
          stream))

(group-count (:res (read-stream "{{{},{},{{}}}}")))


(defn group-count [groups]
  (let [map-count (atom 0)]
    (walk/postwalk (fn [x]
                     (when (vector? x) (swap! map-count inc))
                     x)
                   groups)
    @map-count))
