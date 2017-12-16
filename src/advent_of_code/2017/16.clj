(ns advent-of-code.2017.16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [resource]
  (map (fn [move]
         (case (first move)
           \s [:spin (Long/parseLong (subs move 1))]
           \x (let [[a b] (str/split (subs move 1) #"/")]
                [:exchange [(Long/parseLong a)  (Long/parseLong b)]])
           \p (let [[a b] (str/split (subs move 1) #"/")]
                [:partner [a b]])))
       (str/split (str/trim (slurp resource)) #",")))

(defn spin [n coll]
  (vec (flatten (reverse (split-at (- (count coll) n) coll)))))

(defn partner [a b coll]
  (replace {a b b a} coll))

(defn exchange [a b coll]
  (partner (nth coll a) (nth coll b) coll))

(defn dance
  [programs moves]
  (reduce (fn [ps [move move-val]]
            (case move
              :spin (spin move-val ps)
              :exchange (exchange (first move-val) (second move-val) ps)
              :partner (partner (first move-val) (second move-val) ps)))
          programs
          moves))

(defn position-cycle [programs moves]
  (loop [i 0
         ps (dance programs moves)
         ds [ps]
         seen #{}]
    (let [next (dance ps moves)]
      (if (contains? seen next)
        ds
        (recur (inc i)
               next
               (conj ds next)
               (conj seen next))))))

;; -----------------------------------------------------------------------------

(def input (read-input (io/resource "2017/16/input.txt")))
(def input-sample (read-input (io/resource "2017/16/input-sample.txt")))



(comment
  (->> ["a" "b" "c" "d" "e"]
       (spin 1)
       (exchange 3 4)
       (partner "e" "b"))

  (def programs (map str "abcdefghijklmnop"))
  (def moves input)

  ;; Part One

  (str/join (dance programs moves))
  ;; => "gkmndaholjbfcepi"

  ;; Part Two

  (time (def dance-cycle (position-cycle programs moves)))
  (str/join (nth dance-cycle (inc (rem 1e9 61))))
  ;; => "abihnfkojcmegldp"
  )
