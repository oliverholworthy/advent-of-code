(ns advent-of-code.2021.03
  "Day 3: Binary Diagnostic"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "2021/03.txt"))))

(defn indexed-char-freq [state x]
  (reduce (fn [acc [i c]] (update-in acc [i c] (fn [v] (inc (or v 0)))))
          state
          (map-indexed vector x)))

(defn char-freqs [xs] (reduce indexed-char-freq {} xs))

(defn gamma-epsilon [xs]
  (let [freqs (char-freqs xs)
        gamma-rate-binary
        (str/join (reduce (fn [acc i]
                            (conj acc (ffirst (sort-by (fn [[k v]] v)
                                                       >
                                                       (get freqs i)))))
                          [] (sort (keys freqs))))
        epsilon-binary (str/join (map (fn [c] (if (= c \1) \0 \1)) gamma-rate-binary))]
    {:gamma (Long/parseLong gamma-rate-binary 2)
     :epsilon (Long/parseLong epsilon-binary 2)}))

(defn part-one [xs]
  (let [{:keys [gamma epsilon]} (gamma-epsilon xs)]
    (* gamma epsilon)))

(defn bit-criteria [xs rev]
  (let [res
        (reduce (fn [acc i]
                  (if (= (count acc) 1)
                    (reduced acc)
                    (let [freqs (char-freqs acc)
                          v (ffirst (cond-> (sort-by (fn [[k v]] [v (if (= k \1) 1 0)])
                                                     (get freqs i))
                                      rev
                                      (reverse)))]
                      (filter (fn [x] (= (get x i) v)) acc))))
                xs
                (range (count (first xs))))]
    (if (= (count res) 1)
      (first res)
      res)))

(defn part-two [xs]
  (let [o2-binary (bit-criteria xs true)
        co2-binary (bit-criteria xs false)
        o2 (Long/parseLong o2-binary 2)
        co2 (Long/parseLong co2-binary 2)]
    (* o2 co2)))

(comment
  (part-one input)
  (part-two input)
  )
