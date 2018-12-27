(ns advent-of-code.2018.25
  "Day 25: Four-Dimensional Adventure"
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input-lines-sample
  (str/split-lines
   (slurp (io/resource "2018/25/input-sample.txt"))))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/25/input.txt"))))

(defn parse-lines [lines]
  (map (fn [line]
         (mapv (fn [d] (Long/parseLong d))
               (str/split line #",")))
       lines))

(defprotocol IUnionFind
  (num-sets [uf])
  (rank [uf x])
  (parent [uf x])
  (find [uf x])
  (union [uf x y]))

(defrecord UnionFind [n meta]
  IUnionFind
  (num-sets [uf] (get meta :num-sets n))
  (rank [uf x] (get-in meta [:ranks x] 1))
  (parent [uf x] (get-in meta [:parents x]))
  (find [uf x]
    (if-let [p (parent uf x)]
      (let [[uf new-p] (find uf p)]
        [(assoc-in uf [:parents x] new-p) new-p])
      [uf x]))
  (union [uf x y]
    (let [[uf a] (find uf x)
          [uf b] (find uf y)]
      (if (= a b)
        uf
        (let [rank-a (rank uf a)
              rank-b (rank uf b)]
          (cond
            (< rank-a rank-b)
            (UnionFind. n
                        (-> meta
                            (assoc-in [:parents a] b)
                            (assoc :num-sets (dec (num-sets uf)))))
            (> rank-a rank-b)
            (UnionFind. n
                        (-> meta
                            (assoc-in [:parents b] a)
                            (assoc :num-sets (dec (num-sets uf)))))
            :else
            (UnionFind. n
                        (-> meta
                            (assoc-in [:ranks a] (inc rank-a))
                            (assoc-in [:parents b] a)
                            (assoc :num-sets (dec (num-sets uf)))))))))))

(defn union-find [n] (UnionFind. n {}))

(defn mdist [p1 p2]
  (reduce + (map #(Math/abs %) (mapv - p1 p2))))

(defn part-one [coords]
  (num-sets
   (:uf
    (reduce (fn [{:keys [uf seen] :as acc} p]
              (let [seen (conj seen p)]
                {:seen seen
                 :uf (reduce (fn [uf p2]
                               (if (<= (mdist p p2) 3)
                                 (union uf p p2)
                                 uf))
                             uf
                             seen)}))
            {:uf (union-find (count coords))
             :seen #{}}
            coords))))

(comment
  (time
   (def res-one (part-one (parse-lines input-lines))))
  )
