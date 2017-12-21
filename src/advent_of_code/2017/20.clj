(ns advent-of-code.2017.20
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn read-vec [s]
  (let [[_ x y z] (re-find #".*<(.*),(.*),(.*)>" s)]
    [(Long/parseLong x) (Long/parseLong y) (Long/parseLong z)]))

(defn read-input [resource]
  (map (fn [line]
         (let [[p v a] (str/split line #", ")]
           {:position (read-vec p)
            :velocity (read-vec v)
            :acceleration (read-vec a)}))
       (str/split-lines (slurp resource))))

(defn position-at-tick [particle t]
  (let [v (:velocity particle)
        a (:acceleration particle)
        p0 (:position particle)]
    (mapv +
          p0
          (mapv -
                (mapv #(* % t) v)
                (mapv #(* % (/ (* t t) 2)) a)))))

(defn manhattan [v]
  (reduce + (map #(Math/abs (double %)) v)))

(defn closest-to-origin [particles t]
  (reduce (fn [[acc-i acc-dist] [i particle]]
            (let [dist (manhattan (position-at-tick particle t))]
              (if (or (zero? i) (< dist acc-dist))
                [i dist]
                [acc-i acc-dist])))
          [nil nil]
          (map-indexed vector particles)))

(defn quadratic-roots [a b c]
  (if (zero? a)
    (if (zero? b) [0] [(/ (* -1 c) b)])
    (let [d (Math/sqrt (- (* b b) (* 4 a c)))]
      [(/ (+ (* -1 b) d)
          (* 2 a))
       (/ (- (* -1 b) d)
          (* 2 a))])))

(defn collide? [p1 p2]
  (let [a (mapv - (:acceleration p1) (:acceleration p2))
        b (mapv (comp #(* 2 %) -)
                (mapv + (mapv #(* 0.5 %) (:acceleration p1)) (:velocity p1))
                (mapv + (mapv #(* 0.5 %) (:acceleration p2)) (:velocity p2)))
        c (mapv (comp #(* 2 %) -) (:position p1) (:position p2))
        collisions (apply set/intersection
                          (mapv (comp #(into #{} %) quadratic-roots) a b c))]
    (and (not (empty? collisions))
         (not (empty? (filter pos? collisions))))))

(defn remove-colliding [particles]
  (loop [i 0
         remaining (set particles)]
    (if (or (>= i (count particles)) (empty? remaining))
      remaining
      (recur (inc i)
             (let [particle (nth particles i)]
               (if (contains? remaining particle)
                 (let [collisions
                       (filter #(collide? particle %) (drop i particles))]
                   (if (empty? collisions)
                     remaining
                     (set/difference remaining
                                     (set (cons particle collisions)))))
                 remaining))))))

(comment

  (def particles (read-input (io/resource "2017/20/input.txt")))

  ;; Part One

  (first (last (map #(closest-to-origin particles %)
                    (take 10 (iterate #(* 2 %) 1)))))


  ;; Part Two

  (def remaining-particles (remove-colliding particles))
  (count remaining-particles)
  )
