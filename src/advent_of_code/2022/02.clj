(ns advent-of-code.2022.02
  "Day 2:Rock Paper Scissors"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "2022/02.txt"))))
(def input-sample (str/split-lines (slurp (io/resource "2022/02.sample.txt"))))

(defn game-shapes [input-lines shape-type]
  (map (fn [line] (mapv (comp shape-type keyword) (str/split line #"\s"))) input-lines))

(def shape-value {:rock 1 :paper 2 :scissors 3})
(def game-value {:lose 0 :draw 3 :win 6})
(def wins-against {:rock :scissors :scissors :paper :paper :rock})
(def loses-against (into {} (map (comp vec reverse) wins)))

(defn game-result [opponent-shape player-shape]
  (cond (= player-shape opponent-shape) :draw
        (= (wins-against player-shape) opponent-shape) :win
        :else :lose))

(defn game-score [player-shape game-result]
  (+ (shape-value player-shape) (game-value game-result)))

(defn part-one [input]
  "total score be if strategy guide is the opponent and player shapes"
  (let [shape-type {:A :rock :B :paper :C :scissors :X :rock :Y :paper :Z :scissors}]
    (reduce + (map (fn [[opponent-shape player-shape]]
                     (game-score player-shape (game-result opponent-shape player-shape)))
                   (game-shapes input shape-type)))))

(defn find-player-shape [opponent-shape game-result]
  (case game-result
    :draw opponent-shape
    :lose (wins-against opponent-shape)
    :win (loses-against opponent-shape)))

(defn part-two [input]
  "total score if strategy guide is the opponent sahep and target game result (win/draw/lose)"
  (let [shape-type {:A :rock :B :paper :C :scissors :X :lose :Y :draw :Z :win}]
    (reduce + (map (fn [[opponent-shape game-result]]
                     (game-score (find-player-shape opponent-shape game-result) game-result))
                   (game-shapes input shape-type)))))

(comment
  (part-one input)
  (part-two input)
  )
