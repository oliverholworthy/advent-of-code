(ns advent-of-code.2020.22
  "Day 22: Crab Combat"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (str/split (slurp (io/resource "2020/22/input.txt")) #"\n\n"))

(def input-sample
  (str/split (slurp (io/resource "2020/22/sample.txt")) #"\n\n"))

(defn parse-player [p]
  (map #(Long/parseLong %) (rest (str/split-lines p))))

(defn parse-input [input]
  (let [[p1 p2] input]
    [(parse-player p1) (parse-player p2)]))

(defn score
  "Get score from a list of cards"
  [cards]
  (reduce + (map-indexed (fn [i card] (* (inc i) card)) (reverse cards))))

(defn move-cards
  "move cards between players depending on winner"
  [player-cards p1-winner]
  (let [[cards-a cards-b] player-cards
        [play-card-a rest-cards-a] ((juxt first rest) cards-a)
        [play-card-b rest-cards-b] ((juxt first rest) cards-b)]
    (if p1-winner
      [(concat rest-cards-a (list play-card-a play-card-b)) rest-cards-b]
      [rest-cards-a (concat rest-cards-b (list play-card-b play-card-a))])))

(defn round
  "play a round of Combat between two players. The player with the
  highest drawn card keeps the cards"
  [player-cards]
  (let [[cards-a cards-b] player-cards]
    (if (or (empty? cards-a) (empty? cards-b))
      player-cards
      (move-cards player-cards (> (first cards-a) (first cards-b))))))

(defn game-score
  "return score of game. the score of the player with all the cards"
  [player-cards]
  (let [[cards-a cards-b] player-cards
        winner-cards (if (seq cards-a) cards-a cards-b)]
    (score winner-cards)))

(defn game
  "play a game of Combat between two players. The player with all the
  cards wins."
  [player-cards]
  (game-score
   (first
    (drop-while (fn [[cards-a cards-b]] (and (seq cards-a) (seq cards-b)))
                (iterate round player-cards)))))

(defn game-recursive
  "Plays a game of recursive combat between two players"
  [player-cards n]
  (loop [round 1
         round-cards player-cards
         seen-rounds #{}]
    (cond
      (empty? (first round-cards))
      [2 (second round-cards)]
      (empty? (second round-cards))
      [1 (first round-cards)]
      (contains? seen-rounds round-cards)
      [1 (first round-cards)]
      :else
      (let [[cards-a cards-b] round-cards
            [play-card-a rest-cards-a] ((juxt first rest) cards-a)
            [play-card-b rest-cards-b] ((juxt first rest) cards-b)
            new-round-cards
            (if (and (>= (count rest-cards-a) play-card-a)
                     (>= (count rest-cards-b) play-card-b))
              (let [[winner winner-score]
                    (game-recursive [(take play-card-a rest-cards-a)
                                     (take play-card-b rest-cards-b)]
                                    (inc n))]
                (move-cards round-cards (= winner 1)))
              (move-cards round-cards (> play-card-a play-card-b)))]
        (recur (inc round) new-round-cards (conj seen-rounds round-cards))))))

(comment
  ;; Part One
  (game (parse-input input-sample))
  (game (parse-input input))

  ;; Part Two
  (score (second (game-recursive (parse-input input-sample) 1)))
  (score (second (game-recursive (parse-input input) 1)))
  )
