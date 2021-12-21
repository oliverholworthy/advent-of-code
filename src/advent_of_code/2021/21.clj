(ns advent-of-code.2021.21
  "Day 21: Dirac Dice")

(defn determinstic-die [] (cycle (range 1 101)))

(defn get-new-position [position roll] (inc (mod (+ position roll -1) 10)))

(defn move [{:keys [die scores player moves positions] :as game}]
  (let [rolls 3
        move (reduce + (take rolls die))
        player-position (get positions player)
        new-position (get-new-position player-position move)]
    (-> game
        (update :die (partial drop rolls))
        (update :moves inc)
        (update :rolls + rolls)
        (update :player (fn [i] (mod (inc i) (count scores))))
        (assoc-in [:positions player] new-position)
        (update-in [:scores player] (fn [s] (+ s new-position))))))

(defn game [positions]
  {:die (take 1000 (determinstic-die))
   :scores (mapv (constantly 0) positions)
   :player 0 :moves 0 :positions positions :rolls 0})

(defn part-one [positions]
  (let [{:keys [scores rolls]}
        (first (drop-while (fn [{:keys [scores]}] (< (apply max scores) 1000))
                           (iterate move (game positions))))]
    (* rolls (apply min scores))))

(defn dirac-moves []
  (for [a [1 2 3]
        b [1 2 3]
        c [1 2 3]]
    (reduce + [a b c])))

(def dirac-game
  (memoize
   (fn [[position-1 position-2] [score-1 score-2]]
     (if (or (>= score-1 21) (>= score-2 21))
       [0 1]
       (reduce
        (fn [scores move]
          (let [new-position-1 (get-new-position position-1 move)
                new-score-1 (+ score-1 new-position-1)]
            (mapv +
                  scores
                  (reverse (dirac-game [position-2 new-position-1]
                                       [score-2 new-score-1])))))
        [0 0]
        (dirac-moves))))))

(comment
  (part-one [4 8])
  (part-one [8 2])
  (apply max (dirac-game [4 8] [0 0]))
  )
