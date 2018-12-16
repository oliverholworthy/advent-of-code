(ns advent-of-code.2018.14
  "Day 14: Chocolate Charts")

(defn step [{:keys [recipes elves]}]
  (let [elf-scores (map recipes elves)
        total-score (reduce + elf-scores)
        new-recipes (map (fn [c] (Long/parseLong (str c))) (str total-score))
        recipes (reduce conj recipes new-recipes)]
    {:recipes recipes
     :elves (mapv (fn [elf-idx elf-score]
                    (mod (+ elf-idx (inc elf-score))
                         (count recipes)))
                  elves elf-scores)}))

(defn part-one [n]
  (->> (iterate step {:recipes [3 7] :elves [0 1]})
       (drop-while (fn [{:keys [recipes]}] (< (count recipes) (+ n 10))))
       (first)
       (:recipes)
       (drop n)
       (take 10)))

(defn part-two [score-seq]
  (->> (iterate step {:recipes [3 7] :elves [0 1]})
       (drop-while (fn [{:keys [recipes]}] (not= (take-last (count score-seq) recipes)
                                                score-seq)))
       (first)
       (:recipes)
       (drop-last (count score-seq))
       (count)))

(comment
  (str/join (part-one 793061)) ;; => "4138145721"
  (time (def res-two (part-two [7 9 3 0 6 1]))) ;; => 20276284
  )
