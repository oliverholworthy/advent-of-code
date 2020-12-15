(ns advent-of-code.2020.15
  "Day 15: Rambunctious Recitation")

(defn play [start]
  (let [init
        {:i (dec (count start))
         :pos (into {} (map-indexed (fn [i x] [x i]) start))
         :number (last start)}]
    (concat
     (drop-last start)
     (map :number
          (iterate
           (fn [{:keys [i age pos number prev]}]
             (let [last-number number
                   next-number (- i (get pos last-number i))]
               {:i (inc i)
                :pos (assoc pos last-number i)
                :number next-number}))
           init)))))

(comment
  (nth (play [0 3 6]) (dec 2020))
  (def part-one (nth (play [13 0 10 12 1 5 8]) (dec 2020)))
  (def part-two (nth (play [13 0 10 12 1 5 8]) (dec 30000000)))
  )
