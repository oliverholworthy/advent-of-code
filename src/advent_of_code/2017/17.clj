(ns advent-of-code.2017.17)

(defn spinlock-step [{:keys [spinlock step pos]}]
  (let [n (count spinlock)
        new-pos (mod (+ step pos) n)]
    {:step step
     :pos (inc new-pos)
     :spinlock (into (conj (subvec spinlock 0 (inc new-pos)) n)
                     (subvec spinlock (inc new-pos)))}))

(defn spinlock [step]
  (iterate spinlock-step {:spinlock [0] :pos 0 :step step}))

(defn spinlock-pos [step]
  (iterate (fn [[pos n]] [(mod (+ step pos) (inc n)) (inc n)]) [0 0]))

;; -----------------------------------------------------------------------------

(comment

  (def step-size 380)

  ;; Part One

  (time
   (let [n 2017
         {:keys [pos spinlock]}
         (first (drop n (spinlock step-size)))]
     (get spinlock (mod (inc pos) n))))
  ;; => 204


  ;; Part Two

  (time
   (def part-two
     (->> (spinlock-pos step-size)
          (take 50e6)
          (filter #(zero? (first %)))
          (last)
          (second))))
  ;; => 28954211
  )
