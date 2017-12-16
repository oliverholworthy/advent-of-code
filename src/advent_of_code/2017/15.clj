(ns advent-of-code.2017.15)

(defn next-val [factor v] (rem (* v factor) 2147483647))

(defn generator [start factor] (rest (iterate #(next-val factor %) start)))

(defn lowest-n [x n] (bit-and x (- (bit-shift-left 1 n) 1)))

(defn lowest-16 [x] (lowest-n x 16))

(defn pairs-count [n generator-a generator-b]
  (count
   (filter (fn [[a b]] (= a b))
           (take n
                 (map vector
                      (map lowest-16 generator-a)
                      (map lowest-16 generator-b))))))

(comment

  ;; Part One

  (time (def pairs-one
          (pairs-count
           40e6
           (generator 516 16807)
           (generator 190 48271))))
  ;; => 597


  ;; Part Two

  (time (def pairs-two
          (pairs-count
           5e6
           (filter #(zero? (mod % 4)) (generator 516 16807))
           (filter #(zero? (mod % 8)) (generator 190 48271)))))
  ;; => 303
  )
