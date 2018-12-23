(ns advent-of-code.2018.11
  "Day 11: Chronal Charge"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn hundred-digit [x]
  (mod (int (/ x 100)) 10))

(defn power-level [grid-serial-number [y x]]
  (let [rack-id (+ x 10)]
    (-> (* y rack-id)
        (+ grid-serial-number)
        (* rack-id)
        (hundred-digit)
        (- 5))))

(defn power-levels [grid-serial-number grid-size]
  (vec
   (for [y (range 1 (inc grid-size))]
     (vec (for [x (range 1 (inc grid-size))]
            (power-level grid-serial-number [y x]))))))



(defn subcells [g n]
  (for [y (range -1 (- (count g) n))
        x (range -1 (- (count (first g)) n))]
    (let [I {:A [y x] :B [y (+ x n)] :C [(+ y n) x] :D [(+ y n) (+ x n)]}]
      {:coords I
       :coord-str (str/join "," [(+ x 2) (+ y 2) n])
       :size n
       :power (- (+ (get-in g (:D I) 0)
                    (get-in g (:A I) 0))
                 (get-in g (:B I) 0)
                 (get-in g (:C I) 0))})))

(defn summed-area [grid]
  (reduce
   (fn [g [y x]]
     (let [i (get-in g [y x])
           i1 (get-in g [(dec y) x] 0)
           i2 (get-in g [y (dec x)] 0)
           i3 (get-in g [(dec y) (dec x)] 0)]
       (assoc-in g [y x] (- (+ i i1 i2) i3))))
   grid
   (for [y (range (count grid))
         x (range (count (first grid)))]
     [y x])))

(defn part-one [grid]
  (let [grid-size 3]
    (first (sort-by :power > (subcells (summed-area grid) grid-size)))))

(defn part-two [grid]
  (let [sg (summed-area grid)]
    (loop [grid-size 1
           max-power {}]
      (println grid-size)
      (if (> grid-size 300)
        max-power
        (let [best-coord (first (sort-by :power > (subcells sg grid-size)))]
          (recur (inc grid-size)
                 (if (> (:power best-coord) (:power max-power 0))
                   best-coord
                   max-power)))))))

(comment
  (:coord-str (part-one (power-levels 2866 300)))
  (time (def res-two (part-two (power-levels 2866 300))))
  )

