(ns advent-of-code.2021.17
  "Day 17: Trick Shot")

(defn drag [x]
  (cond (> x 0) (dec x) (< x 0) (inc x) :else x))

(defn step [{:keys [position velocity]}]
  (let [[px py] position
        [vx vy] velocity]
    {:position [(+ px vx) (+ py vy)]
     :velocity [(drag vx) (dec vy)]}))

(defn trajectory [velocity]
  (iterate step {:position [0 0] :velocity velocity}))

(defn some-n [n pred coll]
  (reduce (fn [i x]
            (cond (pred x)
                  (reduced x)
                  (> i n)
                  (reduced nil)
                  :else
                  (inc i)))
          0
          coll))

(defn lands-in-target? [area xs]
  (some-n 300 #(contains? area %) xs))

(defn area-coords [[x1 x2] [y1 y2]]
  (into #{}
        (for [x (range x1 (inc x2))
              y (range y1 (inc y2))]
          [x y])))

(defn velocities-in-target [target-x-range target-y-range]
  (let [target-area (area-coords target-x-range target-y-range)
        velocities (for [x (range -50 150)
                         y (range -200 200)]
                     [x y])]
    (->> velocities
         (filter (fn [velocity]
                   (->> velocity
                        (trajectory)
                        (map :position)
                        (lands-in-target? target-area)))))))

(defn max-y-trajectory [target-x-range target-y-range]
  (->> (velocities-in-target target-x-range target-y-range)
       (sort-by second >)
       (first)
       (trajectory)
       (map (comp second :position))
       (take 300)
       (apply max)))

(comment
  (max-y-trajectory [20 30] [-10 -5])
  (max-y-trajectory [96 125] [-144 -98])
  (count (velocities-in-target [20 30] [-10 -5]))
  (count (velocities-in-target [96 125] [-144 -98]))
  )
