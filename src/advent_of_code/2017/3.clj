(ns advent-of-code.2017.3)

(defn ring-sizes [] (remove even? (range)))

(defn manhattan [[x y]] (+ (Math/abs x) (Math/abs y)))

(defn grid-coords [n]
  (if (<= n 1) [0 0]
      (let [[i ring-width]
            (first (drop-while
                    (fn [[i x]] (> n (* x x)))
                    (map-indexed vector (ring-sizes))))
            spiral-area (* ring-width ring-width)]
        [i
         (- (/ (dec ring-width) 2)
            (mod (- spiral-area n) (dec ring-width)))])))

(def spiral-steps (comp manhattan grid-coords))

(defn test-spiral-steps []
  (assert (= (spiral-steps 1) 0))
  (assert (= (spiral-steps 12) 3))
  (assert (= (spiral-steps 23) 2))
  (assert (= (spiral-steps 1024) 31)))
