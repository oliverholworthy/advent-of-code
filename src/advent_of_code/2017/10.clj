(ns advent-of-code.2017.10)

(defn apply-length [{:keys [skip-size xs position]} length]
  (let [xs-length (take length (drop position (cycle xs)))]
    {:xs (replace (zipmap xs-length (reverse xs-length)) xs)
     :position (mod (+ position length skip-size) (count xs))
     :skip-size (inc skip-size)}))

(defn sparse-hash [s]
  (let [lengths (concat (map int s) [17 31 73 47 23])]
    (:xs (nth (iterate #(reduce apply-length % lengths)
                       {:skip-size 0 :position 0 :xs (range 256)})
              64))))

(defn dense-hash [xs] (map #(apply bit-xor %) (partition 16 xs)))
(defn hex-hash [xs] (apply str (map #(format "%02x" %) xs)))
(def knot-hash (comp hex-hash dense-hash sparse-hash))

;; -----------------------------------------------------------------------------

(comment

  ;; Part One

  (reduce apply-length
          {:skip-size 0 :position 0 :xs [0 1 2 3 4]}
          [3 4 1 5])
  ;; => {:skip-size 4, :position 4, :xs [3 4 2 1 0]}

  (let [[x1 x2]
        (:xs (reduce apply-length
                     {:skip-size 0 :position 0 :xs (range 256)}
                     [165 1 255 31 87 52 24 113 0 91 148 254 158 2 73 153]))]
    (* x1 x2))
  ;; => 4114


  ;; Part Two

  (knot-hash "165,1,255,31,87,52,24,113,0,91,148,254,158,2,73,153")
  ;; => "2f8c3d2100fdd57cec130d928b0fd2dd"
  )
