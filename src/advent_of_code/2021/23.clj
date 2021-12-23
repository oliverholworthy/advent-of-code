(ns advent-of-code.2021.23
  "Day 23: Amphipod"
  (:require [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

(def rooms {:A :amber :B :bronze :C :copper :D :desert})
(def hallway-rooms {:A 2 :B 4 :C 6 :D 8})
(def move-cost {:A 1 :B 10 :C 100 :D 1000})

(defn end-state? [state]
  (and (every? #{:empty} (:hallway state))
       (reduce (fn [acc [amphipod room]] (every? #{amphipod} (get state room))) false rooms)))

(defn get-hallway-path [target curr]
  (if (> target curr)
    (range (inc curr) (inc target))
    (range target curr)))

(defn hallway-to-room [state room-size]
  (let [hallway-amphipods
        (filter (fn [[i v]] (contains? #{:A :B :C :D} v))
                (map-indexed vector (:hallway state)))]
    (reduce (fn [next-states [i amphipod]]
              (let [target-room (get rooms amphipod)
                    room (get state target-room)
                    room-i (get hallway-rooms amphipod)
                    hallway-path (map #(get (:hallway state) %)
                                      (get-hallway-path room-i i))
                    moves (+ (count hallway-path) (- room-size (count room)))
                    cost (* moves (get move-cost amphipod))]
                (if (and (every? #{amphipod} room)
                         (every? #{:empty} hallway-path))
                  (let [new-state
                        (-> state
                            (assoc target-room (conj room amphipod))
                            (assoc-in [:hallway i] :empty))]
                    (conj next-states
                          {:state new-state
                           :cost cost}))
                  next-states)))
            []
            hallway-amphipods)))

(defn room-to-room [state room-size]
  (reduce (fn [next-states [room-amphipod room-name]]
            (let [amphipod (peek (get state room-name))
                  target-room-name (get rooms amphipod)]
              (if (and amphipod (not= target-room-name room-name))
                (let [target-room (get state target-room-name)
                      current-room (get state room-name)
                      current-i (get hallway-rooms room-amphipod)
                      target-i (get hallway-rooms amphipod)
                      hallway-path (map #(get (:hallway state) %)
                                        (get-hallway-path target-i current-i))
                      moves (+ (count hallway-path)
                               (- room-size (count target-room))
                               (- room-size (count (pop current-room))))
                      cost (* moves (get move-cost amphipod))]
                  (if (and (every? #{amphipod} target-room)
                           (every? #{:empty} hallway-path))
                    (let [new-state
                          (-> state
                              (update room-name pop)
                              (assoc target-room-name (conj target-room amphipod)))]
                      (conj next-states
                            {:state new-state
                             :cost cost}))
                    next-states))
                next-states)))
          []
          rooms))

(defn room-to-hallway [state room-size]
  (reduce (fn [next-states [room-amphipod room-name]]
            (let [amphipod (peek (get state room-name))
                  current-i (get hallway-rooms room-amphipod)
                  target-room-name (get rooms amphipod)
                  current-room (get state room-name)]
              (if (and amphipod
                       (not (and (= target-room-name room-name)
                                 (every? #{amphipod} current-room))))
                (reduce (fn [next-states target-i]
                          (let [hallway-path (map #(get (:hallway state) %)
                                                  (get-hallway-path target-i current-i))
                                moves (+ (count hallway-path)
                                         (- room-size (count (pop current-room))))
                                cost (* moves (get move-cost amphipod))]
                            (if (every? #{:empty} hallway-path)
                              (conj next-states
                                    {:state
                                     (-> state
                                         (update room-name pop)
                                         (assoc-in [:hallway target-i] amphipod))
                                     :cost cost})
                              next-states)))
                        next-states
                        (disj
                         (set/difference
                          (set (range 11))
                          (set (vals hallway-rooms)))
                         current-i))
                next-states)))
          []
          rooms))

(defn get-next-states [state room-size]
  (concat (room-to-hallway state room-size)
          (hallway-to-room state room-size)
          (room-to-room state room-size)))

(defn solve-min-energy [start]
  (let [ks [:hallway :amber :bronze :copper :desert]
        room-size (count (get start :amber))
        to-state (fn [v] (into {} (map vector ks v)))]
    (first
     ((fn explore [explored frontier]
        (lazy-seq
         (when-let [[v total-cost] (peek frontier)]
           (cond->>
               (explore (conj explored v)
                        (merge-with
                         min
                         (pop frontier)
                         (into {}
                               (for [{:keys [state cost]}
                                     (remove (comp explored :state)
                                             (get-next-states (to-state v) room-size))]
                                 {(mapv state ks) (+ total-cost cost)}))))
               (end-state? (to-state v))
               (cons total-cost)))))
      #{}
      (priority-map (mapv start ks) 0)))))

(comment
  ;; Part One
  (solve-min-energy
   {:hallway [:empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
    :amber [:A :B]
    :bronze [:D :C]
    :copper [:C :B]
    :desert [:A :D]})
  (solve-min-energy
   {:hallway [:empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
    :amber [:C :D]
    :bronze [:C :A]
    :copper [:B :A]
    :desert [:B :D]})
  ;; Part Two
  (solve-min-energy
   {:hallway [:empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
    :amber [:A :D :D :B]
    :bronze [:D :B :C :C]
    :copper [:C :A :B :B]
    :desert [:A :C :A :D]})
  (solve-min-energy
   {:hallway [:empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
    :amber [:C :D :D :D]
    :bronze [:C :B :C :A]
    :copper [:B :A :B :A]
    :desert [:B :C :A :D]})
  )
