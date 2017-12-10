(ns advent-of-code.2017.9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def input (str/trim (slurp (io/resource "2017/9/input.txt"))))

(defn read-stream
  "Read stream into groups represented by nested vectors
  Storing group score and garbage-count in metadata on the vector

  e.g.

  {} => []
  {{}} => []
  {{},{}} => [[] []]"
  [stream]
  (:res (reduce (fn [acc char]
                  (let [{:keys [res path garbage? ignore?]} acc]
                    (cond
                      ignore?
                      (assoc acc :ignore? false)

                      (= char \!)
                      (assoc acc :ignore? true)

                      (= char \>)
                      (assoc acc :garbage? false)

                      garbage?
                      (update acc :garbage-count inc)

                      (= char \<)
                      (assoc acc :garbage? true)

                      (= char \{)
                      (assoc acc
                             :res
                             (let [new-group (with-meta [] {:score (inc (count path))})]
                               (if res
                                 (update-in res path (fn [x] (if x (conj x new-group) new-group)))
                                 new-group))
                             :path (conj path 0))

                      (= char \})
                      (let [group-path (pop path)
                            update-meta (fn [x]
                                          (with-meta x
                                            (merge (meta x)
                                                   {:garbage-count
                                                    (:garbage-count acc)})))]
                        (assoc acc
                               :garbage-count 0
                               :res
                               (if (empty? group-path)
                                 (update-meta res)
                                 (update-in res group-path update-meta))
                               :path (pop path)))

                      (= char \,)
                      (assoc acc
                             :path
                             (if (get-in res path)
                               (update path (dec (count path)) inc)
                               path))

                      :else acc)))
                {:path []
                 :chars []
                 :garbage-count 0}
                stream)))

(defn group-count
  "Walk the groups and extract metadata counts"
  [groups]
  (let [counts (atom {:groups 0
                      :score 0
                      :garbage-count 0})]
    (walk/postwalk
     (fn [x]
       (when (vector? x)
         (swap! counts update :groups inc)
         (swap! counts update :score
                + (get (meta x) :score 0))
         (swap! counts update :garbage-count
                + (get (meta x) :garbage-count 0))
         )
       x)
     groups)
    @counts))

(defn test-read-stream-score []
  (let [examples
        [["{}" 1]
         ["{{{}}}" 6]
         ["{{},{}}" 5]
         ["{{{},{},{{}}}}" 16]
         ["{<a>,<a>,<a>,<a>}" 1]
         ["{{<ab>},{<ab>},{<ab>},{<ab>}}" 9]
         ["{{<!!>},{<!!>},{<!!>},{<!!>}}" 9]
         ["{{<a!>},{<a!>},{<a!>},{<ab>}}" 3]]]
    (doseq [[stream score] examples]
      (assert (:score (group-count (read-stream stream)))
              score))))

(defn test-read-stream-groups []
  (let [examples
        [["{}" 1]
         ["{{{}}}" 3]
         ["{{},{}}" 3]
         ["{{{},{},{{}}}}" 6]
         ["{<{},{},{{}}>}" 1]
         ["{<a>,<a>,<a>,<a>}" 1]
         ["{{<a>},{<a>},{<a>},{<a>}}" 5]
         ["{{<!>},{<!>},{<!>},{<a>}}" 2]]]
    (doseq [[stream score] examples]
      (assert (:groups (group-count (read-stream stream)))
              score))))

(comment
  (group-count (read-stream input))
  ;; => {:groups 1605, :score 11846, :garbage-count 6285}
  )
