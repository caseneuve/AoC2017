(ns day19
  (:require [input :refer [f->str]]))

(defn parse [it]
  (->> it (reduce
           (fn [[g [x y]] c]
             (case c
               \newline [g [0 (inc y)]]
               \space   [g [(inc x) y]]
               [(assoc g [x y] c) [(inc x) y]]))
           [{} [0 0]]) first))

(defn new-dir [p d g]
  (first
   (for [nd (disj #{[1 0] [-1 0] [0 1] [0 -1]} (mapv * [-1 -1] d))
         :when (g (mapv + p nd))]
     nd)))

(defn move [p d g r]
  (let [c (g p), np (mapv + p d)]
    (case c
      \+ (as-> (new-dir p d g) nd [(mapv + p nd) nd r])
      \| [np d r], \- [np d r]
      [np d (update r :part1 str c)])))

(defn solve [it]
  (loop [pos (->> it keys (sort-by second) first), dir [0 1], result {:part1 "", :part2 1}]
    (let [[pos dir result] (move pos dir it result)]
      (if (nil? (it pos)) result
          (recur pos dir (update result :part2 inc))))))

(defn -main [day] (solve (->> day f->str parse)))


(comment
  (let [test-input "     |
     |  +--+
     A  |  C
 F---|----E|--+
     |  |  |  D
     +B-+  +--+"]
    (= {:part1 "ABCDEF" :part2 38} (solve (->> test-input parse))))
  )
