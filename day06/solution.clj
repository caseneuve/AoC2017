(ns day06.solution
  (:require [input :refer [f->nums]]))

(defn redistribute [bx]
  (let [c (count bx)
        [i v] (reduce-kv #(if (> %3 (second %1)) [%2 %3] %1) [0 0] bx)
        f (fn [[i acc] n] [(inc i) (update acc (mod i c) + n)])]
    (second (reduce f [(mod (inc i) c) (assoc bx i 0)] (repeat v 1)))))

(defn solve [it]
  (loop [n 1, xs it, state #{it}]
    (let [new (redistribute xs)]
        (if (some #{new} state) [n new] (recur (inc n) new (conj state new))))))

(defn -main [day]
  (let [input (f->nums day), [p1 seen] (solve input), [p2 _] (solve seen)]
    {:part1 p1 :part2 p2}))


(comment
  (let [test-input [0 2 7 0], [p1 seen] (solve test-input), [p2 _] (solve seen)]
    {:1 (= 5 p1), :2 (= 4 p2)})
  )
