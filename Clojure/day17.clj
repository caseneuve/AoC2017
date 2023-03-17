(ns day17
  (:require [input :refer [f->str]]))

(defn part1 [it]
  (loop [buffer [0], n 1, pos 0]
    (if (> n 2017) (nth buffer (inc pos))
        (let [i (inc (mod (+ pos it) n)), [b a] (split-at i buffer)]
          (recur (concat b [n] a) (inc n) i)))))

(defn part2 [it]
  (loop [after nil, n 1, pos 0]
    (if (> n 50000000) after
        (let [i (inc (mod (+ pos it) n))]
          (recur (if (= i 1) n after) (inc n) i)))))

(defn -main [day]
  (let [it (->> day f->str parse-long)]
    {:part1 (part1 it) :part2 (part2 it)}))
