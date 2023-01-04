(ns day05.solution
  (:require [input :refer [f->nums nums]]))

(defn solve [it part]
  (let [len (count it)]
    (reduce
     (fn [[xs i] s]
       (let [v (xs i), ni (+ i v), f (if (and (= part 2) (>= v 3)) dec inc)]
         (if (or (< ni 0) (>= ni len)) (reduced (inc s)) [(update xs i f) ni])))
     [it 0] (range))))

(defn -main [day]
  (let [input (f->nums day)]
    {:part1 (solve input 1), :part2 (solve input 2)}))


(comment
  (let [test-input (-> "0\n3\n0\n1\n-3" nums)]
    {:1 (=  5 (solve test-input 1)), :2 (= 10 (solve test-input 2))})
  )
