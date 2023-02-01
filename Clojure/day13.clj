(ns day13
  (:require [input :refer [f->nums nums]]))

(defn parse [it]
  (->> it (partition 2) (reduce (fn [m [k v]] (assoc m k (* 2 (dec v)))) {})))

(defn scan [layers delay]
  (let [n (+ (apply max (keys layers)) delay), p2? (> delay 0)]
    (loop [i 0, c []]
      (cond
        (and p2? (seq c)) nil
        (> i n) (if p2? delay (->> c (map #(* % (inc (/ (layers %) 2)))) (apply +)))
        :else (recur (inc i) (cond-> c (= 0 (mod (+ i delay) (layers i n))) (conj i)))))))

(defn -main [day]
  (let [layers (->> day f->nums parse), firewall (partial scan layers)]
    {:part1 (firewall 0)
     :part2 (first (drop-while nil? (pmap firewall (range 1 ##Inf))))}))


(comment
  (let [input (->> "0: 3\n1: 2\n4: 4\n6: 4" nums parse)]
    {:p1 (= 24 (scan input 0))
     :p2 (= 10 (loop [d 1] (if (scan input d) d (recur (inc d)))))})
  )
