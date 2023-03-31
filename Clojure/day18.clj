(ns day18
  (:require [input :refer [f->lines]]))

(defn parse [it] (mapv #(re-seq #"-?\w+" %) it))

(defn reg-or-num [rx y]
  (if-let [c (and y (re-matches #"-?\d+" y))] (parse-long c) (rx y)))

(def ops
  {"snd" (fn [rx x & _] (assoc rx :freq (rx x)))
   "rcv" (fn [rx x & _] (cond-> rx (not= 0 (rx x)) (assoc :last (rx :freq))))
   "jgz" (fn [rx x y] (if (not= 0 (rx x)) (update rx :i + y) (update rx :i inc)))
   "set" (fn [rx x y] (assoc rx x y))
   "add" (fn [rx x y] (update rx x + y))
   "mul" (fn [rx x y] (update rx x * y))
   "mod" (fn [rx x y] (update rx x rem y))})

(defn part1 [it]
  (loop [rx (conj (zipmap (map second it) (repeat 0)) {:i 0})]
    (if-let [l (rx :last)] l
            (let [[o x y] (it (rx :i)), y (reg-or-num rx y)]
              (recur (cond-> ((ops o) rx x y) (not= o "jgz") (update :i inc)))))))

(defn -main [day]
  (let [input (->> day f->lines parse)]
    {:part1 (part1 input) :part2 nil}))
