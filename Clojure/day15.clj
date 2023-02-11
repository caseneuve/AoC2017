(ns day15
  (:require [input :refer [f->nums]]))

(def factor-a 16807)
(def factor-b 48271)
(def divisor 2147483647)
(def pairs1 40000000)
(def pairs2  5000000)

(defn bits16 [n] (bit-and n 0xffff))
(defn display [n j] (when (= 0 (mod n 100000)) (printf "\r%8d %3d" n j) (flush)))

(defn solve1 [a b]
  (loop [n 0 [a b] [a b] j 0]
    (display n j)
    (if (> n pairs1) j
        (let [ab [(rem (* a factor-a) divisor) (rem (* b factor-b) divisor)]
              match? (apply = (map bits16 ab))]
          (recur (inc n) ab (if match? (inc j) j))))))

(defn generate [n f m]
  (take pairs2
        (keep #(when (= 0 (mod % m)) (bits16 %))
              (iterate #(rem (* % f) divisor) n))))

(defn solve2 [a b]
  (loop [n 0 [a & ax] (generate a factor-a 4) [b & bx] (generate b factor-b 8) j 0]
    (display n j)
    (if (nil? a) j (recur (inc n) ax bx (if (= a b) (inc j) j)))))

(defn -main [day]
  (let [[a b] (f->nums day)]
    {:part1 (solve1 a b) :part2 (solve2 a b)}))
