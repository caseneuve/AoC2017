(ns day15
  (:require [input :refer [f->nums]]))

(def DIV 2147483647)
(def FA       16807)
(def FB       48271)
(def P1    40000000)
(def P2     5000000)

(defn bits16 [n] (bit-and n 0xffff))

(defn display [n j]
  (when (= 0 (mod n 100000)) (printf "\r%8d %3d" n j) (flush)))

(defn solve1 [a b]
  (loop [n 0 [a b] [a b] j 0] ;; (display n j)
    (if (> n P1) j
        (let [ab (map #(rem % DIV) [(* a FA) (* b FB)]), match? (apply = (map bits16 ab))]
          (recur (inc n) ab (cond-> j match? inc))))))

(defn gen [n f m]
  (take P2 (keep #(when (= 0 (mod % m)) (bits16 %)) (iterate #(rem (* % f) DIV) n))))

(defn solve2 [a b]
  (loop [n 0 [a & ax] (gen a FA 4) [b & bx] (gen b FB 8) j 0] ;; (display n j)
    (if (nil? a) j (recur (inc n) ax bx (cond-> j (= a b) inc)))))

;; it will take a while...

(defn -main [day]
  (let [[a b] (f->nums day)]
    {:part1 (solve1 a b) :part2 (solve2 a b)}))


(comment
  (apply = (map bits16 [245556042 1431495498]))
  )
