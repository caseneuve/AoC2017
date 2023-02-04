(ns day14
  (:require [day10 :refer [solve] :rename {solve knot-hash}]))

(defn hex->bin [h]
  (let [h->d (zipmap "0123456789abcdef" (range 16))]
    (apply str (for [d (map h->d h)] (format "%04d" (parse-long (Integer/toString d 2)))))))

(defn grid [it]
  (let [lines (pmap #(hex->bin (knot-hash 2 (str it "-" %))) (range 128))]
    (loop [[c & cx] (apply str lines), x 0, m #{}]
      (if (nil? c) m (recur cx (inc x) (case c \1 (conj m [(mod x 128) (quot x 128)]) m))))))

(defn region [grid]
  (let [adj (fn [g p] (filter #(contains? g %) (map #(mapv + p %) [[-1 0] [1 0] [0 -1] [0 1]])))
        p (first grid), q (into clojure.lang.PersistentQueue/EMPTY (adj grid p))]
    (loop [q q, g (disj grid p)]
      (if (empty? q) g (recur (into (pop q) (adj g (peek q))) (disj g (peek q)))))))

(defn -main [_]
  (let [squares (grid "nbysizxe")]
    {:part1 (count squares)
     :part2 (loop [g squares, r 0] (if (empty? g) r (recur (region g) (inc r))))}))


(comment
  (let [g (grid "flqrgnkx")]
    {:1 (= 8108 (count g))
     :2 (= 1242 (loop [g g, r 0] (if (empty? g) r (recur (region g) (inc r)))))})
  )
