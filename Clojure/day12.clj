(ns day12
  (:require [input :refer [f->lines lines]]))

(defn parse [it]
  (let [f #(if (nil? %1) #{%2} (conj %1 %2))]
    (->> it
         (map #(re-seq #"\d+" %))
         (reduce (fn [m [p & ps]] (reduce #(-> %1 (update p f %2) (update %2 f p)) m ps)) {}))))

(defn group [it e]
  (loop [q (into clojure.lang.PersistentQueue/EMPTY (it e)) , vi #{e}]
    (if (empty? q) vi
        (let [p (peek q)]
          (if (contains? vi p)
            (recur (pop q) vi)
            (recur (into (pop q) (it p)) (conj vi p)))))))

(defn -main [day]
  (let [input (->> day f->lines parse) solve (partial group input)]
    {:part1 (count (solve "0")) :part2 (count (reduce #(conj %1 (solve %2)) #{} (keys input)))}))


(comment
  (let [test-input "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5"
        input (->> test-input lines parse), solve (partial group input)]
    {:1 (= 6 (count (solve "0")))
     :2 (= 2 (count (reduce #(conj %1 (solve %2)) #{} (keys input))))})
  )
