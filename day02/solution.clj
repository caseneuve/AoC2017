(ns day02.solution
  (:require [file :as f]
            [clojure.math.combinatorics :refer [combinations]]))

(defn parse [it]
  (->> it (re-seq #"\d+|\n") (map parse-long) (partition-by nil?) (take-nth 2)))

(defn minmax-diff [sum it]
  (->> it ((juxt #(apply min %) #(apply max %))) (apply -) abs (+ sum)))

(defn eavenly-div [sum it]
  (->> it (map #(sort > %)) (filter #(zero? (apply mod %))) first (apply /) (+ sum)))

(def solve #(reduce %2 0 %1))

(defn -main [day]
  (let [input (->> day f/string parse)]
    {:part1 (solve input minmax-diff)
     :part2 (solve (map #(combinations % 2) input) eavenly-div)}))


(comment
  (let [test-input {:1 ["5 1 9 5\n7 5 3\n2 4 6 8" 18],
                    :2 ["5 9 2 8\n9 4 7 3\n3 8 6 5" 9]}]
    (for [[p [it exp]] test-input
          :let [it (cond->> (parse it) (= p :2) (map #(combinations % 2))),
                f (p {:1 minmax-diff :2 eavenly-div})]]
      (= exp (solve it f))))
  )
