(ns day01.solution
  (:require [input :refer [f->str]]))

(defn parse [it] (keep (comp parse-long str) it))

(defn solve [it shift]
  (let [pairs (->> it cycle (drop shift) (interleave it) (partition 2))]
    (reduce #(if (apply = %2) (+ %1 (first %2)) %1) 0 pairs)))

(defn -main [day]
  (let [input (->> day f->str parse)]
    {:part1 (solve input 1)
     :part2 (solve input (/ (count input) 2))}))


(comment
  (let [test-input {:1 [["1122" 3] ["1111" 4] ["1234" 0] ["91212129" 9]]
                    :2 [["1212" 6] ["1221" 0] ["123425" 4] ["123123" 12] ["12131415" 4]]}]
    (for [[part input] test-input]
      (every? true?
              (for [[it exp] input, :let [shift (case part :1 1, :2 (/ (count it) 2))]]
                (= exp (solve (parse it) shift))))))
  )
