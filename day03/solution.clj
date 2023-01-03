(ns day03.solution
  (:require [file :as f]))

(defn manhattan [[ax ay] [bx by]]
  (+ (abs (- ax bx)) (abs (- ay by))))

(defn adjacent [pos]
  (keep #(mapv + pos %) [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]))

(defn solve [it part]
  (let [steps (interleave (range) (range)), directions (cycle [[1 0] [0 -1] [-1 0] [0 1]])]
    (loop [n 2, p [0 0], s steps, d directions, S {[0 0] 1}]
      (cond
        (> n it) (case part :1 (manhattan p [0 0]), :2 n)
        (= 0 (first s)) (recur n p (rest s) (rest d) S)
        :else (let [p (mapv + (first d) p)
                    n (case part :1 (inc n), :2 (->> (adjacent p) (select-keys S) vals (apply +)))]
                (recur n p (->> (first s) dec (conj (rest s))) d (assoc S p n)))))))

(defn -main [day]
  (let [input (->> day f/string parse-long)]
    {:part1 (solve input :1), :part2 (solve input :2)}))


(comment
  (let [test-input {:1 [[1 0] [12 3] [23 2] [1024 31]]
                    :2 [[1 2] [2 4] [4 5] [5 10] [6 10] [747 806]]}]
    (for [[p its] test-input]
      (every? true? (for [[it exp] its] (= exp (solve it p))))))
  )
