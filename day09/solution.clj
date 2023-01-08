(ns day09.solution
  (:require [input :refer [f->str]]))

(defn solve [it]
  (loop [[c & ch] it, s [0 0 0 0]] ; [group-lvl garbage? group-scores garbage-collected]
    (cond (nil? c) [(s 2) (s 3)]
          (> (s 1) 0) (case c
                        \! (recur (rest ch) s)
                        \> (recur ch (assoc s 1 0))
                           (recur ch (update s 3 inc)))
          :else (case c
                  \< (recur ch (assoc s 1 1))
                  \{ (recur ch (update s 0 inc))
                  \} (recur ch (-> s (update 2 + (s 0)) (update 0 dec)))
                     (recur ch s)))))

(defn -main [day]
  (let [input (f->str day)]
    (zipmap [:part1 :part2] (solve input))))


(comment
  (let [test-input {1 [["{}" 1] ["{{{}}}" 6] ["{{},{}}" 5] ["{{{},{},{{}}}}" 16]
                       ["{<a>,<a>,<a>,<a>}" 1] ["{{<ab>},{<ab>},{<ab>},{<ab>}}" 9]
                       ["{{<!!>},{<!!>},{<!!>},{<!!>}}" 9] ["{{<a!>},{<a!>},{<a!>},{<ab>}}" 3]]
                    2 [["<>" 0] ["<random characters>" 17] ["<<<<>" 3] ["<{!>}>" 2]
                       ["<!!>" 0] ["<!!!>>" 0] ["<{o\"i!a,<{i<a>" 10]]}]
    (for [[p its] test-input]
      (every? true? (for [[it exp] its] (= ((solve it) (dec p)) exp)))))
  )
