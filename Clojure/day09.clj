(ns day09
  (:require [input :refer [f->str]]))

(defn solve [it]
  (reduce
   (fn [s c]
     (cond (s 2) (assoc s 2 false)
           (s 1) (case c \! (assoc s 2 true), \> (assoc s 1 false), (update s 4 inc))
           :else (case c \< (assoc s 1 true), \{ (update s 0 inc), \} (-> s (update 3 + (s 0)) (update 0 dec)), s)))
   [0 false false 0 0]    ; [group-lvl garbage? skip? scores collected]
   it))

(defn -main [day] (zipmap [:part1 :part2] (->> (f->str day) solve (drop 3))))


(comment
  (let [test-input {1 [["{}" 1] ["{{{}}}" 6] ["{{},{}}" 5] ["{{{},{},{{}}}}" 16]
                       ["{<a>,<a>,<a>,<a>}" 1] ["{{<ab>},{<ab>},{<ab>},{<ab>}}" 9]
                       ["{{<!!>},{<!!>},{<!!>},{<!!>}}" 9] ["{{<a!>},{<a!>},{<a!>},{<ab>}}" 3]]
                    2 [["<>" 0] ["<random characters>" 17] ["<<<<>" 3] ["<{!>}>" 2]
                       ["<!!>" 0] ["<!!!>>" 0] ["<{o\"i!a,<{i<a>" 10]]}]
    (for [[p its] test-input]
      (every? true? (for [[it exp] its, :let [r (zipmap [1 2] (drop 3 (solve it)))]] (= (r p) exp)))))
  )
