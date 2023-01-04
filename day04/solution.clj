(ns day04.solution
  (:require [input :refer [f->lines]]
            [clojure.math.combinatorics :refer [combinations]]))

(def no-dups #(cond->> %1 (= (count %2) (count (set %2))) inc))

(defn no-anagrams [sum pwd]
  (let [c (combinations (map sort pwd) 2)]
    (cond->> sum (= c (remove #(apply = %) c)) inc)))

(defn -main [day]
  (let [input (->> day f->lines (map #(re-seq #"\w+" %)))
        solve #(reduce % 0 input)]
    {:part1 (solve no-dups), :part2 (solve no-anagrams)}))


(comment
  (let [test-input {:1 [[["aa bb cc dd ee"] 1]
                        [["aa bb cc dd aa"] 0]
                        [["aa bb cc dd aaa"] 1]]
                    :2 [[["abcde fghij"] 1]
                        [["abcde xyz ecdab"] 0]
                        [["a ab abc abd abf abj"] 1]
                        [["iiii oiii ooii oooi oooo"] 1]
                        [["oiii ioii iioi iiio"] 0]]}
        parse #(apply re-seq #"\w+" %)]
    (for [[p its] test-input]
      (every? true?
              (for [[it exp] its, :let [f (if (= p :1) no-dups no-anagrams)]]
                (= exp (f 0 (parse it)))))))
  )
