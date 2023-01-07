(ns day08.solution
  (:require [input :refer [f->str]]
            [clojure.string :refer [replace split]]))

(defn parse [it]
  (->> (-> it
           (replace #"inc|dec|==|!=" {"inc" "+" "dec" "-" "==" "=" "!=" "not="})
           (split #"\s"))
       (partition 7)))

(defn instructions [it]
  (let [read #(-> % read-string eval)]
    (->> it
         (reduce
          (fn [xs [k op v _ x c n]]
            (let [[v n] (map parse-long [v n])]
              (conj xs [k #(if ((read c) (%1 x) n) (update %1 %2 (read op) v) %1)])))
          []))))

(def state #(apply hash-map (interleave (->> % (map first) set (cons :max)) (repeat 0))))

(defn solve [it part]
  (let [i (instructions it), s (state i)
        r (fn [m [k f]] (let [m (f m k)] (update m :max max (m k))))]
    (->> (cond-> (reduce r s i) (= part 1) (dissoc :max))
         vals (apply max))))

(defn -main [day]
  (let [input (->> day f->str parse)]
    {:part1 (solve input 1) :part2 (solve input 2)}))


(comment
  (let [test-input "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"
        input (parse test-input)]
    {:1 (=  1 (solve input 1))
     :2 (= 10 (solve input 2))})
  )
