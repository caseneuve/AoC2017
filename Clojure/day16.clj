(ns day16
  (:require [input :refer [f->str]]
            [clojure.string :refer [replace]]))

(defn parse [it len]
  (->> it (re-seq #"((s)(\d+)|(x)(\d+)/(\d+)|(p)(\w)/(\w))")
       (map (fn [[_ _ s s1 x x1 x2 p p1 p2]]
              (cond s #(->> % cycle (drop (- len (parse-long s1))) (take len) vec)
                    x #(-> % (assoc (parse-long x1) (% (parse-long x2))) (assoc (parse-long x2) (% (parse-long x1))))
                    p #(-> (apply str %) (replace (re-pattern (str p1 "|" p2)) {p1 p2 p2 p1}) vec))))
       reverse (apply comp)))

(defn -main [day]
  (let [programs (vec "abcdefghijklmnop")
        moves (-> day f->str (parse (count programs)))
        dances (iterate moves programs)]
    {:part1 (apply str (nth dances 1))
     ;; heuristics: my bet was that programs would eventually repeat, it appeared
     ;; that the sequence repeated quite early with my input -- after 60th dance
     ;; it returned the alphabetical order
     :part2 (loop [[it & itx] dances, vi #{}, n 0]
              (if (contains? vi it) (apply str (nth dances (rem 1000000000 n)))
                  (recur itx (conj vi it) (inc n))))}))


(comment
  (let [test-input "s1,x3/4,pe/b", p (vec "abcde")]
    (= "baedc" (apply str ((parse test-input (count p)) p))))
  )
