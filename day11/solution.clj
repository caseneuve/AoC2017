(ns day11.solution
  (:require [input :refer [f->str]]))

;;
;;       0   1   2   3
;; 0     __      __
;;      /  \1,0 /  \ 3,0/
;; 1   /0,1 \__/2,1 \__/
;;     \    /  \    /  \
;; 2    \__/1,2 \__/ 3,2\
;;      /  \    /  \    /
;; 3   /0,3 \__/2,3 \__/
;;

(def hex {"n" [0 -2], "ne" [1 -1], "se" [1 1], "s" [0 2], "sw" [-1 1], "nw" [-1 -1]})

(defn parse [it] (map hex (re-seq #"\w+" it)))

(defn dist [[x y]] (+ (abs x) (/ (- (abs y) (abs x)) 2)))

(defn maxd [it]
  (reduce (fn [[p m] s] (let [np (mapv + p s)] [np (max m (dist np))])) [[0 0] 0] it))

(defn -main [day]
  (let [input (->> day f->str parse), [dest mx] (maxd input)]
    {:part1 (dist dest) :part2 mx}))


(comment
  (let [test-input [["ne,ne,ne" 3] ["ne,ne,sw,sw" 0] ["ne,ne,s,s" 2] ["se,sw,se,sw,sw" 3]]]
    (for [[it e] test-input] (= e (->> it parse (reduce #(mapv + %1 %2)) dist))))
  )
