(ns day07
  (:require [input :refer [f->str]]
            [clojure.set :refer [difference]]))

(defn parse [it] (->> it (re-seq #"(\w+) .(\d+).( -> (.+))*\n?")))

(defn mktree [it]
  (reduce
   (fn [m [_ p w _ ch]] (assoc m p {:w (parse-long w) :ch (when ch (re-seq #"\w+" ch))}))
   {} it))

(defn root-of [t]
  (let [c (select-keys t (for [[k v] t :when (:ch v)] k))
        v (->> c vals (map :ch) flatten set)
        k (->> c keys set)]
    (first (difference k v))))

(defn weights [r t]
  (let [ch (get-in t [r :ch])]
    (when ch
      (for [c ch :let [r (weights c t), v (get-in t [c :w])]]
        (if r [v r] v)))))

(defn imbalanced [s]
  (->> s frequencies (keep #(when (= 1 (second %)) (first %))) first (.indexOf s)))

(defn balance [r t]
  (let [sum #(if (seqable? %) (->> % flatten (apply +)) %)]
    (loop [w (weights r t), d 0]
      (cond (number? w) (- w d)
            (= 2 (count w))    ; assumming: [parent [children]]
            (if (= 1 (->> (second w) (map sum) set count))
              (recur (first w) d)
              (recur (second w) 0))
            :else              ; assuming children
            (let [sums (map sum w), imb-idx (imbalanced sums), imb-val (nth sums imb-idx)
                  dif (->> sums distinct (remove #(= % imb-val)) first (- imb-val))]
              (recur (nth w imb-idx) (+ d dif)))))))

(defn -main [day]
  (let [tree (->> day f->str parse mktree)
        root (root-of tree)]
    {:part1 root :part2 (balance root tree)}))


(comment
  (let [test-input "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"
        tree (->> test-input parse mktree)
        root (root-of tree)]
    {:1 (= "tknk" root)
     :2 (= 60 (balance root tree))})
  )
