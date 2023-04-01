(ns day18
  (:require [input :refer [f->lines]]))

(defn ops [prog [op x y]]
  (let [value #(if-let [n (and % (parse-long %))] n (prog %)), y (value y)]
    (case op "jgz" (if (> (value x) 0) (update prog :pos + y) (update prog :pos inc))
          (-> (case op
                ;; part 1 only:
                "snd" (assoc prog :freq (prog x))
                "rcv" (cond-> prog (not= 0 x) (assoc :last (prog :freq)))
                ;; common:
                "set" (assoc  prog x y)
                "add" (update prog x + y)
                "mul" (update prog x * y)
                "mod" (update prog x mod y))
              (update :pos inc)))))

(defn run1 [instructions]
  (loop [prog (merge (zipmap (map second instructions) (repeat 0)) {:pos 0})]
    (or (prog :last) (recur (ops prog (instructions (prog :pos)))))))

(defn run2 [instructions]
  (let [init {:pos 0, "p" 0, :sent 0, :queue (clojure.lang.PersistentQueue/EMPTY)}
        A (merge (zipmap (map second instructions) (repeat 0)) init)
        B (merge A {"p" 1})]
    (loop [duet [A B], [this other :as order] [0 1]]
      (if (and (get-in duet [0 :waiting?]) (empty? (get-in duet [1 :queue]))
               (get-in duet [1 :waiting?]) (empty? (get-in duet [0 :queue])))
        (get-in duet [1 :sent])
        (let [current (duet this), [op x _ :as instruction] (instructions (current :pos))]
          (case op
            "snd" (recur (-> duet
                             (update-in [this :queue] conj (current x)) (update-in [this :sent] inc)
                             (update-in [this :pos] inc))
                         order)
            "rcv" (let [q (get-in duet [other :queue])]
                    (if (empty? q)
                      (recur (assoc-in duet [this :waiting?] true) (reverse order))
                      (recur (-> duet
                                 (assoc-in [this x] (peek q)) (update-in [other :queue] pop)
                                 (assoc-in [this :waiting?] false)
                                 (update-in [this :pos] inc))
                             order)))
            (recur (assoc duet this (ops current instruction)) order)))))))

(defn -main [day]
  (let [input (->> day f->lines (mapv #(re-seq #"-?\w+" %)))]
    {:part1 (run1 input) :part2 (run2 input)}))
