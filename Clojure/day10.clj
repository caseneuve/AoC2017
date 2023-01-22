(ns day10
  (:require [input :refer [f->str]]))

(def suffix [17 31 73 47 23])

(defn twist [it len part]
  (let [xs (take len (range))
        it (case part
             1 (->> it (re-seq #"\d+") (map parse-long))
             2 (->> (concat (map int it) suffix) (repeat 64) flatten))]
    (loop [[l & lens] it, xs xs, idx 0, [s & skips] (cycle xs)]
      (if (nil? l) xs
          (let [cycled (cycle xs)
                sub (->> cycled (drop idx) (take l) reverse)
                rst (->> cycled (drop (+ idx l)) (take (- len l)))
                xs (take len (drop (- len idx) (cycle (concat sub rst))))
                idx (mod (+ idx l s) len)]
            (recur lens, xs, idx, skips))))))

(defn dense-hash [it]
  (->> it
       (partition 16)
       (map #(apply bit-xor %))
       (map #(format "%02x" %))
       (apply str)))

(defn solve
  ([part it] (solve part it 256))
  ([part it len]
   (let [twisted (twist it len part)]
     (case part
       1 (->> twisted (take 2) (apply *))
       2 (->> twisted dense-hash)))))

(defn -main [day]
  (let [input (f->str day)]
    {:part1 (solve 1 input) :part2 (solve 2 input)}))


(comment
  (let [test-input {1 [[["3,4,1,5" 5] 12]]
                    2 [[[""] "a2582a3a0e66e6e86e3812dcb672a272"]
                       [["AoC 2017"] "33efeb34ea91902bb2f59c9920caa6cd"]
                       [["1,2,3"] "3efbe78a8d82f29979031a4aa0b16a9d"]
                       [["1,2,4"] "63960835bcdc130f0b66d7ff4f6a5a8e"]]}]
    (for [[p its] test-input]
      (every? true? (for [[args exp] its] (= exp (apply solve (cons p args)))))))
  )
