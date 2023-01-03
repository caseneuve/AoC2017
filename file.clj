(ns file
  (:require [clojure.string :refer [split-lines trim]]))

(defn string [dir] (->> "input.txt" (str dir "/") slurp trim))
(defn lines [dir] (split-lines (string dir)))
