(ns file
  (:require [clojure.string :refer [split-lines]]))

(defn string [dir] (slurp (str dir "/" "input.txt")))
(defn lines [dir] (split-lines (string dir)))
