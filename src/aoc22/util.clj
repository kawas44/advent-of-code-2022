(ns aoc22.util
  (:require [clojure.string :as str]))

(defn read-lines
  [filepath]
  (str/split-lines (slurp filepath)))

(defn sum [xs] (apply + xs))
