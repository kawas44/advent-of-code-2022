(ns aoc22.day4
  (:require [aoc22.util :refer [read-lines]]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-assignment-pairs
  [line]
  (->> (str/split line #"[,-]")
       (map parse-long)
       (partition 2)
       (map (fn [[a b]] (set (range a (inc b)))))))

(defn count-fully-overlap-pairs
  [filepath]
  (->> (read-lines filepath)
       (keep #(let [[a1 a2 :as ap] (parse-assignment-pairs %)]
                (when (or (set/subset? a1 a2)
                          (set/superset? a1 a2))
                  ap)))
       count))

(defn count-overlap-pairs
  [filepath]
  (->> (read-lines filepath)
       (keep #(let [[a1 a2 :as ap] (parse-assignment-pairs %)]
                (when (seq (set/intersection a1 a2)) ap)))
       count))

(comment
  ;; In how many assignment pairs does one range fully contain the other?
  (count-fully-overlap-pairs "inputs/day4.txt")

  ;; In how many assignment pairs do the ranges overlap?
  (count-overlap-pairs "inputs/day4.txt"))
