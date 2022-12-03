(ns aoc22.day3
  (:require [aoc22.util :refer [read-lines sum]]
            [clojure.set :as set]))

(defn item-priority
  [item]
  (let [n (int item)] (if (< n 91) (- n 38) (- n 96))))

(defn sum-wrong-items-priorities
  [filepath]
  (->> (read-lines filepath)
       (map #(let [compartments (map set (split-at (/ (count %) 2) %))
                   item (first (apply set/intersection compartments))]
               (item-priority item)))
       (sum)))

(defn sum-badge-items-priorities
  [filepath]
  (->> (read-lines filepath)
       (partition 3)
       (map #(let [team-bags (map set %)
                   item (first (apply set/intersection team-bags))]
               (item-priority item)))
       (sum)))

(comment
  ;; What is the sum of the priorities of those item types?
  (sum-wrong-items-priorities "inputs/day3.txt")

  ;; What is the sum of the priorities of those item types?
  (sum-badge-items-priorities "inputs/day3.txt"))
