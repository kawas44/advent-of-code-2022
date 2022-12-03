(ns aoc22.day1
  (:require [aoc22.util :refer [read-lines sum]]))

(defn- parse-longs
  [coll]
  (map parse-long coll))

(defn- read-elves-calories
  [filepath]
  (->> (read-lines filepath)
       (partition-by empty?)
       (remove #(= "" (first %)))
       (map parse-longs)))

(defn- elves-total-calories
  [elves-calories]
  (map sum elves-calories))

(defn- find-most-calories-carried
  [elves-calories]
  (->> elves-calories
       (elves-total-calories)
       (apply max)))

(defn- find-top3-total-calories-carried
  [elves-calories]
  (->> elves-calories
       (elves-total-calories)
       (sort >)
       (take 3)
       (sum)))

(comment
  ;; How many total Calories is that Elf carrying?
  (find-most-calories-carried (read-elves-calories "inputs/day1.txt"))

  ;; How many Calories are those Elves carrying in total?
  (find-top3-total-calories-carried (read-elves-calories "inputs/day1.txt")))
