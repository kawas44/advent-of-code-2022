(ns aoc22.day2
  (:require [aoc22.util :refer [read-lines sum]]
            [clojure.string :as str]))

(def text->shape
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(defn- line->round
  [line]
  (map text->shape (str/split line #"\s")))

(defn- read-rounds-v1
  [filepath]
  (map line->round (read-lines filepath)))

(def shape-score {:rock 1 :paper 2 :scissors 3})
(def win-rounds #{[:scissors :rock] [:rock :paper] [:paper :scissors]})

(defn- round-score
  [round]
  (cond
    (contains? win-rounds round) 6
    (apply = round) 3
    :else 0))

(defn- total-round-score
  [round]
  (+ (shape-score (second round))
     (round-score round)))

(defn- game-score
  [rounds]
  (sum (map total-round-score rounds)))

(comment
  ;; What would your total score be if everything goes exactly according
  ;; to your strategy guide?
  (game-score (read-rounds-v1 "inputs/day2.txt")))

(defn- shape-for-outcome
  [outcome his-shape]
  (case outcome
    "X" (some (fn [[lose-shape win-shape]]
                (when (= his-shape win-shape) lose-shape))
              win-rounds)
    "Y" his-shape
    "Z" (some (fn [[lose-shape win-shape]]
                (when (= his-shape lose-shape) win-shape))
              win-rounds)))

(defn- line->round'
  [line]
  (let [[his-play outcome] (str/split line #"\s")
        his-shape (text->shape his-play)]
    [his-shape (shape-for-outcome outcome his-shape)]))

(defn- read-rounds'
  [filepath]
  (map line->round' (read-lines filepath)))

(comment
  ;; what would your total score be if everything goes exactly according
  ;; to the true strategy guide?
  (game-score (read-rounds' "inputs/day2.txt")))
