(ns aoc22.day8
  (:require [aoc22.util :refer [read-lines]]))

(defn read-grid
  [filepath]
  (let [lines (read-lines filepath)
        w (count lines)]
    {:trees (vec (mapcat #(map (comp parse-long str) %) lines))
     :w w
     :limit (* w w)}))

(defn indices-toward
  [{:keys [w limit] :as _grid} position direction]
  (case direction
    :S (drop 1 (range position limit w))
    :N (drop 1 (range position -1 (- w)))
    :E (take-while #(pos? (mod % w)) (range (inc position) limit))
    :W (take-while #(pos? (mod (inc %) w)) (range (dec position) -1 -1))))

(defn trees-toward
  [grid position direction]
  (map (:trees grid) (indices-toward grid position direction)))

(defn visible-from?
  [grid position direction]
  (let [curr-tree ((:trees grid) position)]
    (->> (trees-toward grid position direction)
         (filter #(<= curr-tree %))
         empty?)))

(defn visible?
  [grid position]
  (boolean (some #(visible-from? grid position %) [:N :S :E :W])))

(defn count-visible-trees
  [grid]
  (count (filter #(visible? grid %) (range (:limit grid)))))

(defn viewing-distance
  [tree trees]
  (let [[smallest others] (split-with #(< % tree) trees)]
    (+ (count smallest) (if (seq others) 1 0))))

(defn scenic-score
  [grid position]
  (->> [:N :S :E :W]
       (map #(viewing-distance ((:trees grid) position)
                               (trees-toward grid position %)))
       (apply *)))

(defn max-scenic-score
  [grid]
  (apply max (map #(scenic-score grid %) (range (:limit grid)))))

(comment
  ;; how many trees are visible from outside the grid?
  (count-visible-trees (read-grid "inputs/day8.txt"))

  ;; What is the highest scenic score possible for any tree?
  (max-scenic-score (read-grid "inputs/day8.txt")))
