(ns aoc22.day12-bfs
  (:require [aoc22.util :refer [read-lines]])
  (:import clojure.lang.PersistentQueue))

(defn read-height
  [h]
  (-> (cond (= \S h) \a (= \E h) \z :else h) int (- 97)))

(defn read-heightmap
  [filepath]
  (let [lines (read-lines filepath)]
    (reduce-kv (fn [acc x line]
                 (reduce-kv (fn [acc y h]
                              (cond-> acc
                                (= \S h) (assoc :S [x y])
                                (= \E h) (assoc :E [x y])
                                :always (assoc-in [:heights [x y]]
                                                  (read-height h))))
                            acc
                            (vec line)))
               {:heights nil
                :S nil :E nil
                :max-x (count lines)
                :max-y (count (first lines))}
               lines)))

(defn walkable-neighbors
  [htmap [x y]]
  (let [{:keys [max-x max-y heights]} htmap
        walkable-height (inc (heights [x y]))]
    (->> [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
         (filter (fn [[a b]]
                   (and (< -1 a max-x)
                        (< -1 b max-y)
                        (<= (heights [a b]) walkable-height)))))))

(defn update-steps
  [steps curr neighbors]
  (let [neighbor-cost (inc (steps curr))]
    (reduce (fn [steps xy] (assoc steps xy neighbor-cost)) steps neighbors)))

(defn bfs
  [htmap start stop-fn]
  (loop [curr start
         to-visit PersistentQueue/EMPTY
         seen #{start}
         steps {curr 0}]
    (if (or (nil? curr) (stop-fn curr))
      [curr (steps curr)]
      (let [neighbors (remove seen (walkable-neighbors htmap curr))
            to-visit (into to-visit neighbors)]
        (recur (peek to-visit) (pop to-visit)
               (into seen neighbors)
               (update-steps steps curr neighbors))))))

(defn fewest-steps-to-E
  [htmap]
  (second (bfs htmap (:S htmap) #(= % (:E htmap)))))

(defn reverse-height
  [htmap]
  (reduce-kv (fn [htmap xy h]
               (assoc-in htmap [:heights xy] (- 25 h)))
             htmap
             (:heights htmap)))

(defn fewest-steps-to-a
  [htmap]
  (let [htmap (reverse-height htmap)
        stop-fn #(= 25 (get-in htmap [:heights %]))]
    (second (bfs htmap (:E htmap) stop-fn))))

(comment
  (time (fewest-steps-to-E (read-heightmap "tmp/inputs/day12.txt")))
  (time (fewest-steps-to-a (read-heightmap "tmp/inputs/day12.txt"))))
