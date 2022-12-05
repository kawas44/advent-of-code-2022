(ns aoc22.day5
  (:require [aoc22.util :refer [read-lines]]
            [clojure.string :as str]))

(defn read-stacks
  [lines]
  (->> lines
       (take-while #(str/index-of % "["))
       (map #(map (fn [stack-txt] (re-find #"\w" (apply str stack-txt)))
                  (partition-all 4 %)))
       (apply map list)
       (mapv #(filter some? %))))

(defn read-moves
  [lines]
  (->> lines
       (filter seq)
       (map #(mapv parse-long (re-seq #"\d+" %)))))

(defn update-stacks
  [crane-op stacks [nb-elt src-stack-id dst-stack-id]]
  (let [src-stack (nth stacks (dec src-stack-id))
        [elts new-src-stack] (split-at nb-elt src-stack)]
    (-> stacks
        (assoc (dec src-stack-id) new-src-stack)
        (update (dec dst-stack-id) crane-op elts))))

(defn top-crates-after-moves
  [filepath crane-op]
  (let [[stacks-lines moves-lines] (split-with seq (read-lines filepath))
        stacks (read-stacks stacks-lines)
        moves (read-moves moves-lines)]
    (->> (reduce (partial update-stacks crane-op) stacks moves)
         (map first)
         (apply str))))

(def crane-9000-op (partial apply conj))
(def crane-9001-op #(concat %2 %1))

(comment
  ;; After the rearrangement procedure completes, what crate ends up on
  ;; top of each stack?
  (top-crates-after-moves "inputs/day5.txt" crane-9000-op)

  ;; After the rearrangement procedure completes, what crate ends up on
  ;; top of each stack?
  (top-crates-after-moves "inputs/day5.txt" crane-9001-op))
