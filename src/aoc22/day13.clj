(ns aoc22.day13
  (:require [aoc22.util :refer [read-lines]]))

(defn read-packets
  [filepath]
  (->> (read-lines filepath)
       (remove empty?)
       (map read-string)))

(defn compare-packet
  ([pair] (compare-packet pair []))
  ([[p1 p2] acc]
   (let [[[e1] r1] (map vec (split-at 1 p1))
         [[e2] r2] (map vec (split-at 1 p2))]
     (cond
       (and (nil? e1) (nil? e2)) (if-let [new-pair (peek acc)]
                                   (recur new-pair (pop acc))
                                   (throw (ex-info "Equal packets!" {})))
       (nil? e1) -1
       (nil? e2) 1
       (and (int? e1) (int? e2) (= e1 e2)) (recur [r1 r2] acc)
       (and (int? e1) (int? e2)) (compare e1 e2)
       (int? e1) (compare-packet [[e1] e2] (conj acc [r1 r2]))
       (int? e2) (compare-packet [e1 [e2]] (conj acc [r1 r2]))
       :else (compare-packet [e1 e2] (conj acc [r1 r2]))))))

(defn sum-ordered-pairs-index
  [packets]
  (->> packets
       (partition 2)
       (map compare-packet)
       (keep-indexed #(when (neg-int? %2) (inc %1)))
       (apply +)))

(defn decoder-key
  [packets]
  (let [dividers [[[2]] [[6]]]]
    (->> (concat packets dividers)
         (sort #(compare-packet [%1 %2]))
         (keep-indexed (fn [i packet]
                         (when (some #(= packet %) dividers) (inc i))))
         (apply *))))

(comment
  ;; What is the sum of the indices of those pairs?
  (sum-ordered-pairs-index (read-packets "tmp/inputs/day13.txt"))

  ;; What is the decoder key for the distress signal?
  (decoder-key (read-packets "tmp/inputs/day13.txt")))
