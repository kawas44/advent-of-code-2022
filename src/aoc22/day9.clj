(ns aoc22.day9
  (:require [aoc22.util :refer [read-lines]]
            [clojure.string :as str]))

(def motion->coords
  {"U" [0  1]
   "D" [0 -1]
   "R" [1  0]
   "L" [-1  0]})

(defn read-motion
  [line]
  (let [[d s] (str/split line #" ")]
    [(motion->coords d) (parse-long s)]))

(defn read-motions
  [filepath]
  (map read-motion (read-lines filepath)))

(defn move-toward
  [[tx ty] [hx hy]]
  (let [dx (- hx tx)
        dy (- hy ty)]
    [(cond-> tx (neg? dx) dec (pos? dx) inc)
     (cond-> ty (neg? dy) dec (pos? dy) inc)]))

(defn far-apart?
  [[hx hy] [tx ty]]
  (some (fn [[c1 c2]] (= 2 (abs (- c1 c2)))) [[hx tx] [hy ty]]))

(defn update-knot
  [rope knot]
  (let [prev (get-in rope [:knots (dec knot)])
        curr (get-in rope [:knots knot])]
    (cond-> rope
      (far-apart? prev curr) (assoc-in [:knots knot] (move-toward curr prev)))))

(defn move-coords
  [knot-coords direction-coords]
  (mapv + knot-coords direction-coords))

(defn update-tail-hst
  [{:keys [knots n-knot] :as rope}]
  (update rope :tail-hst conj (knots (dec n-knot))))

(defn update-knots
  [{:keys [n-knot] :as rope} mxy]
  (update-tail-hst
   (reduce update-knot
           (update-in rope [:knots 0] move-coords mxy)
           (range 1 n-knot))))

(defn update-rope
  [rope motion]
  (let [[mxy n] motion] (reduce update-knots rope (repeat n mxy))))

(defn count-rope-tail-uniq-positions
  [motions n-knot]
  (-> (reduce update-rope
              {:knots (vec (repeat n-knot [0 0]))
               :tail-hst #{[0 0]}
               :n-knot n-knot}
              motions)
      :tail-hst
      count))

(comment
  ;; How many positions does the tail of the 2 knots rope visit at least once?
  (count-rope-tail-uniq-positions (read-motions "inputs/day9.txt") 2)

  ;; How many positions does the tail of the 10 knots rope visit at least once?
  (count-rope-tail-uniq-positions (read-motions "inputs/day9.txt") 10))
