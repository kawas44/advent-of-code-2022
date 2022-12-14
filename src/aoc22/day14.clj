(ns aoc22.day14
  (:require [aoc22.util :refer [read-lines]]
            [clojure.string :as str]))

(defn read-rock-lines
  [line]
  (let [coords (->> (str/split line #"( -> |,)")
                    (map parse-long)
                    (partition 2))]
    (map vector coords (next coords))))

(defn read-rocks-lines
  [lines]
  (mapcat read-rock-lines lines))

(defn rocks-coords
  [rocks-lines]
  (->> rocks-lines
       (mapcat (fn [rock-line]
                 (let [[[a b] [c d]] rock-line]
                   (if (= a c)
                     (map #(vector a %) (range (min b d) (inc (max b d))))
                     (map #(vector % b) (range (min a c) (inc (max a c))))))))
       set))

(defn cave-dimensions
  [coords]
  (let [min+max (juxt #(apply min %) #(apply max %))
        [min-x max-x] (min+max (map first coords))
        [min-y max-y] (min+max (map second coords))]
    {:min-x min-x :max-x max-x :min-y min-y :max-y max-y}))

(defn read-cave
  [filepath]
  (let [sand-source [500 0]
        rocks (->> (read-lines filepath)
                   (read-rocks-lines)
                   (rocks-coords))

        {:keys [min-x max-x min-y max-y]}
        (cave-dimensions (conj rocks sand-source))

        in? (fn [p] (and (some? p)
                         (<= min-x (nth p 0) max-x)
                         (<= min-y (nth p 1) max-y)))
        abyss (map #(vector % (inc max-y))
                   (range (dec min-x) (+ 2 max-x)))]
    {:in? in?
     :source sand-source
     :obstacles rocks
     :-blocked? (set abyss)}))

(defn blocked?
  [p cave]
  (let [{:keys [-blocked? obstacles]} cave]
    (or (obstacles p) (-blocked? p))))

(defn fall-straight
  [position cave]
  (loop [[x y :as p] position]
    (let [p' [x (inc y)]]
      (if (blocked? p' cave) p (recur p')))))

(defn sand-fall
  [{:keys [source in?] :as cave}]
  (loop [curr source diag-dirs [[-1 1] [1 1]]]
    (let [p (fall-straight curr cave)]
      (when (in? p)
        (let [[p1 p2] (map #(map + p %) diag-dirs)]
          (cond
            (not (blocked? p1 cave)) (recur p1 diag-dirs)
            (not (blocked? p2 cave)) (recur p2 diag-dirs)
            :else p))))))

(defn count-sand-at-rest
  [cave]
  (reduce (fn [cave i]
            (if-let [p (sand-fall cave)]
              (if (= p (:source cave))
                (reduced (inc i))
                (update cave :obstacles conj p))
              (reduced i)))
          cave
          (range)))

(defn read-cave-2
  [filepath]
  (let [sand-source [500 0]
        rocks (->> (read-lines filepath)
                   (read-rocks-lines)
                   (rocks-coords))

        rock-max-y (apply max (map second rocks))
        floor-y (+ 2 rock-max-y)

        in? (fn [p] (and (some? p)
                         (<= 0 (nth p 1) floor-y)))]
    {:in? in?
     :source sand-source
     :obstacles rocks
     :-blocked? (fn [p] (= (nth p 1) floor-y))}))

(comment
  ;; How many units of sand come to rest before sand starts flowing into the
  ;; abyss below?
  (count-sand-at-rest (read-cave "tmp/inputs/day14.txt"))

  ;; How many units of sand come to rest?
  (count-sand-at-rest (read-cave-2 "tmp/inputs/day14.txt")))
