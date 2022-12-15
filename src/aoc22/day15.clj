(ns aoc22.day15
  (:require [aoc22.util :refer [read-lines]]
            [clojure.string :as str]))

(defn read-sensor-report
  [line]
  (map #(map parse-long (re-seq #"[-]?\d+" %)) (str/split line #":")))

(defn read-report
  [filepath]
  (map read-sensor-report (read-lines filepath)))

(defn mdistance
  [[x y] [a b]]
  (+ (abs (- a x)) (abs (- b y))))

(defn sensor-distance
  [[sensor beacon]]
  (vector sensor (mdistance sensor beacon)))

(defn covered-at
  [row [sensor distance]]
  (let [sx (nth sensor 0)
        distance-to-row (mdistance sensor [sx row])
        dist-left (- distance distance-to-row)]
    (when (nat-int? dist-left)
      (vector (- sx dist-left) (+ sx dist-left)))))

(defn reduce-segments
  [segments]
  (let [segments (sort segments)]
    (loop [s1 (first segments) segments (next segments) res nil]
      (if (nil? s1) res
          (if-let [s2 (first segments)]
            (let [[a1 b1] s1
                  [a2 b2] s2]
              (if (<= a2 (inc b1))
                (recur [a1 (max b1 b2)] (next segments) res)
                (recur s2 (next segments) (conj res s1))))
            (conj res s1))))))

(defn count-segment-positions
  [segments]
  (reduce (fn [acc segment]
            (+ acc (inc (- (nth segment 1) (nth segment 0)))))
          0
          segments))

(defn count-covered-positions-at
  [sensors+beacons row]
  (let [n-beacons-on-row (->> sensors+beacons
                              (map second)
                              (filter #(= row (nth % 1)))
                              distinct
                              count)]
    (->> (map sensor-distance sensors+beacons)
         (keep #(covered-at row %))
         reduce-segments
         count-segment-positions
         (#(- % n-beacons-on-row)))))

(defn reduce-distress-beacon
  [max-xy row segments]
  (when-let [bad (some (fn [[a b :as s]]
                         (when (or (< 0 a max-xy) (< 0 b max-xy)) s))
                       segments)]
    [(if (< 0 (nth bad 0) max-xy)
       (dec (first bad)) (inc (nth bad (dec (count bad)))))
     row]))

(defn covered-segments
  [sensors+distance max-xy row]
  (->> sensors+distance
       (keep #(covered-at row %))
       reduce-segments
       (reduce-distress-beacon max-xy row)))

(defn distress-beacon-frequency
  [sensors+beacons max-xy]
  (let [sensors+distance (map sensor-distance sensors+beacons)]
    (->> (range (inc max-xy))
         (pmap (partial covered-segments sensors+distance max-xy))
         (remove nil?)
         first
         (#(+ (* max-xy (first %)) (second %))))))

(comment
  ;; How many positions cannot contain a beacon at row 2000000?
  (count-covered-positions-at (read-report "tmp/inputs/day15.txt") 2000000)

  ;; What is its tuning frequency?
  (distress-beacon-frequency (read-report "tmp/inputs/day15.txt") 4000000))
