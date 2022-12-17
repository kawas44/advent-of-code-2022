(ns aoc22.day16
  (:require [aoc22.util :refer [read-lines]]
            [clojure.math.combinatorics :as mc]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import clojure.lang.PersistentQueue))

(defn read-valve
  [line]
  (let [m (re-matches #"Valve ([A-Z]{2}).+rate=(\d+);.+es? ([ ,A-Z]+)" line)
        valve (keyword (nth m 1))
        rate (parse-long (nth m 2))
        valves (map keyword (str/split (nth m 3) #", "))]
    [valve rate valves]))

(defn valves-graph
  [valves-info]
  (reduce (fn [acc valve-info]
            (let [valve (nth valve-info 0)]
              (-> acc
                  (assoc-in [:rates valve] (nth valve-info 1))
                  (update :tunnels into (map #(vector (set [valve %]) 1)
                                             (nth valve-info 2))))))
          {:rates nil :tunnels {}}
          valves-info))

(defn add-new-tunnel
  [tunnels [edge cost]]
  (let [curr-cost (tunnels edge Long/MAX_VALUE)]
    (cond-> tunnels
      (< cost curr-cost) (assoc edge cost))))

(defn remove-valve
  [tunnels valve-ko]
  (let [ko-tunnels (filter #((key %) valve-ko) tunnels)
        tunnels (apply dissoc tunnels (map first ko-tunnels))
        new-tunnels (map (fn [pair]
                           (let [cost (apply + (map second pair))
                                 edge (disj (apply set/union (map first pair))
                                            valve-ko)]
                             [edge cost]))
                         (mc/combinations ko-tunnels 2))]
    (reduce add-new-tunnel tunnels new-tunnels)))

(defn reduce-graph
  [{:keys [rates tunnels] :as data}]
  (let [ko-valves (keep #(when (and (not= :AA (key %))
                                    (zero? (val %)))
                           (key %))
                        rates)]
    (-> data
        (update :rates #(apply dissoc % ko-valves))
        (assoc :tunnels (reduce remove-valve tunnels ko-valves)))))

(defn read-valves-graph
  [filepath]
  (->> (read-lines filepath)
       (map read-valve)
       valves-graph
       reduce-graph))

(defn dijk-neighbors
  [curr tunnels to-visit]
  (filter (fn [[edge _]]
            (and (contains? edge curr)
                 (set/intersection edge to-visit)))
          tunnels))

(defn dijkstra
  [valves-graph source]
  (let [valves (-> valves-graph :rates keys)
        tunnels (:tunnels valves-graph)]
    (loop [to-visit (set valves) dists {source 0}]
      (if (empty? to-visit)
        dists
        (let [[curr curr-dist] (->> dists
                                    (filter #(to-visit (key %)))
                                    (sort-by val)
                                    first)
              to-visit (disj to-visit curr)
              neighbors (dijk-neighbors curr tunnels to-visit)
              dists (reduce (fn [dists [edge cost]]
                              (let [dst (first (disj edge curr))
                                    new-dst-dist (+ cost curr-dist)
                                    curr-dst-dist (dists dst Long/MAX_VALUE)]
                                (cond-> dists
                                  (< new-dst-dist curr-dst-dist)
                                  (assoc dst new-dst-dist))))
                            dists
                            neighbors)]
          (recur to-visit dists))))))

(defn openable-within
  [all-dists curr mins opened]
  (->> (all-dists curr)
       (filter #(and (not (opened (key %)))
                     (<= (val %) (- mins 2))))))

(defn bfs-pression
  [all-dists rates mins]
  (loop [to-visit (conj PersistentQueue/EMPTY [:AA mins 0 #{:AA}])
         best-pression 0]
    (if (empty? to-visit)
      best-pression
      (let [[curr mins pression opened] (peek to-visit)
            to-visit (pop to-visit)
            openables+mins (openable-within all-dists curr mins opened)]
        (if (empty? openables+mins)
          (recur to-visit best-pression)
          (let [visits (map #(let [[v dist] %
                                   rmins (- mins (inc dist))]
                               (vector v rmins
                                       (+ pression (* rmins (rates v)))
                                       (conj opened v)))
                            openables+mins)
                best-pression (max best-pression
                                   (apply max (map #(nth % 2) visits)))]
            (recur (into to-visit visits) best-pression)))))))

(defn best-released-pression
  [valves-graph]
  (let [rates (:rates valves-graph)
        valves (keys rates)
        dijk (partial dijkstra valves-graph)
        all-dists (into {} (map #(vector % (dissoc (dijk %) %)) valves))]
    (bfs-pression all-dists rates 30)))

(defn bfs-paths
  [all-dists rates mins]
  (loop [to-visit (conj PersistentQueue/EMPTY [:AA mins 0 #{:AA}])
         paths {}]
    (if (empty? to-visit)
      paths
      (let [[curr mins pression opened] (peek to-visit)
            to-visit (pop to-visit)
            openables+mins (openable-within all-dists curr mins opened)]
        (if (empty? openables+mins)
          (recur to-visit paths)
          (let [visits (map #(let [[v dist] %
                                   rmins (- mins (inc dist))]
                               (vector v rmins
                                       (+ pression (* rmins (rates v)))
                                       (conj opened v)))
                            openables+mins)
                paths (reduce (fn [paths [path pression]]
                                (let [prev-pression (get paths path 0)]
                                  (cond-> paths
                                    (< prev-pression pression)
                                    (assoc path pression))))
                              paths
                              (map #(vector (nth % 3) (nth % 2)) visits))]
            (recur (into to-visit visits) paths)))))))

(defn best-released-pression2
  [valves-graph]
  (let [rates (:rates valves-graph)
        valves (keys rates)
        dijk (partial dijkstra valves-graph)
        all-dists (into {} (map #(vector % (dissoc (dijk %) %)) valves))
        all-paths (bfs-paths all-dists rates 26)
        paths-tandem (->> all-paths
                          (map first)
                          (#(mc/combinations % 2))
                          (filter (fn [[s1 s2]]
                                    (= #{:AA} (set/intersection s1 s2)))))]
    (->> paths-tandem
         (map (fn [[p1 p2]]
                (+ (all-paths p1) (all-paths p2))))
         (apply max))))

(comment
  ;; What is the most pressure you can release?
  (best-released-pression (read-valves-graph "tmp/inputs/day16.txt"))

  ;; With you and an elephant working together for 26 minutes, what is the
  ;; most pressure you could release?
  (best-released-pression2 (read-valves-graph "tmp/inputs/day16.txt")))
