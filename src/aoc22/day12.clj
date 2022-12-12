(ns aoc22.day12
  (:require [aoc22.util :refer [read-lines]]
            [clojure.string :as str]))

(defn update-htmap
  [htmap [idx line]]
  (let [sidx (str/index-of line "S")
        eidx (str/index-of line "E")
        line (cond-> line
               sidx (str/replace "S" "a")
               eidx (str/replace "E" "z"))]
    (-> htmap
        (cond-> sidx (assoc :S [sidx idx]))
        (cond-> eidx (assoc :E [eidx idx]))
        (update :grid conj (mapv #(- (int %) 97) line)))))

(defn read-heightmap
  [filepath]
  (let [lines (read-lines filepath)
        max-y (count lines)
        max-x (count (first lines))]
    (reduce update-htmap
            {:S nil :E nil :grid [] :max-x max-x :max-y max-y}
            (map-indexed vector lines))))

(defn neighbor-pts
  [htmap pt]
  (let [[x y] pt
        pt-lvl (get-in htmap [:grid y x])]
    (->> (concat (map vector ((juxt dec inc) x) [y y])
                 (map (comp vec reverse vector) ((juxt dec inc) y) [x x]))
         (filter (fn [[a b]] (and (< -1 a (htmap :max-x))
                                  (< -1 b (htmap :max-y))
                                  (<= 0
                                      (get-in htmap [:grid b a])
                                      (inc pt-lvl))))))))

(defn mdistance
  [pt other-pt]
  (+ (abs (- (other-pt 0) (pt 0)))
     (abs (- (other-pt 1) (pt 1)))))

(defn heuristic-cmp
  [a b]
  (if-not (= (:hrs a) (:hrs b))
    (< (:hrs a) (:hrs b))
    (compare (:pt a) (:pt b))))

(defn update-open
  [E closed open neighbor]
  (if (contains? closed (:pt neighbor))
    open
    (let [{:keys [pt cost]} neighbor
          same-open (first (filter #(= pt (:pt %)) open))
          cost-in-open (if same-open (:cost same-open) Long/MAX_VALUE)]
      (if (and same-open (<= cost-in-open cost))
        open
        (conj open (assoc neighbor :hrs (+ cost (mdistance pt E))))))))

(defn a-star
  [{:keys [S E] :as htmap}]
  (loop [closed #{}
         open (sorted-set-by heuristic-cmp
                             {:pt S :cost 0 :hrs (mdistance S E)})]
    (if-let [{:keys [pt cost] :as best} (first open)]
      (if (= E pt)
        best
        (recur (conj closed pt)
               (->> (neighbor-pts htmap pt)
                    (map #(array-map :pt % :cost (inc cost)))
                    (reduce (partial update-open E closed) (disj open best)))))
      (throw (ex-info "Did not find a path" {})))))

(defn fewest-steps-from-S-to-E
  [htmap]
  (:cost (a-star htmap)))

(defn find-a-next-to-b
  [{:keys [grid max-x max-y] :as htmap}]
  (->> (for [x (range max-x) y (range max-y)
             :let [pt-val (get-in grid [y x])]
             :when (= 1 pt-val)] ;; look for b first!
         (neighbor-pts htmap [x y]))
       (mapcat identity)
       distinct
       (filter #(zero? (get-in grid [(nth % 1) (nth % 0)])))))

(defn fewest-steps-from-a-to-E
  [htmap]
  (->> (find-a-next-to-b htmap)
       (map #(fewest-steps-from-S-to-E (assoc htmap :S %)))
       (apply min)))

(comment
  ;; What is the fewest steps required to move from your current position to
  ;; the location that should get the best signal?
  (fewest-steps-from-S-to-E (read-heightmap "tmp/inputs/day12.txt"))

  ;; What is the fewest steps required to move starting from any square with
  ;; elevation a to the location that should get the best signal?
  (fewest-steps-from-a-to-E (read-heightmap "tmp/inputs/day12.txt")))
