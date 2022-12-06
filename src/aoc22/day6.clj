(ns aoc22.day6)

(defn start-of-marker
  [filepath marker-length]
  (->> (slurp filepath)
       (partition marker-length 1)
       (take-while #(< (count (set %)) marker-length))
       (count)
       (+ marker-length)))

(comment
  ;; How many characters need to be processed before the first
  ;; start-of-packet marker is detected?
  (start-of-marker "inputs/day6.txt" 4)

  ;; How many characters need to be processed before the first
  ;; start-of-message marker is detected?
  (start-of-marker "inputs/day6.txt" 14))
