(ns aoc22.day10
  (:require [aoc22.util :refer [read-lines]]
            [clojure.string :as str]))

(def op-cycle {:noop 1 :addx 2})

(defn read-instruction
  [line]
  (if-let [sp-idx (str/index-of line " ")]
    [(keyword (subs line 0 sp-idx)) (parse-long (subs line (inc sp-idx)))]
    [(keyword line)]))

(defn read-instructions
  [filepath]
  (map read-instruction (read-lines filepath)))

(defn cycle-instructions
  [[op :as instruction]]
  (conj (vec (repeat (dec (op-cycle op)) [:noop])) instruction))

(defn per-tick-instructions
  [instructions]
  (mapcat cycle-instructions instructions))

(defn update-cpu-states
  [cpu-states [op arg]]
  (let [cpu-state (peek cpu-states)]
    (case op
      :noop (conj cpu-states cpu-state)
      :addx (conj cpu-states (update cpu-state :X + arg)))))

(defn per-tick-cpu-states
  [instructions]
  (reduce update-cpu-states [{:X 1}] instructions))

(defn sum-signal-strengths
  [instructions]
  (->> instructions
       per-tick-instructions
       per-tick-cpu-states
       (keep-indexed (fn [tick cpu-state]
                       (when (zero? (mod (- tick 19) 40))
                         (* (inc tick) (:X cpu-state)))))
       (apply +)))

(defn print-screen
  [instructions]
  (->> instructions
       per-tick-instructions
       per-tick-cpu-states
       (map-indexed (fn [i cpu-state]
                      (let [[l r :as _sprite] ((juxt dec inc) (:X cpu-state))]
                        (if (<= l (mod i 40) r) "#" " "))))
       (partition 40)
       (run! #(println (apply str %)))))

(comment
  ;; What is the sum of these six signal strengths?
  (sum-signal-strengths (read-instructions "inputs/day10.txt"))

  ;; What eight capital letters appear on your CRT?
  (print-screen (read-instructions "inputs/day10.txt")))
