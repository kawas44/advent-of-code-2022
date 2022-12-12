(ns aoc22.day11
  (:require [clojure.string :as str]))

(defn read-operation
  [line]
  (let [[_ op-part] (str/split line #"old ")
        [op arg] (re-seq #"(?:[*+]|\d+)" op-part)]
    (cond
      (= "*" op) (cond arg   #(* % (parse-long arg))
                       :else #(* % %))
      (= "+" op) (cond arg   #(+ % (parse-long arg))
                       :else #(+ % %)))))

(defn update-monkey
  [monkey line]
  (cond
    (str/starts-with? line "  Starting")
    (update monkey :items into (map parse-long (re-seq #"\d+" line)))

    (str/starts-with? line "  Operation")
    (assoc monkey :inspect-fn (read-operation line))

    (str/starts-with? line "  Test")
    (let [modulo (parse-long (re-find #"\d+" line))]
      (-> monkey
          (assoc :modulo modulo)
          (assoc :test-fn (fn [x] (zero? (mod x modulo))))))

    (str/starts-with? line "    If")
    (let [[bool-str monkey-id-str] (re-seq #"(?:true|false|\d)" line)]
      (update monkey :throw-to
              assoc (read-string bool-str) (parse-long monkey-id-str)))

    :else monkey))

(defn read-monkey
  [paragraph]
  (reduce update-monkey
          {:items []}
          (str/split-lines paragraph)))

(defn read-monkeys
  [filepath]
  (zipmap (range) (map read-monkey (str/split (slurp filepath) #"\n\n"))))

(defn update-monkeys-items
  [monkey-id monkeys _]
  (let [{:keys [inspect-fn relief-fn test-fn throw-to]} (monkeys monkey-id)
        item (relief-fn (inspect-fn (get-in monkeys [monkey-id :items 0])))
        next-monkey-id (throw-to (test-fn item))]
    (-> monkeys
        (update-in [monkey-id :n-inspect] (fnil inc 0))
        (update-in [monkey-id :items] subvec 1)
        (update-in [next-monkey-id :items] conj item))))

(defn update-monkeys
  [monkeys monkey-id]
  (let [monkey (monkeys monkey-id)]
    (reduce (partial update-monkeys-items monkey-id)
            monkeys
            (range (count (:items monkey))))))

(defn run-round
  [monkeys n-round]
  (reduce update-monkeys monkeys
          (flatten (repeat n-round (range (count monkeys))))))

(defn monkey-business-level
  ([monkeys n-round]
   (let [moduli (map :modulo (vals monkeys))
         relief-fn (fn [x] (mod x (apply * moduli)))]
     (monkey-business-level monkeys n-round relief-fn)))
  ([monkeys n-round relief-fn]
   (->> (run-round (update-vals monkeys #(assoc % :relief-fn relief-fn))
                   n-round)
        vals
        (map :n-inspect)
        (sort >)
        (take 2)
        (apply *))))

(comment
  ;; What is the level of monkey business after 20 rounds of
  ;; stuff-slinging simian shenanigans?
  (monkey-business-level (read-monkeys "tmp/inputs/day11.txt")
                         20
                         (fn [x] (quot x 3)))

  ;; what is the level of monkey business after 10000 rounds?
  (monkey-business-level (read-monkeys "tmp/inputs/day11.txt") 10000))
