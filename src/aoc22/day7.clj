(ns aoc22.day7
  (:require [aoc22.util :refer [read-lines sum]]
            [clojure.string :as str]))

(defn read-filetree*
  "Parse a filetree.
   filetree := {dirpath {:dirs [dir, ...] :file-size 0} ...}
   dirpath  := [dir, ...]"
  [lines]
  (loop [line (first lines) lines (next lines) filetree {:pwd []}]
    (cond
      (nil? line)
      (dissoc filetree :pwd)

      (str/starts-with? line "$ cd ")
      (let [arg (subs line 5)]
        (if (= ".." arg)
          (recur (first lines) (next lines) (update filetree :pwd pop))
          (recur (first lines) (next lines) (update filetree :pwd conj arg))))

      (str/starts-with? line "dir ")
      (let [arg (subs line 4)
            pwd (:pwd filetree)]
        (recur (first lines) (next lines)
               (-> filetree
                   (assoc (conj pwd  arg) {})
                   (update-in [pwd :dirs] (fnil conj []) arg))))

      (re-matches #"\d+ .+" line)
      (let [[fsize _fname] (str/split line #" " 2)
            fsize (parse-long fsize)]
        (recur (first lines) (next lines)
               (update-in filetree [(:pwd filetree) :file-size] (fnil + 0) fsize)))

      :else
      (recur (first lines) (next lines) filetree))))

(defn update-filetree-total-size
  [filetree]
  (let [dirs (sort-by count > (keys filetree))]
    (reduce (fn [ftree a-dir]
              (assoc-in ftree [a-dir :total-size]
                        (+ (get-in ftree [a-dir :file-size] 0)
                           (->> (get-in ftree [a-dir :dirs])
                                (map (fn [subdir]
                                       (get-in ftree [(conj a-dir subdir) :total-size] 0)))
                                (sum)))))
            filetree
            dirs)))

(defn read-filetree
  [filepath]
  (->> (read-lines filepath) read-filetree* update-filetree-total-size))

(defn sum-dirs-size
  [filetree max-dir-size]
  (->> filetree
       (keep (fn [[_dirpath {:keys [total-size]}]]
               (when (<= total-size max-dir-size) total-size)))
       sum))

(defn size-of-delete-dir
  [filetree disk-size update-size]
  (let [root-size (get-in filetree [["/"] :total-size])
        free-size (- disk-size root-size)
        need-size (- update-size free-size)]
    (->> filetree
         (keep (fn [[dirpath {:keys [total-size]}]]
                 (when (>= total-size need-size)
                   {:dirpath dirpath :total-size total-size})))
         (sort-by :total-size)
         first
         :total-size)))

(comment
  ;; What is the sum of the total sizes of those directories?
  (sum-dirs-size (read-filetree "inputs/day7.txt") 100000)

  ;; What is the total size of that smallest directory to free enough space?
  (size-of-delete-dir (read-filetree "inputs/day7.txt") 70000000 30000000))
