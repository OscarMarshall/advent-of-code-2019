(ns aoc2019.day01
  (:require [clojure.edn :as edn]
            [clojure.string :as string]))

(def input (-> "src/aoc2019/day01_input.txt"
               slurp
               string/split-lines
               (->> (map edn/read-string))))


;;; Part 1

(def part1
  (->> input
       (map #(quot % 3))
       (map #(- % 2))
       (apply +)))
;; => 3224048


;;; Part 2


;; Long => Sequence of Longs
(defn fuels [x]
  (let [y (- (quot x 3) 2)]
    (when (pos? y)
      (cons y (fuels y)))))

(def part2
  (->> input
       (mapcat fuels)
       (apply +)))
;; => 4833211
