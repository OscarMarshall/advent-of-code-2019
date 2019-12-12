(ns aoc2019.day5
  (:require [aoc2019.intcode-computer :as intcode-computer]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def input
  (-> "src/aoc2019/day5_input.txt"
      slurp
      (string/split #",")
      (->> (mapv edn/read-string))))

(comment
  (:output (intcode-computer/compute input 1))
  )

(def part1
  (peek (:output (intcode-computer/compute input 1))))
;; => 7839346

(def part2
  (peek (:output (intcode-computer/compute input 5))))
