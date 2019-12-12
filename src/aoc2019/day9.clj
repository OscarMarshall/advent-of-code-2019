(ns aoc2019.day9
  (:require [aoc2019.intcode-computer :as intcode-computer]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(def input
  (-> "src/aoc2019/day9_input.txt"
      slurp
      (string/split #",")
      (->> (map edn/read-string))))


;;; Part 1

(def example-input1
  '(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))

(comment
  (intcode-computer/compute example-input1)
  )

(def part1
  (first (:output (intcode-computer/compute input 1))))
;; => 2399197539


;;; Part 2

(def part2
  (first (:output (intcode-computer/compute input 2))))
;; => 35106
