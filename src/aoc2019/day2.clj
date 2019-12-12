(ns aoc2019.day2
  (:require [aoc2019.intcode-computer :as intcode-computer]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def input
  (-> "src/aoc2019/day2_input.txt"
      slurp
      (string/split #",")
      (->> (mapv edn/read-string))))

(defn interpretter [program]
  (loop [memory program, instruction-pointer 0]
    (let [[op-code x y z] (subvec memory instruction-pointer)]
      (if (= op-code 99)
        memory
        (recur (assoc memory z ((case op-code 1 +, 2 *) (nth memory x) (nth memory y))) (+ instruction-pointer 4))))))

(defn run-program [program x y]
  (-> program
      (assoc 1 x 2 y)
      #_interpretter
      intcode-computer/compute
      :memory
      first))


;;; Part 1

(def part1
  (run-program input 12 2))
;; => 3654868


;;; Part 2

(defn find-args [program target]
  (first (for [x (range 100)
               y (range 100)
               :when (= (run-program program x y) target)]
           [x y])))

(def part2
  (-> (find-args input 19690720)
      (update 0 * 100)
      (->> (apply +))))
;; => 7014
