(ns aoc2019.day7
  (:require [aoc2019.intcode-computer :as intcode-computer]
            [clojure.string :as string]
            [clojure.edn :as edn])
  (:import (clojure.lang PersistentQueue)))

(def input
  (-> "src/aoc2019/day7_input.txt"
      slurp
      (string/split #",")
      (->> (mapv edn/read-string))))


;;; Part 1

(defn all-possible-inputs [inputs]
  (for [a inputs
        b inputs
        :when (not (contains? #{a} b))
        c inputs
        :when (not (contains? #{a b} c))
        d inputs
        :when (not (contains? #{a b c} d))
        e inputs
        :when (not (contains? #{a b c d} e))]
    [a b c d e]))

(def part1
  (apply max (map #(reduce (fn [amp-input phase-setting]
                             (first (:output (intcode-computer/compute input phase-setting amp-input))))
                           0
                           %)
                  (all-possible-inputs (range 5)))))
;; => 45730


;;; Part 2

(defn run-amps [phase-settings]
  (loop [[amp :as amps] (into PersistentQueue/EMPTY (map #(intcode-computer/compute input %)) phase-settings)
         amp-input 0]
    (if amp
      (let [{:keys [halted output], :as amp} (intcode-computer/compute amp amp-input)]
        (recur (-> amps pop (cond-> (not halted) (conj (dissoc amp :output)))) (first output)))
      amp-input)))

(def part2
  (apply max (map #(run-amps %) (all-possible-inputs (range 5 10)))))
;; => 5406484
