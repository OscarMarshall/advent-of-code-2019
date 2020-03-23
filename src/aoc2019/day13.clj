(ns aoc2019.day13
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [aoc2019.intcode-computer :as intcode-computer]))

(def input
  (-> "src/aoc2019/day13_input.txt"
      slurp
      (string/split #",")
      (->> (mapv edn/read-string))))

(def part1
  (let [{:keys [output]} (intcode-computer/compute input)]
    (->> (reduce (fn [acc [x y block]]
                   (assoc acc [x y] block))
                 {}
                 (partition 3 output))
         (filter (comp #{2} val))
         count)))

(defn print-screen [screen]
  (let [score (screen [-1 0])
        width (inc (apply max (map first (keys screen))))
        height (inc (apply max (map second (keys screen))))]
    (println "Score: " score)
    (doseq [y (range height)]
      (doseq [x (range width)]
        (print (case (screen [x y] 0)
                 0 \space
                 1 \#
                 2 \=
                 3 \-
                 4 \O)))
      (println))))

(defn play! []
  (loop [{:keys [halted output], :as state} (intcode-computer/compute (assoc input 0 2)), screen {}]
    (let [screen (reduce (fn [screen [x y block]]
                           (assoc screen [x y] block))
                         screen
                         (partition 3 output))]
      (if halted
        (screen [-1 0])
        (recur (intcode-computer/compute (dissoc state :output)
                                         (let [direction (- (some #(when (= (val %) 4) (first (key %))) screen)
                                                            (some #(when (= (val %) 3) (first (key %))) screen))]
                                           (cond
                                             (pos? direction) 1
                                             (neg? direction) -1
                                             :else 0)))
               screen)))))

(def part2
  (play!))
