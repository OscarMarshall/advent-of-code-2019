(ns aoc2019.day11
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [aoc2019.intcode-computer :as intcode-computer]))

(def input
  (-> "src/aoc2019/day11_input.txt"
      slurp
      (string/split #",")
      (->> (map edn/read-string))))


;;; Part 3

(defn run-robot [brain starting-panel]
  (loop [brain brain, panels {[0, 0] starting-panel}, posn [0 0], facing :up]
    (let [{:keys [halted output], :as brain} (intcode-computer/compute brain (panels posn 0))
          [color turn] output
          panels (assoc panels posn color)
          facing (get-in {:up [:left :right], :right [:up :down], :down [:right :left], :left [:down :up]}
                         [facing turn])]
      (if halted
        panels
        (recur (dissoc brain :output)
               panels
               (case facing
                 :up (update posn 1 inc)
                 :right (update posn 0 inc)
                 :down (update posn 1 dec)
                 :left (update posn 0 dec))
               facing)))))

(def part1
  (count (run-robot input 0)))
;; => 2322


;;; Part 2

(def part2
  (let [panels (run-robot input 1)
        xs (map first (keys panels))
        min-x (apply min xs)
        max-x (apply max xs)
        ys (map second (keys panels))
        min-y (apply min ys)
        max-y (apply max ys)]
    (for [y (range max-y (dec min-y) -1)]
      (apply str (for [x (range min-x (inc max-x))]
                   (case (panels [x y] 0)
                     0 \.
                     1 \#))))))
;; => ("...##.#..#..##..###..###...##...##..#..#..."
;;     "....#.#..#.#..#.#..#.#..#.#..#.#..#.#..#..."
;;     "....#.####.#..#.#..#.###..#....#....#..#..."
;;     "....#.#..#.####.###..#..#.#.##.#....#..#..."
;;     ".#..#.#..#.#..#.#.#..#..#.#..#.#..#.#..#..."
;;     "..##..#..#.#..#.#..#.###...###..##...##....")
