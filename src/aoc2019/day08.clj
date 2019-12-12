(ns aoc2019.day08
  (:require [clojure.edn :as edn]))

(def width 25)
(def height 6)

(def input
  (-> "src/aoc2019/day08_input.txt"
      slurp
      (->> (remove #{\newline})
           (map str)
           (map edn/read-string)
           (partition (* width height)))))


;;; Part 1

(def part1
  (let [layer (apply min-key #(count (filter #{0} %)) input)
        ones (count (filter #{1} layer))
        twos (count (filter #{2} layer))]
    (* ones twos)))
;; => 828


;;; Part 2

(defn top-visible [& stacked-pixels]
  (first (drop-while #{2} stacked-pixels)))

(comment
  (top-visible 0 1)
  (top-visible 1 0)
  (top-visible 2 2 2 1 0)
  )

(def part2 (partition 25 (apply map top-visible input)))
;; => ((1 1 1 1 0 1 0 0 0 0 1 1 1 0 0 0 0 1 1 0 1 1 1 1 0)
;;     (0 0 0 1 0 1 0 0 0 0 1 0 0 1 0 0 0 0 1 0 1 0 0 0 0)
;;     (0 0 1 0 0 1 0 0 0 0 1 1 1 0 0 0 0 0 1 0 1 1 1 0 0)
;;     (0 1 0 0 0 1 0 0 0 0 1 0 0 1 0 0 0 0 1 0 1 0 0 0 0)
;;     (1 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 0 1 0 1 0 0 0 0)
;;     (1 1 1 1 0 1 1 1 1 0 1 1 1 0 0 0 1 1 0 0 1 0 0 0 0))
