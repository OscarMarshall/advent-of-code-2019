(ns aoc2019.day3
  (:require [clojure.string :as string]
            [clojure.edn :as edn]))

(def example-input
  ["R8,U5,L5,D3"
   "U7,R6,D4,L4"])

(def input (->> "src/aoc2019/day3_input.txt" slurp string/split-lines))

(defn process-input [input]
  (map (fn [x]
         (-> x
             (string/split #",")
             (->> (map (juxt (comp {"U" :up, "D" :down, "L" :left, "R" :right} #(subs % 0 1))
                             (comp edn/read-string #(subs % 1))))
                  (mapcat (fn [[direction amount]] (repeat amount direction))))))
       input))


;;; Part 1

(defn step-wire [wire [[x y] board] direction]
  (let [posn (case direction
               :up [x (inc y)]
               :right [(inc x) y]
               :down [x (dec y)]
               :left [(dec x) y])]
    [posn (update board posn (fnil conj #{}) wire)]))

(defn manhattan-distance [[x y]]
  (let [x (cond-> x (neg? x) -)
        y (cond-> y (neg? y) -)]
    (+ x y)))

(defn closest-intersection [input]
  (->> input
       process-input
       (map-indexed vector)
       (reduce (fn [board [wire path]] (second (reduce (partial step-wire wire) [[0 0] board] path))) {})
       (filter (comp #{#{0 1}} val))
       keys
       (apply min-key manhattan-distance)))

(comment
  (manhattan-distance (closest-intersection example-input))
  )

(def part1 (manhattan-distance (closest-intersection input)))
;; => 260


;;; Part 2

(defn step-wire2 [wire [board step [x y]] direction]
  (let [step (inc step)
        posn (case direction
               :up [x (inc y)]
               :right [(inc x) y]
               :down [x (dec y)]
               :left [(dec x) y])]
    [(update-in board [posn wire] #(or % step)) step posn]))

(defn total-steps [m]
  (apply + (vals m)))

(defn fewest-combined-steps [input]
  (->> input
       process-input
       (map-indexed vector)
       (reduce (fn [board [wire path]] (first (reduce (partial step-wire2 wire) [board 0 [0 0]] path))) {})
       vals
       (filter (comp #{#{0 1}} set keys))
       (map total-steps)
       (apply min)))

(comment
  (fewest-combined-steps example-input)
  )

(def part2 (fewest-combined-steps input))
;; => 15612
