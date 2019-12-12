(ns aoc2019.day10
  (:require [clojure.string :as string]
            [clojure.set :as set])
  (:import (java.lang Math)))

(defn process-input [x]
  (->> x
       string/split-lines
       (map-indexed vector)
       (reduce (fn [acc [y line]]
                 (->> line
                      (map-indexed vector)
                      (reduce (fn [acc [x char]]
                                (cond-> acc
                                  (= char \#) (conj [x y])))
                              acc)))
               #{})))

(def input
  (->> "src/aoc2019/day10_input.txt"
       slurp
       process-input))

(def example-input1
  (process-input ".#..#
.....
#####
....#
...##"))


;;; Part 1

(defn abs [x] (cond-> x (neg? x) -))

(defn in-line-of-sight? [asteroids [ax ay :as a] [bx by :as b]]
  (if (= a b)
    false
    (let [vx (- bx ax)
          vy (- by ay)
          [n d] (cond (zero? vx) [0 1]
                      (zero? vy) [1 0]
                      :else (let [slope (/ (abs vx) (abs vy))]
                              (if (ratio? slope)
                                [(numerator slope) (denominator slope)]
                                [slope 1])))
          n (cond-> n (neg? vx) -)
          d (cond-> d (neg? vy) -)
          path (set (rest (map vector
                               (if (zero? n) (repeat ax) (range ax bx n))
                               (if (zero? d) (repeat ay) (range ay by d)))))]
      (empty? (set/intersection path asteroids)))))

(comment
  (in-line-of-sight? #{[0 0] [1 1] [2 2]} [0 0] [1 1])
  (in-line-of-sight? example-input1 [4 3] [4 0])
  )

(defn asteroids-in-line-of-sight [asteroids posn]
  (filter (partial in-line-of-sight? asteroids posn) asteroids))

(comment
  (asteroids-in-line-of-sight #{[0 0] [1 1] [2 2]} [2 2])
  (asteroids-in-line-of-sight example-input1 [4 3]))

(def part1 (apply max (map (comp count (partial asteroids-in-line-of-sight input)) input)))
;; => 214


;;; Part 2

(def station
  (apply max-key (comp count (partial asteroids-in-line-of-sight input)) input))

(defn percent-rotation [[ax ay] [bx by]]
  (/ (mod (- (+ (Math/atan2 (- ay by) (- bx ax)) (* 3/2 Math/PI))) (* 2 Math/PI)) (* 2 Math/PI)))

(comment
  (sort-by (partial percent-rotation [3 4]) (asteroids-in-line-of-sight example-input1 [3 4]))
  )

(defn vaporization-order [asteroids posn]
  (when (not= asteroids #{posn})
    (let [asteroids-in-line-of-sight (asteroids-in-line-of-sight asteroids posn)]
      (lazy-cat (sort-by (partial percent-rotation posn) asteroids-in-line-of-sight)
                (vaporization-order (set/difference asteroids asteroids-in-line-of-sight) posn)))))

(comment
  (vaporization-order example-input1 [3 4])
  )

(def part2 (let [[x y] (nth (vaporization-order input station) (dec 200))] (+ (* x 100) y)))
;; => 502
