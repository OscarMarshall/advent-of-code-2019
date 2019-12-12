(ns aoc2019.day12
  (:require [clojure.string :as string]
            [clojure.edn :as edn]))

(def input
  (->> "src/aoc2019/day12_input.txt"
       slurp
       string/split-lines
       (map #(map edn/read-string (rest (re-matches #"<x=(.+), y=(.+), z=(.+)>" %))))))

(def system
  (map vector input (repeat '(0 0 0))))


;;; Part 1

(defn step-system [system]
  (let [posns (map first system)
        xs-ys-zs (apply map vector posns)]
    (map (fn [[posn velocity]]
           (let [velocity (map +
                               velocity
                               (map (fn [x xs]
                                      (apply + (map #(cond
                                                       (< % x) -1
                                                       (> % x) 1
                                                       :else 0)
                                                    xs)))
                                    posn
                                    xs-ys-zs))]
             [(map + posn velocity) velocity]))
         system)))

(comment
  (step-system system)
  )

(defn abs [x] (cond-> x (neg? x) -))

(defn total-enery-of-moon [[posn velocity :as _moon]]
  (* (apply + (map abs posn)) (apply + (map abs velocity))))

(def part1 (apply + (map total-enery-of-moon (nth (iterate step-system system) 1000))))
;; => 7722


;;; Part 2

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  ([a b]
   (/ (* a b) (gcd a b)))
  ([a b c & rest]
   (apply lcm (lcm a b) c rest)))

(defn step-dimension [system]
  (let [xs (map first system)]
    (map (fn [[x velocity]]
           (let [velocity (+ velocity
                             (apply + (map #(cond
                                              (< % x) -1
                                              (> % x) 1
                                              :else 0)
                                           xs)))]
             [(+ x velocity) velocity]))
         system)))

(defn cycles [posns]
  (let [dimensions (map (fn [i] (map #(nth % i) posns)) (range 3))]
    (map (fn [xs]
           (let [starting-system (map vector xs (repeat 0))]
             (some #(when (= (second %) starting-system) (first %))
                   (rest (map-indexed vector (iterate step-dimension starting-system))))))
         dimensions)))

(comment
  (apply lcm (cycles [[-1 0 2] [2 -10 -7] [4 -8 8] [3 5 -1]]))
  (apply lcm (cycles [[-8 -10 0] [5 5 10] [2 -7 3] [9 -8 -3]]))
  )

(def part2 (apply lcm (cycles input)))
;; => 292653556339368
