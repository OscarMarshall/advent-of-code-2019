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

(def primes
  (letfn [(find-primes [[prime & more]]
            (lazy-seq (cons prime (find-primes (remove #(zero? (rem % prime)) more)))))]
    (find-primes (nnext (range)))))

(defn factor [x]
  (loop [x x, factors {}, primes primes]
    (if (= x 1)
      factors
      (let [[factor :as primes] (drop-while #(pos? (mod x %)) primes)]
        (recur (quot x factor) (update factors factor (fnil inc 0)) primes)))))

(comment
  (factor 1)
  (factor 2)
  (factor 4)
  (factor 6)
  )

(defn least-common-multiple [& xs]
  (let [factors (map factor xs)
        largest-factor (apply max (mapcat keys factors))]
    (apply * (mapcat (fn [prime] (repeat (apply max (map #(get % prime 0) factors)) prime))
                     (take-while #(<= % largest-factor) primes)))))

(comment
  (least-common-multiple 2 5 3)
  )

(defn cycles [posns]
  (let [dimensions (map (fn [i] (map #(nth % i) posns)) (range 3))]
    (map (fn [xs]
           (let [starting-system (map vector (map list xs) (repeat '(0)))]
             (inc (count (take-while #(not= % starting-system) (rest (iterate step-system starting-system)))))))
         dimensions)))

(comment
  (cycles [[-1 0 2]
           [2 -10 -7]
           [4 -8 8]
           [3 5 -1]])
  (cycles [[-8 -10 0]
           [5 5 10]
           [2 -7 3]
           [9 -8 -3]])
  (cycles input))

(def part2
  #_(reduce (fn [acc system]
            (let [posns (map first system)]
              (if (contains? acc posns)
                (reduced (count acc))
                (conj acc posns))))
          #{}
          (iterate step-system system)))
