(ns aoc2019.day16
  (:require [clojure.edn :as edn])
  (:import [clojure.lang PersistentQueue]))

(def input
  (->> "src/aoc2019/day16_input.txt"
       slurp
       (into [] (comp (map str)
                      (map edn/read-string)
                      (filter identity)))))

(defn abs [x] (cond-> x (neg? x) -))

(declare fft-digit)

(defn fft-digit*
  ([signal phase digit]
   (fft-digit signal (count signal) phase digit))
  ([signal length phase digit]
   (cond
     (or (zero? phase) (= digit (dec length))) (nth signal (rem digit (count signal)))
     (>= digit (/ length 2)) (-> (fft-digit signal length (dec phase) digit)
                                 (+ (fft-digit signal length phase (inc digit)))
                                 (rem 10))
     :else (let [digit+1 (inc digit)
                 sub-xform (map (partial fft-digit signal length (dec phase)))]
             (-> (range digit length (* digit+1 2))
                 (->> (transduce (map-indexed (fn [i d]
                                                (cond-> (transduce (comp (take digit+1) sub-xform) + (range d length))
                                                  (odd? i) -)))
                                 +))
                 abs
                 (rem 10))))))

(def fft-digit (memoize fft-digit*))

(defn digits->number [coll]
  (reduce (fn [x y] (+ (* 10 x) y)) 0 coll))

(def part1
  (time (digits->number (map (partial fft-digit input 100) (range 8)))))
;; => 49254779

(def part2
  (time (let [loc (digits->number (take 7 input))
              length (* (count input) 10000)]
          (loop [signal []
                 i (dec length)
                 i+1-phases (into [] (repeat 101 0))
                 last-eight (into PersistentQueue/EMPTY (repeat 8 0))]
            (cond
              (< i loc) (digits->number (reverse last-eight))
              (empty? signal) (recur input i i+1-phases last-eight)
              :else (let [phases (reduce (fn [phases i] (conj phases (rem (+ (peek phases) (nth i+1-phases i)) 10)))
                                         [(peek signal)]
                                         (range 1 101))]
                      (recur (pop signal) (dec i) phases (conj (pop last-eight) (peek phases)))))))))
;; => 55078585
