(ns aoc2019.day4)

(def input (range 156218 (inc 652527)))

(defn integer->digits [x]
  (if (zero? x)
    []
    (conj (integer->digits (quot x 10)) (rem x 10))))

(comment
  (integer->digits (first input))
  )


;; Part 1

(defn two-adjacent-digits-are-the-same? [[x y :as digits]]
  (and (some? y) (or (= x y) (recur (rest digits)))))

(comment
  (two-adjacent-digits-are-the-same? [1 1])
  (two-adjacent-digits-are-the-same? [2 1 1])
  (two-adjacent-digits-are-the-same? [2 1 2])
  )

(defn digits-never-decrease? [[x y :as digits]]
  (or (nil? y) (and (<= x y) (recur (rest digits)))))

(comment
  (digits-never-decrease? [1 1])
  (digits-never-decrease? [1 0])
  )

(defn valid? [x]
  (let [d (integer->digits x)]
    (and (two-adjacent-digits-are-the-same? d)
         (digits-never-decrease? d))))

(def part1
  (->> input
       (filter valid?)
       count))
;; => 1694


;;; Part 2

(defn two-adjacent-digits-are-the-same-and-are-not-part-of-a-larger-group? [[x :as digits]]
  (and (some? x) (let [[group rest] (split-with #{x} digits)]
                   (or (= (count group) 2) (recur rest)))))

(comment
  (two-adjacent-digits-are-the-same-and-are-not-part-of-a-larger-group? [1 1])
  (two-adjacent-digits-are-the-same-and-are-not-part-of-a-larger-group? [1 1 1])
  (two-adjacent-digits-are-the-same-and-are-not-part-of-a-larger-group? [0 1 1])
  )

(defn valid2? [x]
  (let [d (integer->digits x)]
    (and (two-adjacent-digits-are-the-same-and-are-not-part-of-a-larger-group? d)
         (digits-never-decrease? d))))

(def part2
  (->> input
       (filter valid2?)
       count))
;; => 1148
