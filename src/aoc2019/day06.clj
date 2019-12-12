(ns aoc2019.day06
  (:require [clojure.set :as set]
            [clojure.string :as string])
  (:import (clojure.lang PersistentQueue)))

(def input
  (->> "src/aoc2019/day06_input.txt"
       slurp
       string/split-lines
       (map #(rest (re-matches #"(.+)\)(.+)" %)))
       (map (juxt second first))
       (into {})))


;;; Part 1

(defn orbit-count-checksum [orbits object]
  (if-some [direct-orbit (orbits object)]
    (inc (orbit-count-checksum orbits direct-orbit))
    0))

(comment
  (let [orbits {:b :com
                :c :b
                :d :c
                :e :d
                :f :e
                :g :b
                :h :g
                :i :d
                :j :e
                :k :j
                :l :k}]
    (apply + (map (partial orbit-count-checksum orbits) (keys orbits))))
  )

(def part1
  (apply + (map (partial orbit-count-checksum input) (keys input))))
;; => 295834


;;; Part 2

(defn find-path [orbits start end]
  (let [edges (merge-with set/union
                          (->> orbits
                               (group-by val)
                               (into {} (map (juxt key (comp set (partial map key) val)))))
                          (into {} (map (juxt key (comp hash-set val))) orbits))]
    (loop [paths (conj PersistentQueue/EMPTY (list start))]
      (let [path (peek paths)
            paths (pop paths)]
        (cond
          (nil? path) (recur paths)
          (= (first path) end) (reverse path)
          :else (recur (apply conj paths (->> path
                                              first
                                              edges
                                              (map #(cons % path))
                                              (filter #(= (count (set %)) (count %)))))))))))


(comment
  (let [orbits {:b :com
                :c :b
                :d :c
                :e :d
                :f :e
                :g :b
                :h :g
                :i :d
                :j :e
                :k :j
                :l :k}]
    (merge-with set/union
                (->> orbits
                     (group-by val)
                     (into {} (map (juxt key (comp set (partial map key) val)))))
                (into {} (map (juxt key (comp hash-set val))) orbits)))
  )

(def part2
  (dec (count (find-path input (input "YOU") (input "SAN")))))
;; => 361
