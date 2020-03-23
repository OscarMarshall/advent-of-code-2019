(ns aoc2019.day15
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [aoc2019.intcode-computer :as intcode-computer])
  (:import [clojure.lang PersistentQueue]))

(def input
  (-> "src/aoc2019/day15_input.txt"
      slurp
      (string/split #",")
      (->> (map edn/read-string))))

(defn dijkstra-seq [neighbors [label :as root]]
  (letfn [(continue [[[label node steps :as elem] :as queue] seen]
            (lazy-seq (when elem
                        (let [steps (inc steps)
                              neighbors (remove (comp seen first) (neighbors [label node]))]
                          (cons elem (continue (into (pop queue) (map #(conj % steps)) neighbors)
                                               (into seen (map first) neighbors)))))))]
    (continue (conj PersistentQueue/EMPTY (conj root 0)) #{label})))

(defn neighbors [[[x y :as _label] state]]
  (let [state (dissoc state :output)]
    (eduction (map (juxt {1 [x (inc y)]
                          2 [x (dec y)]
                          3 [(inc x) y]
                          4 [(dec x) y]}
                         (partial intcode-computer/compute state)))
              (remove (comp #{[0]} :output second))
              (range 1 5))))

(def part1
  (some (fn [[_ {:keys [output]} steps]] (when (= output [2]) steps))
        (dijkstra-seq neighbors [[0 0] (intcode-computer/compute input)])))
;; => 244

(def part2
  (->> [[0 0] (intcode-computer/compute input)]
       (dijkstra-seq neighbors)
       (some (fn [[_ {:keys [output], :as state} :as elem]] (when (= output [2]) elem)))
       pop
       (dijkstra-seq neighbors)
       last
       last))
;; => 278
