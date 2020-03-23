(ns aoc2019.day18
  (:require [clojure.string :as string]
            [aoc2019.day15 :as day15]
            [clojure.set :as set])
  (:import [clojure.lang PersistentQueue]))

(defn process-input [input]
  (-> input
      string/split-lines
      (->> (mapv vec))))

(def input
  (-> "src/aoc2019/day18_input.txt"
      slurp
      process-input))

(def example1
  (-> "#########
#b.A.@.a#
#########"
      process-input))

(defn adjacent-posns [[x y]] [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn dead-end? [maze [x y]]
  (and (= (get-in maze [y x]) \.)
       (= (count (filter #{\#} (map (comp (partial get-in maze) reverse) (adjacent-posns [x y]))))
          3)))

(defn simplify-maze [maze]
  (let [height (count maze)
        width (count (first maze))
        dead-ends (into #{}
                        (for [y (range height)
                              x (range width)
                              :when (dead-end? maze [x y])]
                          [x y]))]
    (loop [dead-ends dead-ends, maze maze]
      (if-let [[x y :as dead-end] (first dead-ends)]
        (let [maze (assoc-in maze [y x] \#)]
          (recur (into (disj dead-ends dead-end) (filter (partial dead-end? maze)) (adjacent-posns dead-end)) maze))
        maze))))

(defn find-char [maze c]
  (let [height (count maze)
        width  (count (first maze))]
    (->> (for [y (range height), x (range width)] [x y])
         (some (fn [[x y :as posn]] (when (= (get-in maze [y x]) c) posn))))))

(def lower-case-chars (map char (range (long \a) (inc (long \z)))))

(def upper-case-chars (map char (range (long \A) (inc (long \Z)))))

(def door-chars (into #{\#} upper-case-chars))

(def key-chars (set lower-case-chars))

(def door->key (zipmap upper-case-chars lower-case-chars))

(defn maze-keys [maze]
  (into #{} (filter key-chars) (apply str maze)))

(defn abs [x] (cond-> x (neg? x) -))

(defn manhattan-distance [[x1 y1 :as _from-posn] [x2 y2 :as _to-posn]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn dijkstra-seq [{:keys [neighbors priority-bias], :or {priority-bias (constantly 0)}} x & more]
  (letfn [(continue [m seen]
            (lazy-seq (when (seq m)
                        (let [[i xs] (first m)]
                          (if (empty? xs)
                            (continue (dissoc m i) seen)
                            (let [[steps x & more :as elem] (peek xs)]
                              (when (zero? (rand-int 100)) (prn steps))
                              (cons elem
                                    (let [seen (conj seen x)
                                          m (update m i pop)]
                                      (continue (transduce (remove (comp seen first))
                                                           (completing (fn [m xyz]
                                                                         (let [steps (+ steps (:steps (meta xyz) 1))]
                                                                           (update m
                                                                                   (+ steps (apply priority-bias xyz))
                                                                                   (fnil conj [])
                                                                                   (into [steps] xyz)))))
                                                           m
                                                           (apply neighbors x more))
                                                seen)))))))))]
    (continue (sorted-map (apply priority-bias x more) [(into [0 x] more)]) #{x})))

(defn shortest-distance [maze from-posn to-posn]
  (some (fn [[steps posn keys]] (when (= posn to-posn) [steps keys]))
        (dijkstra-seq {:neighbors (fn [posn ks]
                                    (eduction (keep (fn [posn]
                                                      (let [c (get-in maze (reverse posn))]
                                                        (when (not= c \#)
                                                          [posn (cond-> ks (door-chars c) (conj (door->key c)))]))))
                                              (adjacent-posns posn)))
                       :priority-bias (fn [posn _] (manhattan-distance posn to-posn))}
                      from-posn
                      #{})))

(comment
  (shortest-distance input (find-char input \@) (find-char input \s))
  )

(defn smallest-steps [maze]
  (let [all-ks (maze-keys maze)
        k->posn (into {} (map (juxt identity (partial find-char maze))) (conj all-ks \@))
        k->dependencies (into {}
                              (map (juxt identity (comp second (partial shortest-distance maze (k->posn \@)) k->posn)))
                              all-ks)]
    (some (fn f [[steps [_ ks]]] (when (= ks all-ks) steps))
          (dijkstra-seq {:neighbors (fn g [[from-k ks]]
                                      (->> k->dependencies
                                           (remove (comp ks key))
                                           (filter (comp (partial set/superset? ks) val))
                                           keys
                                           (map (fn h [to-k]
                                                  ^{:steps (first (shortest-distance maze
                                                                                     (k->posn from-k)
                                                                                     (k->posn to-k)))}
                                                  [[to-k (conj ks to-k)]]))))}
                        [\@ #{}]))))

(comment
  (smallest-steps (simplify-maze example1))
  (map (partial apply str) (simplify-maze input))
  )

(def part1
  (time (smallest-steps (simplify-maze input))))
;; => 3048
