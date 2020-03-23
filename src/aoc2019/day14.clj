(ns aoc2019.day14
  (:require [clojure.string :as string]
            [clojure.edn :as edn]))

(def input
  (->> "src/aoc2019/day14_input.txt"
       slurp
       string/split-lines
       (map (fn [s]
              (map (fn [s]
                     (into {}
                           (map #(let [[_ amount kind] (re-matches #"(.+) (.+)" %)] [kind (edn/read-string amount)]))
                           (string/split s #", ")))
                   (string/split s #" => "))))))

(def indexed-input (into {} (map (juxt (comp first keys second) identity)) input))

(defn scale [m multiplier]
  (into {} (map (fn [[k v]] [k (*' v multiplier)])) m))

(defn ore-needed
  ([[kind amount] product->reaction] (first (ore-needed [kind amount] product->reaction {})))
  ([[kind amount] product->reaction extras]
   (cond
     (= kind "ORE") [amount extras]
     (>= (extras kind 0) amount) [0 (update extras kind - amount)]
     :else (let [[ingredients result] (product->reaction kind)
                 amount (- amount (extras kind 0))
                 extras (dissoc extras kind)
                 lots (rationalize (Math/ceil (/ amount (result kind))))
                 [ore extras] (reduce (fn [[ore extras] need]
                                        (let [[mo-ore extras] (ore-needed need product->reaction extras)]
                                          [(+' ore mo-ore) extras]))
                                      [0 extras]
                                      (scale ingredients lots))]
             [ore (-> extras
                      (into (scale result lots))
                      (update kind - amount))]))))

(def part1
  (ore-needed ["FUEL" 1] indexed-input))
;; => 397771

(def part2
  (let [target 1000000000000]
    (loop [ratio (/ 1 part1)]
      (prn (double ratio))
      (let [fuel (long (Math/floor (* ratio target)))
            actual-ore (ore-needed ["FUEL" fuel] indexed-input)]
        (if (<= (- target actual-ore) part1)
          (->> (range fuel Long/MAX_VALUE)
               (map (juxt identity #(ore-needed ["FUEL" %] indexed-input)))
               (some #(when (> (second %) 1000000000000) (dec (first %)))))
          (recur (/ fuel actual-ore)))))))
;; => 3126714
