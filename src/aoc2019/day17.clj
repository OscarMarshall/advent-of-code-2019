(ns aoc2019.day17
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [aoc2019.intcode-computer :as intcode-computer]))

(def input
  (-> "src/aoc2019/day17_input.txt"
      slurp
      (string/split #",")
      (->> (mapv edn/read-string))))

(def scaffold-map
  (string/split-lines (apply str (map char (:output (intcode-computer/compute input))))))

(def part1
  (let [height (count scaffold-map)
        width (count (first scaffold-map))]
    (apply + (for [y (range height), x (range width)]
               (if (and (= (get-in scaffold-map [y x]) \#)
                        (= (get-in scaffold-map [(inc y) x]) \#)
                        (= (get-in scaffold-map [(dec y) x]) \#)
                        (= (get-in scaffold-map [y (inc x)]) \#)
                        (= (get-in scaffold-map [y (dec x)]) \#))
                 (* x y)
                 0)))))
;; => 8408

["........................................#########...........#########........"
 "........................................#.......#...........#.......#........"
 "........................................#.......#...........#.......#........"
 "........................................#.......#...........#.......#........"
 "........................................#.......#...........#.......#........"
 "........................................#.......#...........#.......#........"
 "........................................#.......#...........#.......#........"
 "........................................#.......#...........#.......#........"
 "........................................#.......#.......#############........"
 "........................................#.......#.......#...#................"
 "#########...................#############.......#.....^######................"
 "#.......#...................#...................#.......#...................."
 "#.......#...................#...................###########.................."
 "#.......#...................#...........................#.#.................."
 "#.......#...................#...........................#.#.................."
 "#.......#...................#...........................#.#.................."
 "#.......#...................#...........................###########.........."
 "#.......#...................#.............................#.......#.........."
 "###########.................#.............................###########........"
 "........#.#.................#.....................................#.#........"
 "........#.#...........#######.....................................#.#........"
 "........#.#...........#...........................................#.#........"
 "........###########...#...........................................###########"
 "..........#.......#...#.............................................#.......#"
 "..........###########.#.............................................#.......#"
 "..................#.#.#.............................................#.......#"
 "..................#.#.#.............................................#.......#"
 "..................#.#.#.............................................#.......#"
 "..................###########.......................................#.......#"
 "....................#.#.............................................#.......#"
 "................#######.............................................#########"
 "................#...#........................................................"
 "........#############........................................................"
 "........#.......#............................................................"
 "........#.......#............................................................"
 "........#.......#............................................................"
 "........#.......#............................................................"
 "........#.......#............................................................"
 "........#.......#............................................................"
 "........#.......#............................................................"
 "........#########............................................................"]

"R,6,L,10,R,8,R,8"   ; A
"R,12,L,8,L,10"      ; B
"R,6,L,10,R,8,R,8"   ; A
"R,12,L,10,R,6,L,10" ; C
"R,12,L,8,L,10"      ; B
"R,12,L,10,R,6,L,10" ; C
"R,6,L,10,R,8,R,8"   ; A
"R,12,L,8,L,10"      ; B
"R,6,L,10,R,8,R,8"   ; A
"R,12,L,10,R,6,L,10" ; C

(def instructions
  (string/join ["A,B,A,C,B,C,A,B,A,C\n" "R,6,L,10,R,8,R,8\n" "R,12,L,8,L,10\n" "R,12,L,10,R,6,L,10\n" "n\n"]))

(def part2
  (-> instructions
      (->> (map long)
           (apply intcode-computer/compute (assoc input 0 2)))
      :output
      last))
;; => 1168948
