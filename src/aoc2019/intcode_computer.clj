(ns aoc2019.intcode-computer)

(defn read-address [{:keys [memory]} address]
  (memory address 0))

(defn write [state address value]
  (assoc-in state [:memory address] value))

(defn parameter-address [{:keys [instruction-pointer relative-base], :as state} n]
  (let [address (+ instruction-pointer 1 n)
        parameter (read-address state address)]
    (case (rem (quot (read-address state instruction-pointer) (apply * 100 (repeat n 10))) 10)
      ;; Position
      0 parameter
      ;; Immediate
      1 address
      ;; Relative
      2 (+ relative-base parameter))))

(defn parameter [state n]
  (read-address state (parameter-address state n)))

(defn op-code [{:keys [instruction-pointer], :as state}]
  (rem (read-address state instruction-pointer) 100))

(ns-unmap *ns* 'run-instruction)

(defmulti run-instruction op-code)

(defmethod run-instruction :default [state]
  (throw (let [op-code (op-code state)]
           (ex-info (str "Unknown op-code: " op-code) {:state state, :op-code op-code}))))

;; Halt
(defmethod run-instruction 99 [state]
  (assoc state :halted true))

(defn increment-instruction-pointer [state parameters]
  (update state :instruction-pointer + 1 parameters))

(defn apply-arithmetic [state op]
  (-> state
      (write (parameter-address state 2) (op (parameter state 0) (parameter state 1)))
      (increment-instruction-pointer 3)))

;; Add
(defmethod run-instruction 1 [state]
  (apply-arithmetic state +'))

;; Multiply
(defmethod run-instruction 2 [state]
  (apply-arithmetic state *'))

;; Input
(defmethod run-instruction 3 [{:keys [input], :as state}]
  (if-some [x (first input)]
    (-> state
        (write (parameter-address state 0) x)
        (update :input rest)
        (increment-instruction-pointer 1))
    (assoc state :blocked :input)))

;; Output
(defmethod run-instruction 4 [state]
  (-> state
      (update :output (fnil conj []) (parameter state 0))
      (increment-instruction-pointer 1)))

;; Jump If True
(defmethod run-instruction 5 [state]
  (if (zero? (parameter state 0))
    (increment-instruction-pointer state 2)
    (assoc state :instruction-pointer (parameter state 1))))

;; Jump If False
(defmethod run-instruction 6 [state]
  (if (zero? (parameter state 0))
    (assoc state :instruction-pointer (parameter state 1))
    (increment-instruction-pointer state 2)))

(def bool->int {false 0, true 1})

(defn apply-comparison [state op]
  (apply-arithmetic state (comp bool->int op)))

;; Less Than
(defmethod run-instruction 7 [state]
  (apply-comparison state <))

;; Equals
(defmethod run-instruction 8 [state]
  (apply-comparison state =))

;; Adjust the Relative Base
(defmethod run-instruction 9 [state]
  (-> state
      (update :relative-base + (parameter state 0))
      (increment-instruction-pointer 1)))

(defn compute [program-or-state & input]
  (let [state (if (sequential? program-or-state)
                {:input input, :instruction-pointer 0, :memory (zipmap (range) program-or-state), :relative-base 0}
                (-> program-or-state
                    (dissoc :blocked)
                    (update :input concat input)))]
    (loop [state state]
      (let [{:keys [blocked halted], :as state} (run-instruction state)]
        (if (or blocked halted)
          state
          (recur state))))))
