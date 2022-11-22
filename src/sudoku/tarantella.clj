(ns sudoku.tarantella
  "Awesome implementation stolen from https://github.com/Engelberg/tarantella#sudoku-solver.
   See also https://www.youtube.com/watch?v=TA9DBG8x-ys&t=770&ab_channel=ClojureTV"
  (:require
    [tarantella.core :as tarantella]))

(def sudoku-constraints
  (into {} (for [i (range 9), j (range 9), n (range 1 10)]
             [[[i j] n]
              #{[i j] [:row i n] [:col j n] [:sector (quot i 3) (quot j 3) n]}])))

(defn rowi
  [i]
  (quot i 9))

(defn coli
  [i]
  (mod i 9))

(def coords (juxt rowi coli))

(defn i
  [[row col]]
  (+ (* 9 row) col))

(defn grid->clues
  [puzzle]
  (keep-indexed
    (fn [i v]
      (when-not (zero? v)
        [(coords i) v]))
    puzzle))

(defn clues->grid
  [clues]
  (reduce
    (fn [grid [coords v]]
      (assoc grid (i coords) v))
    (vec (repeat 81 0))
          clues))

(defn solve-sudoku
  [puzzle]
  (->> (tarantella/dancing-links
         sudoku-constraints
         :select-rows (grid->clues puzzle)
         :limit 1)
       first
       clues->grid))
