(ns sudoku.logic
  "Alternate implementation with logical programming;
  mostly a ripoff of https://github.com/clojure/core.logic/wiki/Examples#sudoku."
  (:require
    [clojure.core.logic :as l]
    [clojure.core.logic.fd :as fd]))

(def box-corners
  [0 3 6
   27 30 33
   54 57 60])

(def box-offsets
  [0 1 2
   9 10 11
   18 19 20])

(defn box
  [i]
  (mapv (partial + (box-corners i)) box-offsets))

(def boxes (map box (range 9)))

(defn solve-sudoku
  [puzzle]
  (let [vars (vec (repeatedly 81 l/lvar))
        rows (partition 9 vars)
        cols (apply mapv vector rows)                       ;transposition
        boxes (mapv #(mapv vars %) boxes)
        hint-binds (keep-indexed (fn [i v]
                                   (when-not (zero? v)
                                     (l/== v (vars i))))
                                 puzzle)]
    (first
      (l/run 1 [q]
               (l/== q vars)
               (l/and* hint-binds)
               (l/everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
               (l/everyg fd/distinct rows)
               (l/everyg fd/distinct cols)
               (l/everyg fd/distinct boxes)))))
