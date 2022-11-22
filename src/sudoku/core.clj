(ns sudoku.core
  "Terminology:
    - Cell: One of the 81 fields in which numbers can be filled,
      indexed from the top-left corner to the bottom-right corner.
    - Row: Horizontal line of 9 cells, indexed top-to-bottom.
    - Column: Vertical line of 9 cells, indexed left-to-right.
    - Box: 3x3 square of cells, indexed top-left corner to bottom-right corner.")

(defn take-until
  "Lazily take elements from coll until (and including) the first item that
  breaks the predicate pred.

  Example:
  (take-until odd? [2 2 1 2]
  => (2 2 1)

  Source is based on clojure.core/take-while 1.10."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (cons (first s) (if-not (pred (first s))
                        (take-until pred (rest s))
                        ())))))

(defn fixpoint
  "Return a lazy collection of xs up until the first repetition,
  or until the termination of xs.

  For example:
    (fixpoint [1 2 3 3 3]) => (1 2 3)
    (fixpoint [1 2 3 3 4]) => (1 2 3)
    (fixpoint [1 2 3]) => (1 2 3)"
  [xs]
  (->> xs
       (partition 2 1 [:padding])
       (take-until (fn [[a b]] (= a b)))
       (map first)))

(defn every-pred'
  "Like clojure.core/every-pred, but each predicate may take multiple arguments."
  [& preds]
  (fn [& args]
    (every? #(apply % args) preds)))

(def ^{:doc "Cell indices by row."} rows
  (mapv vec (partition 9 (range 81))))

(defn transpose
  "Given a matrix represented by a vector of vectors, return its transposition.

  For example:
    (transpose [[1 2]
                [3 4])
    => [[1 3]
        [2 4]]"
  [matrix]
  (apply mapv vector matrix))

(def ^{:doc "Cell indices by column."} cols
  (transpose rows))

(defn cell
  "The index of the cell in the provided row and col."
  [row col]
  (+ row (* col 9)))

(def ^{:doc "Cell indices of the respective top-left corners of the 9 Sudoku 3x3 boxes."} box-corners
  (for [i (range 3)
        j (range 3)]
    (cell (* 3 j) (* 3 i))))

(def ^{:doc "Cell indices of the top-left 3x3 box."} box-0
  [0 1 2 9 10 11 18 19 20])

(def ^{:doc "Cell indices by box."} boxes
  (vec
    (for [offset box-corners]
      (mapv (partial + offset) box-0))))

(defn x-by-cell
  "Inverts a structural mapping represented as a matrix (uniform vector of vectors), e.g. cell indices by [row-i col-i], and returns a flat vector of e.g. rows indiced by cell."
  [matrix]
  (reduce-kv
    (fn [acc i cells]
      (reduce #(assoc %1 %2 i) acc cells))
    {}
    matrix))

(def ^{:doc "Row indices by cell index."} rows-by-cell
  (x-by-cell rows))

(def ^{:doc "Row indices by cell index."} cols-by-cell
  (x-by-cell cols))

(def ^{:doc "Box indices by cell index."} boxes-by-cell
  (x-by-cell boxes))

(defn calc-neighbours
  "The set of cell indices that are in the same row, column or box as a cell,
  excluding the cell itself."
  [cell]
  (-> (concat (rows (rows-by-cell cell))
              (cols (cols-by-cell cell))
              (boxes (boxes-by-cell cell)))
      set
      (disj cell)))

(def ^{:doc "Sets of neighbours for every cell."} neighbours
  (mapv calc-neighbours (range 81)))

(defn eliminate-candidates
  "When filling cell, the sets of candidates for the row, column and box are
  updated to exclude that number, and the candidate set for the cell itself
  is emptied."
  [candidates cell n]
  (reduce
    #(update %1 %2 disj n)
    (assoc candidates cell #{})
    (neighbours cell)))

(defn fill
  "Given a game, containing a :sudoku and the sets of :candidates, fill in the
  cell at the given index with the provided number n, updating the candidates
  in the process."
  [game cell n]
  (-> game
      (update :sudoku assoc cell n)
      (update :candidates eliminate-candidates cell n)))

(defn init-candidates
  "Calculate the candidates for the provided sudoku. Candidates are the vector,
  indexed by cell, of the numbers 1-9 not contained by a neighbouring cell.
  In other words, the candidates for cell i are the numbers that could be filled
  in cell i without directly violating the row, column or box uniqueness constraint."
  [sudoku]
  (reduce-kv (fn [cands cell n]
               (cond-> cands
                       (pos-int? n) (eliminate-candidates cell n)))
             (vec (repeat 81 #{1 2 3 4 5 6 7 8 9}))
             sudoku))

(defn init
  "Initialize the gamestate for the provided sudoku. A sudoku is a vector of
  81 integers from 0-9. 0 means 'not filled'. Indexing is made as a Westerner reads."
  [sudoku]
  {:sudoku sudoku
   :candidates (init-candidates sudoku)})

(defn indices-where
  "Return the indices for which #(pred game %) is truthy."
  [pred game]
  (->> (range 81)
       (filter (partial pred game))))

(defn cands
  "Return the set of candidates the cell."
  [game cell]
  (get-in game [:candidates cell]))

(defn filled?
  "Is the cell filled?"
  [game cell]
  (pos-int? (get-in game [:sudoku cell])))

(defn unfillable?
  "Are there any candidates for the cell?"
  [game cell]
  (empty? (cands game cell)))

(defn single-candidate?
  "Does the cell have exactly one candidate?"
  [game cell]
  (= 1 (count (cands game cell))))

(defn fill-first-candidate
  "Fill the cell with an arbitrary choice among its candidates."
  [game cell]
  (fill game cell (first (cands game cell))))

(defn fill-first-candidate-of-first-single-candidate
  "Fill a cell, if any, that has exactly one candidate."
  [game]
  (let [first-single-candidate (first (indices-where single-candidate? game))]
    (cond-> game
            first-single-candidate (fill-first-candidate first-single-candidate))))

(defn guesses
  "Pick an arbitrary cell and return a lazy sequence of the games that would
  result from filling its respective candidates."
  [game]
  (let [guessing-cell (first (indices-where (complement filled?) game))]
    (->> (cands game guessing-cell)
         (map (partial fill game guessing-cell)))))

(defn solved?
  "Is every cell filled? If so, the sudoku is solved assuming no illegal filling was made."
  [game]
  (not (some #{0} (:sudoku game))))

(defn unsolvable?
  "Are there cells that are not filled, yet have no candidates? If so, the
  sudoku cannot be solved."
  [game]
  (->> game
       (indices-where (every-pred' (complement filled?) unfillable?))
       (seq)
       (some?)))

(defn fill-obvious
  "Keep filling single candidates until the sudoku is solved or there are no more
  single candidates."
  [game]
  (->> game
       (iterate fill-first-candidate-of-first-single-candidate)
       (take-until solved?)
       fixpoint
       last))

(defn solve-game
  [game]
  (let [attempt-without-guessing (fill-obvious game)]
    (cond (solved? attempt-without-guessing) attempt-without-guessing
          (unsolvable? attempt-without-guessing) nil
          :else (some solve-game (guesses game)))))

(defn solve-sudoku
  [puzzle]
  (:sudoku (solve-game (init puzzle))))
