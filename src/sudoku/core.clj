(ns sudoku.core)

(defn take-until
  "Like take-while, but also includes the first element
  that breaks the predicate.

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
    (fixpoint [1 2 3 3]) => [1 2 3]
    (fixpoint [1 2 3]) [1 2 3]"
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

(def ^{:doc "Square indices by row."} rows
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

(def ^{:doc "Square indices by column."} cols
  (transpose rows))

(defn square
  "The index of the square in the provided row and col."
  [row col]
  (+ row (* col 9)))

(def ^{:doc "Square indices of the top-left corners of the 9 Sudoku 3x3 boxes."} box-corners
  (for [i (range 3)
        j (range 3)]
    (square (* 3 j) (* 3 i))))

(def ^{:doc "Square indices of the top-left 3x3 box."} box-0
  [0 1 2 9 10 11 18 19 20])

(def ^{:doc "Square indices by box."} boxes
  (vec
    (for [offset box-corners]
      (mapv (partial + offset) box-0))))

(defn x-by-square
  "Return a map that allow to for instance look up what row square 7 is in."
  [matrix]
  (reduce-kv
    (fn [acc i squares]
      (reduce #(assoc %1 %2 i) acc squares))
    {}
    matrix))

(def ^{:doc "Row indices by square index."} rows-by-square
  (x-by-square rows))

(def ^{:doc "Row indices by square index."} cols-by-square
  (x-by-square cols))

(def ^{:doc "Box indices by square index."} boxes-by-square
  (x-by-square boxes))

(defn calc-neighbours
  "The set of square indices that are in the same row, column or box as square,
  excluding square itself."
  [square]
  (-> (concat (rows (rows-by-square square))
              (cols (cols-by-square square))
              (boxes (boxes-by-square square)))
      set
      (disj square)))

(def ^{:doc "Sets of neighbours for every square."} neighbours
  (mapv calc-neighbours (range 81)))

(defn eliminate-candidates
  "When filling square, the sets of candidates for the row, column and box are
  updated to excluding that number, and the candidate set for the square itself
  is emptied."
  [candidates square n]
  (reduce
    #(update %1 %2 disj n)
    (assoc candidates square #{})
    (neighbours square)))

(defn fill
  "Given a game, containing a :sudoku and the sets of :candidates, fill in the
  square at the given index with the provided number n, updating the candidates
  in the process."
  [game square n]
  (-> game
      (update :sudoku assoc square n)
      (update :candidates eliminate-candidates square n)))

(defn init-candidates
  "Calculate the candidates for the provided sudoku. Candidates are the vector,
  indexed by square, of the numbers 1-9 not contained by a neighbouring square.
  In other words, the candidates for square i are the numbers that could be filled
  in square i without directly violating the row, column or box uniqueness constraint."
  [sudoku]
  (reduce-kv (fn [cands square n]
               (cond-> cands
                       (pos-int? n) (eliminate-candidates square n)))
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
  "Return the set of candidates the square."
  [game square]
  (get-in game [:candidates square]))

(defn filled?
  "Is the square filled?"
  [game square]
  (pos-int? (get-in game [:sudoku square])))

(defn unfillable?
  "Are there any candidates for the square?"
  [game square]
  (empty? (cands game square)))

(defn single-candidate?
  "Does the square have exactly one candidate?"
  [game square]
  (= 1 (count (cands game square))))

(defn fill-first-candidate
  "Fill the square with an arbitrary choice among its candidates."
  [game square]
  (fill game square (first (cands game square))))

(defn fill-first-candidate-of-first-single-candidate
  "Fill a square, if any, that has exactly one candidate."
  [game]
  (let [first-single-candidate (first (indices-where single-candidate? game))]
    (cond-> game
            first-single-candidate (fill-first-candidate first-single-candidate))))

(defn guesses
  "Pick an arbitrary square and return a lazy sequence of the games that would
  result from filling its respective candidates."
  [game]
  (let [guessing-square (first (indices-where (complement filled?) game))]
    (->> (cands game guessing-square)
         (map (partial fill game guessing-square)))))

(defn solved?
  "Is every square filled? If so, the sudoku is solved assuming no illegal filling was made."
  [game]
  (not (some #{0} (:sudoku game))))

(defn unsolvable?
  "Are there squares that are not filled, yet have no candidates? If so, the
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
  [sudoku]
  (:sudoku (solve-game (init sudoku))))

(def fmt
"%d %d %d   %d %d %d   %d %d %d
%d %d %d   %d %d %d   %d %d %d
%d %d %d   %d %d %d   %d %d %d

%d %d %d   %d %d %d   %d %d %d
%d %d %d   %d %d %d   %d %d %d
%d %d %d   %d %d %d   %d %d %d

%d %d %d   %d %d %d   %d %d %d
%d %d %d   %d %d %d   %d %d %d
%d %d %d   %d %d %d   %d %d %d")

(defn print-sudoku
  [sudoku]
  (println (apply format fmt sudoku)))

(def easy-sudoku-1
  [0 3 0   0 0 0   0 1 0
   9 0 0   0 0 0   0 0 5
   8 0 5   4 0 9   2 0 7

   0 0 0   3 9 6   0 0 0
   0 8 0   0 0 0   0 2 0
   0 0 0   2 7 8   0 0 0

   3 0 7   8 0 2   5 0 9
   1 0 0   0 0 0   0 0 3
   0 4 0   0 0 0   0 6 0])

(def hard-sudoku-1
  [0 0 8   5 0 4   6 0 0
   0 4 0   0 8 0   0 2 0
   2 0 0   0 0 0   0 0 9

   9 0 0   0 0 0   0 0 1
   4 0 0   0 5 0   0 0 6
   0 6 0   0 0 0   0 7 0

   0 0 7   0 0 0   8 0 0
   0 0 0   9 0 1   0 0 0
   0 0 0   0 3 0   0 0 0])

(def hard-sudoku-2
  [0 9 0   1 4 0   0 0 0
   0 0 5   0 0 0   0 2 0
   0 3 0   0 0 0   0 6 0

   0 4 6   0 0 0   0 0 0
   1 2 0   9 3 0   0 4 5
   0 0 3   0 0 4   0 0 6

   4 0 0   0 0 1   2 0 0
   0 8 0   4 0 0   0 0 3
   3 5 0   7 0 0   9 0 0])

(comment
  (time (print-sudoku (solve-sudoku hard-sudoku-1))))
