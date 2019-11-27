(ns peg-game.core
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn tri*
  "Generates lazy seq of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular? 
  "Is the number triangular? 1, 3, 6, 10, etc?"
  [n]
  (= n (last (take-while #(<= % n) tri))))

(defn row-tri
  "Triangular number at the end of row n"
  [n]
  (last (take n tri))
)

(defn row-num
  "Returns row num based on pos: p1 r1, p2,3 r2"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  "Form a mutual connection between two pos"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce 
     (fn [new-board [p1 p2]]
       (assoc-in new-board [p1 :connections p2] neighbor))
     board
     [[pos destination] [destination pos]])
    board))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ pos row)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 pos row)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Creates a board with a given num of rows"
  [rows]
  (let [max-pos (row-tri rows)
        initial-board {:rows rows}]
    (reduce (fn [board pos]
              (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

(def my-board (assoc-in (new-board 5) [4 :pegged] false))

(defn pegged?
  "Does the pos have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Remove peg from pos"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Place peg into pos"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Move peg from p1 to p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "Return map of valid moves for pos"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))) 
        (get-in board [pos :connections])))

(defn valid-move?
  "Checks if move from p1 to p2 is valid"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Jump from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)] 
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  [board]
  (some (comp not-empty (partial valid-moves board)) (map first (filter #(get (second %) :pegged) board)))
)

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (pegged? board pos)
         "0"
         "-")))

(defn row-positions
  "Returns all positions in a given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))  
         (inc (row-tri row-num))))

(defn row-padding
  "String of spaces to add to line to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board) (row-positions row-num)))))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

(defn letter->pos
  "Converts a letter to corresponding pos num"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "Waits for a user to enter text, then cleans input"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))

(defn characters-as-strings
  [string]
  (map str (filter #(Character/isLetter %) string)))

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn game-over
  "Announce the game is over and prompt to play again?"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had " remaining-pegs "pegs left:")
    (print-board board)
    (println "Play again? [y/n]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(defn user-entered-invalid-move
  "Handles next step after user entered invalid move"
  [board]
  (println "\n!!! That was an invalid move. :( \n")
  (prompt-move board))

(defn user-entered-valid-move
  "Handles next step after user entered valid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board))
  )

(defn prompt-move
  [board]
  (println "\n Here's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move new-board))))


