(ns com.schach.chess)

(defn abs [x] (if (>= x 0) x (* x -1)))

(defn to-upper [s] (.toUpperCase s))
(defn to-lower [s] (.toLowerCase s))

(defn char-to-int [char-value base-char]
    (- (int char-value) (int base-char)))

(defn int-to-char [int-value base-char]
    (char (+ int-value (int base-char))))

(defn is-algebric-position? [value] (string? value))
(defn are-coordinates? [value] (vector? value))

(def file-base-char \a)
(def rank-base-char \1)
    
(defn from-algebric-to-coordinates [algebric-position]
    (let [[file-char rank-char] (vec algebric-position)]
        [(char-to-int rank-char rank-base-char) (char-to-int file-char file-base-char)]))

(defn from-coordinates-to-algebric [coordinates]
    (let [[rank file] coordinates]
        (str (int-to-char file file-base-char) (int-to-char rank rank-base-char))))
        
(defn create-empty-square [color]
    {:color color :piece nil})
    
(defn create-rank [rank-index width]
    (vec (take width 
        (map #(create-empty-square (if (zero? (mod % 2)) :dark :light))
            (iterate inc (mod rank-index 2))))))

(defn create-board
  "Creates a empty square chess board of the specified width and height."
  [height width]
  (vec (map #(create-rank % width) (range 0 height))))
    
(def chess-piece-names
    {\K :K \Q :Q \R :R \B :B \N :N \P :P})
        
(def chess-piece-colors
    {\w :white \b :black})

(defn create-piece [name color]
    {:name name :color color :number-of-moves 0})
    
(defn replace-piece [board coordinates new-piece]
    (let [square (get-in board coordinates)]
        (assoc-in board coordinates (assoc (dissoc square :piece) :piece new-piece))))

(defn populate-board [board pieces]
    ;(println board pieces)
    (reduce (fn [board piece-str]
        ;(println "piece" piece-str)
		(let [
            color-symbol (first piece-str)
            color (chess-piece-colors color-symbol)
            name-symbol (first (rest piece-str))
            name (chess-piece-names name-symbol)
            algebric-position (rest (rest piece-str))
            coordinates (from-algebric-to-coordinates algebric-position)
            new-piece (create-piece name color)]
            ;(println color name algebric-position coordinates new-piece)
            (replace-piece board coordinates new-piece)))
            board pieces))

(defn setup-chess-board []
    (let [
        height 8 width 8
        board (create-board height width)]
        (populate-board board 
            ["wRa1" "wNb1" "wBc1" "wQd1" "wKe1" "wBf1" "wNg1" "wRh1"
             "wPa2" "wPb2" "wPc2" "wPd2" "wPe2" "wPf2" "wPg2" "wPh2"
             "bRa8" "bNb8" "bBc8" "bQd8" "bKe8" "bBf8" "bNg8" "bRh8"
             "bPa7" "bPb7" "bPc7" "bPd7" "bPe7" "bPf7" "bPg7" "bPh7"])))
 
(defn move-piece-by-coordinates [board from-coordinates to-coordinates]
    (let 
        [from-square (get-in board from-coordinates)
         to-square (get-in board to-coordinates)
         piece (:piece from-square)
         number-of-moves (:number-of-moves piece)
         updated-piece (assoc (dissoc piece :number-of-moves) :number-of-moves (inc number-of-moves))
         updated-from-square (dissoc from-square :piece)
         updated-to-square (assoc to-square :piece updated-piece)]
        (assoc-in (assoc-in board from-coordinates updated-from-square) to-coordinates updated-to-square)))
        
(defn move-piece [board from-algebric-position to-algebric-position]
     (let [
        from-coordinates (from-algebric-to-coordinates from-algebric-position)
        to-coordinates (from-algebric-to-coordinates to-algebric-position)]
     (move-piece-by-coordinates board from-coordinates to-coordinates)))

(defn is-opponent? [piece-to-move piece-to-check]
    (not= (:color piece-to-move) (:color piece-to-check)))

(defn get-square-occupant-type [piece-to-move to-square] 
    (let [piece-to-check (:piece to-square)]
        (cond
            (nil? piece-to-check) :empty-square
            (is-opponent? piece-to-move piece-to-check) :opponent
            (true? true) :teammate)))
  
(defn walk-by-steps [board piece-to-move from-coordinates step is-single-step]
    (let [height (count board) width (count (board 0))]
        (loop [to-coordinates (vec (map + from-coordinates step)) moves [] opponent-found false number-of-steps 0]
           (let [occupant-type (get-square-occupant-type piece-to-move (get-in board to-coordinates))]
                ;(println to-coordinates moves opponent-found occupant-type number-of-steps)
                (if (or 
                        (and is-single-step (= 1 number-of-steps))
                        (true? (some #(< % 0) to-coordinates))
                        (true? (some true? (map >= to-coordinates [height width])))
                        (= :teammate occupant-type)
                        (true? opponent-found))
                    moves
                    (recur 
                        (vec (map + to-coordinates step))
                        (conj moves to-coordinates) 
                        (= :opponent occupant-type) 
                        (inc number-of-steps)))))))
                        
(defn get-all-moves [board from-coordinates possible-steps is-single-step]
    (let 
        [piece-to-move (:piece (get-in board from-coordinates))
         no-piece-to-move (nil? piece-to-move)]
        ;(println from-coordinates piece-to-move possible-steps)
        (if (true? no-piece-to-move)
            []
            (vec (reduce
                (fn [moves step] 
                    ;(println moves step)
                    (into moves (walk-by-steps board piece-to-move from-coordinates step is-single-step))) 
                []
               possible-steps)))))                        

(defn get-king-steps []
    (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] [dy dx]))

(defn get-queen-steps []
    (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] [dy dx]))
        
(defn get-rook-steps []
    (for [dx [-1 0 1] dy [-1 0 1] :when (and (not= 0 dx dy) (or (= 0 dx) (= 0 dy)))] [dy dx]))

(defn get-bishop-steps []
    (for [dx [-1 0 1] dy [-1 0 1] :when (and (not= 0 dx) (not= 0 dy))] [dy dx]))

(defn get-knight-steps []
    (for [dx [-2 -1 1 2] dy [-2 -1 1 2] :when (not= (abs dx) (abs dy))] [dy dx]))

(defn get-pawn-steps []
    (for [dx [-1 0 1] dy [1 2]] [dy dx]))
 
(defmulti get-valid-moves (fn [board from-coordinates] (-> (get-in board from-coordinates) :piece :name)))
(defmethod get-valid-moves :default [board from-coordinates]
    ;(println :default)
    ())
(defmethod get-valid-moves :K [board from-coordinates]
    ;(println :K)
    (let 
       [steps (get-king-steps)
        is-single-step true]
        (get-all-moves board from-coordinates steps is-single-step)))
(defmethod get-valid-moves :Q [board from-coordinates]
    ;(println :Q)
    (let 
       [steps (get-queen-steps)
        is-single-step false]
        (get-all-moves board from-coordinates steps is-single-step)))
(defmethod get-valid-moves :R [board from-coordinates]
    ;(println :R)
    (let 
       [steps (get-rook-steps)
        is-single-step false]
        (get-all-moves board from-coordinates steps is-single-step)))
(defmethod get-valid-moves :B [board from-coordinates]
    ;(println :B)
    (let 
       [steps (get-bishop-steps)
        is-single-step false]
        (get-all-moves board from-coordinates steps is-single-step)))
(defmethod get-valid-moves :N [board from-coordinates]
    ;(println :N)
    (let 
       [steps (get-knight-steps)
        is-single-step true]
        (get-all-moves board from-coordinates steps is-single-step)))
(defmethod get-valid-moves :P [board from-coordinates]
    ;(println :P)
    (let 
       [[y x] from-coordinates
        steps (remove 
            (fn [[dy dx]] (and (= dy 2) (or (not= dx 0) (not= y 1)))) 
            (get-pawn-steps))
        is-single-step true]
        (get-all-moves board from-coordinates steps is-single-step)))
        
(defn print-board [board]
    (println
        (reduce 
            (fn [board-str rank] 
                (str board-str
                    (reduce (fn [rank-str square] 
                        (let 
                            [is-dark (= (:color square) :dark)
                             piece (:piece square)
                             is-empty (nil? piece)
                             is-black (= (:color piece) :black)
                             piece-name (if (true? is-empty) "" (name (:name piece)))
                             piece-rep (if (true? is-black) (partial to-upper) (partial to-lower))]
                            (str rank-str 
                                (format "%s %1s %s" 
                                    (if (true? is-dark) "[" " ") 
                                    (piece-rep piece-name)
                                    (if (true? is-dark) "]" " "))
                               "|")))    
                    "|" rank) "\n")) 
            "" (reverse board))))