(ns com.schach.chess
  (:require [com.schach.common :refer :all])
  (:require [com.schach.board :refer :all])
  (:require [com.schach.chess-steps :refer :all]))

(use '[clojure.string :only (join split)])

(def piece-names
    "Map that associates the letter that represents a piece to a specific keyword." 
    {\K :K \Q :Q \R :R \B :B \N :N \P :P})

(defn map-piece-color 
    "Maps the piece letter to the associated color.
    If the letter is uppercase, the piece is black. Otherwise, white."
    [c]
    (if (is-upper? c) :black :white))
    
(defn parse-piece-positioning
    "Parses a string that represents a piece in a given algebric position. 
    The first letter is the name of the piece. 
    If the letter is uppercase, the piece is black. Otherwise, the piece is white."
    [piece-str]
    (let 
        [[name-symbol algebric-position] (rest (re-matches #"([KQRBNPkqrbnp])([a-h][1-8])" piece-str))
         name (piece-names (to-upper (first name-symbol)))
         color (map-piece-color (first name-symbol))]
        [name color algebric-position]))

(defn populate-chess-board 
    [board pieces]
    (let 
        [piece-list 
            (reduce 
                (fn [piece-list piece-str] 
                    (conj piece-list (parse-piece-positioning piece-str)))
                []
                pieces)]
        (populate-board board piece-list)))

(defn setup-chess-board 
    "Fill out a board with the standard chess pieces."
    []
    (let 
        [height 8 width 8
         board (create-board height width)]
        (populate-chess-board board 
            ["ra1" "nb1" "bc1" "qd1" "ke1" "bf1" "ng1" "rh1"
             "pa2" "pb2" "pc2" "pd2" "pe2" "pf2" "pg2" "ph2"
             "Ra8" "Nb8" "Bc8" "Qd8" "Ke8" "Bf8" "Ng8" "Rh8"
             "Pa7" "Pb7" "Pc7" "Pd7" "Pe7" "Pf7" "Pg7" "Ph7"])))

(defn get-all-moves 
    "Finds all moves that a piece can perform given its possible steps. 
    This function simulates the path that the piece can move through 
    multiple sequential steps. Nevertheless, there are pieces that 
    can perform only a single step."
    ([board from-coords possible-steps is-single-step]
        (let 
            [piece-to-move (get-piece board from-coords)
             no-piece-to-move (nil? piece-to-move)]
            ;(println "get-all-moves1" from-coords piece-to-move possible-steps is-single-step)
            (if (true? no-piece-to-move)
                []
                (vec (reduce
                    (fn [moves step] 
                        ;(println moves step)
                        (into 
                            moves 
                           (walk-by-steps 
                                board 
                                piece-to-move 
                                from-coords 
                                step 
                                is-single-step))) 
                    []
                    possible-steps)))))
    ([board from-coords possible-steps]
        (let [is-single-step false]
            ;(println "get-all-moves2" from-coords possible-steps is-single-step)
            (get-all-moves board from-coords possible-steps is-single-step))))

(defmulti get-valid-moves 
    "Finds all valid moves for the piece in the board that is found 
    in the square specified by the given coords." 
    (fn [board from-coords] (:name (get-piece board from-coords))))
(defmethod get-valid-moves :default [board from-coords]
    ;(println :default)
    ())
(defmethod get-valid-moves :K [board from-coords]
    ;(println :K)
    (let 
       [steps (get-king-steps)
        is-single-step true]
       (get-all-moves board from-coords steps is-single-step)))
(defmethod get-valid-moves :Q [board from-coords]
    ;(println :Q)
    (let 
       [steps (get-queen-steps)]
       (get-all-moves board from-coords steps)))
(defmethod get-valid-moves :R [board from-coords]
    ;(println :R)
    (let 
       [steps (get-rook-steps)]
       (get-all-moves board from-coords steps)))
(defmethod get-valid-moves :B [board from-coords]
    ;(println :B)
    (let 
       [steps (get-bishop-steps)]
       (get-all-moves board from-coords steps)))
(defmethod get-valid-moves :N [board from-coords]
    ;(println :N)
    (let 
       [steps (get-knight-steps)
        is-single-step true]
       (get-all-moves board from-coords steps is-single-step)))
(defmethod get-valid-moves :P [board from-coords]
    ;(println :P)
    (let 
       [[y x] from-coords
        steps (remove 
            (fn [[dy dx]] (and (= dy 2) (or (not= dx 0) (not= y 1)))) 
            (get-pawn-steps))
        is-single-step true]
       (get-all-moves board from-coords steps is-single-step)))                         

(defn parse-move 
    "Parses a string that represents a move from a position to
    another in the board. The move is defined by the sequence 
    '<piece><from>[-x]<to>', where <piece> is the letter that represents
    the piece and its color, <from> and <to> are respectively the start 
    and the end positions of the move in algebric notation, and <-x>
    represents the expected result of the move. The character '-' represents
    a move to an empty square and 'x' represents a capture." 
    [move]
    (let [[_ piece-symbol from-position separator to-position annotation] 
          (re-matches #"([KQRBNPkqrbnp])([a-h][1-8])([-x])([a-h][1-8])(\+*)" move)]
        [(first piece-symbol) from-position to-position (= separator "x") annotation]))        

(defn is-correct-piece? 
    "Verifies if an expected piece is located in the board."
    [board position piece-symbol]
    (let   
        [piece-name (piece-names piece-symbol)
         piece-in-place (get-piece board position)]
        ;(println piece-symbol piece-name piece-in-place)
        (= piece-name (:name piece-in-place))))
        
(defn apply-move 
    "Applies a single move in the board. If the move indicates a piece that is
    not located in the specified position, the board is not altered."
    [board move]
    (let 
        [[piece-symbol from-position to-position _ is-capture] (parse-move move)]
        ;(println move piece-symbol from-position to-position is-capture)
        (if (is-correct-piece? board from-position piece-symbol)
            (move-piece board from-position to-position)
            board)))

(def mate-move "+")
(def checkmate-move "++")
            
(defn play-moves 
    "Applies a sequence of moves in the board. Each move is represented by the
    pattern '<white-piece><from>[+|++][-x]<to>...<black-piece>[+|++]<from>[-x]<to>'.
    The '[+|++]' is the annotation of the movement, where '+' represents a check
    and '++' a checkmate. The move of the black piece may not exist in the case 
    of checkmate performed by the white piece."
    [board moves]
    (reduce
        (fn [board move]
            (let 
                [move-reg-exp #"(\w*-*\w*)(\+*)\.*(\w*-*\w*)(\+*)"
                 player-moves (rest (re-matches move-reg-exp move))
                 [white-move white-annotation black-move black-annotation] player-moves
                 board-after-white-move (apply-move board white-move)]
                ;(println move player-moves white-move white-annotation black-move black-annotation)
                (if (not= white-annotation checkmate-move)
                    (apply-move board-after-white-move black-move)
                    board-after-white-move)))
        board
        moves))
