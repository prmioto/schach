(ns com.schach.chess)

(use '[clojure.string :only (join split)])

(defn abs [x] (if (>= x 0) x (* x -1)))

(defn to-upper [c] (Character/toUpperCase c))
(defn to-lower [c] (Character/toLowerCase c))
(defn is-upper? [c] (Character/isUpperCase c))
(defn is-lower? [c] (Character/isLowerCase c))

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
        (map #(create-empty-square (if (even? %) :dark :light))
            (iterate inc (mod rank-index 2))))))

(defn create-board
  "Creates a empty square chess board of the specified width and height."
  [height width]
  (vec (map #(create-rank % width) (range 0 height))))
    
(def chess-piece-names
    {\K :K \Q :Q \R :R \B :B \N :N \P :P})

(defn map-piece-color [c]
    (if (is-upper? c) :black :white))
    
(defn parse-piece-str [piece-str]
    (let 
        [[_ name-symbol algebric-position] (re-matches #"([KQRBNPkqrbnp])([a-h][1-8])" piece-str)
         name (chess-piece-names (to-upper (first name-symbol)))
         color (map-piece-color (first name-symbol))]
        [name color algebric-position]))
        
(defn create-piece [name color]
    {:name name :color color :number-of-moves 0})
    
(defn replace-piece [board coordinates new-piece]
    (let [square (get-in board coordinates)]
        (assoc-in board coordinates (assoc (dissoc square :piece) :piece new-piece))))

(defn populate-board [board pieces]
    ;(println board pieces)
    (reduce 
        (fn [board piece-str]
            ;(println "piece" piece-str)
            (let 
                [[name color algebric-position] (parse-piece-str piece-str)
                 new-piece (create-piece name color)
                 coordinates (from-algebric-to-coordinates algebric-position)]
                 ;(println color name algebric-position coordinates new-piece)
                (replace-piece board coordinates new-piece)))
        board
        pieces))

(defn setup-chess-board []
    (let 
        [height 8 width 8
         board (create-board height width)]
        (populate-board board 
            ["ra1" "nb1" "bc1" "qd1" "ke1" "bf1" "ng1" "rh1"
             "pa2" "pb2" "pc2" "pd2" "pe2" "pf2" "pg2" "ph2"
             "Ra8" "Nb8" "Bc8" "Qd8" "Ke8" "Bf8" "Ng8" "Rh8"
             "Pa7" "Pb7" "Pc7" "Pd7" "Pe7" "Pf7" "Pg7" "Ph7"])))
 
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
     (let 
        [from-coordinates (from-algebric-to-coordinates from-algebric-position)
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

(defn parse-move [move]
    (let [[_ piece-symbol from-position separator to-position annotation] 
          (re-matches #"([KQRBNPkqrbnp])([a-h][1-8])([-x])([a-h][1-8])(\+*)" move)]
        [(first piece-symbol) from-position to-position (= separator "x") annotation]))        

(defn is-correct-piece? [board position piece-symbol]
    (let   
        [piece-name (chess-piece-names piece-symbol)
        piece-in-place (:piece (get-in board (from-algebric-to-coordinates position)))]
        ;(println piece-symbol piece-name piece-in-place)
        (= piece-name (:name piece-in-place))))
        
(defn apply-move [board move]
    (let 
        [[piece-symbol from-position to-position _ is-capture] (parse-move move)]
        ;(println move piece-symbol from-position to-position is-capture)
        (if (is-correct-piece? board from-position piece-symbol)
            (move-piece board from-position to-position)
            board)))

(def mate "+")
(def checkmate "++")
            
(defn play-moves [board moves]
    (reduce
        (fn [board move]
            (let 
                [move-reg-exp #"(\w*-*\w*)(\+*)\.*(\w*-*\w*)(\+*)"
                 player-moves (re-matches move-reg-exp move)
                 [_ white-move white-annotation black-move black-annotation] player-moves
                 board-after-white-move (apply-move board white-move)]
                ;(println move player-moves white-move white-annotation black-move black-annotation)
                (if (not= white-annotation checkmate)
                    (apply-move board-after-white-move black-move)
                    board-after-white-move)))
        board
        moves))
        
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
                             piece-name (if (true? is-empty) \space (first (name (:name piece))))
                             piece-representation (if (true? is-black) (partial to-upper) (partial to-lower))]
                            (str rank-str 
                                (format "%s %1s %s" 
                                    (if (true? is-dark) "[" " ") 
                                    (piece-representation piece-name)
                                    (if (true? is-dark) "]" " "))
                               "|")))    
                    "|" rank) "\n")) 
            "" (reverse board))))