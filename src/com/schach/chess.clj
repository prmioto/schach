(ns com.schach.chess
  (:require [com.schach.common :refer :all])
  (:require [com.schach.board :refer :all])
  (:require [com.schach.chess-steps :refer :all]))

(use '[clojure.string :only (join split)])

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
