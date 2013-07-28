(ns com.schach.chess-test
  (:require [clojure.test :refer :all]
            [com.schach.common :refer :all]
            [com.schach.board :refer :all]
            [com.schach.chess :refer :all]
            [com.schach.print-board :refer :all]
            [clojure.pprint :refer :all]))

(deftest test-parse-piece-positioning
    (testing "Check if parsing algebric notation with piece indication are done correctly."
        (is (= [:P :black "a1"] (parse-piece-positioning "Pa1")))
        (is (= [:K :white "h8"] (parse-piece-positioning "kh8")))))
            
(deftest test-populate-chess-board
    (testing "Assure that a chess board is correctly populated by the given piece positioning string list."
        (let [height 8 width 8 board (create-board height width)]
            (testing "Assign a black king in the position a1."
                (let [updated-board (populate-chess-board board ["Ka1"])]
                ;(print-board updated-board)
                    (is (= [:K :black 0] ((juxt :name :color :number-of-moves) (get-piece updated-board "a1"))))))
            (testing "Check if the white pieces are correctly located in the board."
                (let [updated-board 
                        (populate-chess-board board 
                            ["ra1" "nb1" "bc1" "qd1" "ke1" "bf1" "ng1" "rh1"
                             "pa2" "pb2" "pc2" "pd2" "pe2" "pf2" "pg2" "ph2"])]
                ;(print-board updated-board)
                    (is (= [:Q :white 0] ((juxt :name :color :number-of-moves) (get-piece updated-board "d1"))))
                    (is (= [:R :white 0] ((juxt :name :color :number-of-moves) (get-piece updated-board "h1"))))
                    (is (= [:P :white 0] ((juxt :name :color :number-of-moves) (get-piece updated-board "a2")))))))))

(deftest test-setup-chess-board
    (testing "Setup all the white and black pieces."
        (let [chess-board (setup-chess-board)]
        (print-board chess-board)
        (is (= :Q (-> (get-in chess-board [0 3]) :piece :name)))
        (is (= :R (-> (get-in chess-board [0 7]) :piece :name)))
        (is (= :P (-> (get-in chess-board [1 7]) :piece :name)))
        (is (= :Q (-> (get-in chess-board [7 3]) :piece :name)))
        (is (= :R (-> (get-in chess-board [7 7]) :piece :name)))
        (is (= :P (-> (get-in chess-board [6 7]) :piece :name))))))
                
 
(deftest test-get-moves-given-the-possible-steps
    (let [chess-board (setup-chess-board)]
        (testing "Check the moves given the possible steps that are possible for a king."
                (let [possible-king-steps [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
                    (testing "Check for possible moves for the king locked in the initial position."
                        (is (= [] 
                            (get-all-moves 
                                chess-board 
                                (from-algebric-to-coords "e1") 
                                possible-king-steps true))))
                    (testing "Check for possible moves for a king in front of the pawns rank."
                        (let [prepared-board (move-piece chess-board "e1" "e3")]
                            ;(print-board prepared-board)
                            (is (= [[2 3] [2 5] [3 3] [3 4] [3 5]] 
                                (get-all-moves 
                                    prepared-board 
                                    (from-algebric-to-coords "e3") 
                                    possible-king-steps true)))))
                    (testing "Check for possible moves for a free king"
                        (let [prepared-board (move-piece chess-board "e1" "e4")]    
                            ;(print-board prepared-board)
                            (is (= [[2 3] [2 4] [2 5] [3 3] [3 5] [4 3] [4 4] [4 5]] 
                                (get-all-moves 
                                    prepared-board 
                                    (from-algebric-to-coords "e4") 
                                    possible-king-steps true)))))))
        (testing "Check the moves given the possible steps that are possible for a queen."
                (let [possible-queen-steps [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
                    (testing "Check for possible moves for a queen in front of the rank of pawns."
                        (let [prepared-board (move-piece chess-board "d1" "a3")]
                            ;(print-board prepared-board)
                            (is (= [[2 1] [2 2] [2 3] [2 4] [2 5] [2 6] [2 7] ; dx=1 and dy=0
                                    [3 0] [4 0] [5 0] [6 0] ; dx=0 and dy=1
                                    [3 1] [4 2] [5 3] [6 4]] ; dx=1 and dy=1 
                                (get-all-moves 
                                    prepared-board 
                                    (from-algebric-to-coords "a3") 
                                    possible-queen-steps)))))))))                
                    
(deftest test-valid-moves
    (let [chess-board (setup-chess-board)]
        (testing "Check if there is no valid moves for an empty square."
            (is (= [] (get-valid-moves chess-board [4 4]))))   
        (testing "Check the valid moves for the king from a given coords."
            (testing "Check for valid moves for a locked king in the initial position."
                (is (= [] (get-valid-moves chess-board [0 4]))))
            (testing "Check for valid moves for a free king in position a5."
                (let 
                    [prepared-board (move-piece chess-board "e1" "e5")]
                    (is (= [[3 3] [4 3] [5 3] [3 4] [5 4] [3 5] [4 5] [5 5]] 
                        (get-valid-moves prepared-board [4 4]))))))
        (testing "Check the valid moves for the queen from a given coords."
            (testing "Check for valid moves for a queen in the center of the board."
                (let 
                    [prepared-board (move-piece chess-board "d1" "d5")]
                    ;(print-board prepared-board)
                    (is (= [[3 2] [2 1] ; dx=-1 and dy=-1
                            [4 2] [4 1] [4 0] ; dx=-1 and dy=0
                            [5 2] [6 1] ; dx=-1 and dy=1
                            [3 3] [2 3] ; dx=0 and dy=-1
                            [5 3] [6 3] ; dx=0 and dy=1
                            [3 4] [2 5] ; dx=1 and dy=-1
                            [4 4] [4 5] [4 6] [4 7] ;dx=1 and dy=0
                            [5 4] [6 5]] ;dx=1 and dy=1 
                        (get-valid-moves prepared-board [4 3]))))))                     
        (testing "Check the valid moves for the bishop from a given coords."
            (testing "Check for valid moves for a bishop in the center of the board."
                (let 
                    [prepared-board (move-piece chess-board "c1" "c4")]
                    ;(print-board prepared-board)
                    (is (= [[2 1] ; dx=-1 and dy=-1
                            [4 1] [5 0] ; dx=-1 and dy=1
                            [2 3]  ; dx=1 and dy=-1
                            [4 3] [5 4] [6 5]] ;dx=1 and dy=1 
                        (get-valid-moves prepared-board [3 2]))))))
        (testing "Check the valid moves for the rook from a given coords."
            (testing "Check for valid moves for a rook in the center of the board."
                (let 
                    [prepared-board (move-piece chess-board "a1" "b5")]
                    ;(print-board prepared-board)
                    (is (= [[4 0] ; dx=-1 and dy=0
                            [3 1] [2 1] ; dx=0 and dy=-1
                            [5 1] [6 1] ; dx=0 and dy=1
                            [4 2] [4 3] [4 4] [4 5] [4 6] [4 7]] ; dx=1 and dy=0
                        (get-valid-moves prepared-board [4 1]))))))
        (testing "Check the valid moves for the knight from a given coords."
            (testing "Check for valid moves for a knight in front the rank of the pawns."
                (let 
                    [prepared-board (move-piece chess-board "g1" "f3")]
                    ;(print-board prepared-board)
                    (is (= [[3 3] [4 4] [0 6] [4 6] [3 7]]
                        (get-valid-moves prepared-board [2 5])))))
            (testing "Check for valid moves for a knight next to the rank of opponent pawns."
                (let 
                    [prepared-board (move-piece chess-board "g1" "f6")]
                    ;(print-board prepared-board)
                    (is (= [[4 3] [6 3] [3 4] [7 4] [3 6] [7 6] [4 7] [6 7]]
                        (get-valid-moves prepared-board [5 5]))))))
        (testing "Check the valid moves for the pawn from a given coords."
            (testing "Check for valid moves for a pawn in the initial position."
                (is (= [[2 2] [2 3] [3 3] [2 4]]
                    (get-valid-moves chess-board [1 3])))))
            (testing "Check for valid moves for a pawn in the center of the board."
                (let [prepared-board (move-piece chess-board "d2" "d4")]
                (is (= [[4 2] [4 3] [4 4]]
                    (get-valid-moves prepared-board [3 3])))))))
                    
(deftest test-apply-move
    (testing "Check if a player move is applied correctly."
        (let 
            [board (setup-chess-board)
             piece-to-move "R"
             from-position "a1"
             to-position "c4"
             move (str piece-to-move from-position "-" to-position)
             from-coords (from-algebric-to-coords from-position)
             to-coords (from-algebric-to-coords to-position)
             piece-to-move (:piece (get-in board from-coords))
             new-board (apply-move board move)
             moved-piece (:piece (get-in new-board to-coords))]
        ;(print-board new-board)
        (is (= piece-to-move (assoc moved-piece :number-of-moves 0)))
        (is (= 1 (:number-of-moves moved-piece))))))

(deftest test-play-moves
    (testing "Check if a sequence of moves is executed correctly."
        (let [board (setup-chess-board)]
            (testing "Check if the moves of both players are applied correctly."
                (let
                    [moves ["Pe2-e3...Pe7-e5"]
                     new-board (play-moves board moves)]
                    ;(print-board new-board)
                    (is (true? (is-correct-piece? new-board "e3" \P)))
                    (is (true? (is-correct-piece? new-board "e5" \P)))))
            (testing "Check if the scholar move is played correctly."
                (let
                    [moves 
                        ["Pe2-e3...Pe7-e5"
                         "Bf1-c4...Nb8-c6"
                         "Qd1-h5...Ng8-f6"
                         "Qh5xf7++"]
                     new-board (play-moves board moves)]
                    ;(print-board new-board)
                    (is (true? (is-correct-piece? new-board "f7" \Q)))
                    (is (true? (is-correct-piece? new-board "c4" \B)))))
                    )))
