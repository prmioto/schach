(ns com.schach.chess-test
  (:require [clojure.test :refer :all]
            [com.schach.chess :refer :all]
            [clojure.pprint :refer :all]))

(deftest test-char-to-int
    (testing "Conversion from char to integer."
        (is (= 0 (char-to-int \1 \1)))
        (is (= 7 (char-to-int \8 \1)))
        (is (= 0 (char-to-int \a \a)))
        (is (= 7 (char-to-int \h \a)))))

(deftest test-int-to-char
    (testing "Conversion from integer to char."
        (is (= \1 (int-to-char 0 \1)))
        (is (= \8 (int-to-char 7 \1)))
        (is (= \a (int-to-char 0 \a)))
        (is (= \h (int-to-char 7 \a)))))
        
(deftest test-convertion-from-algebric-to-coordinates
    (testing "Conversion from algebric chess notation to coordinates."
        (is (= [0 0] (from-algebric-to-coordinates "a1")))
        (is (= [7 7] (from-algebric-to-coordinates "h8")))))

(deftest test-convertion-from-coordinates-to-algebric
    (testing "Conversion from coordinates to algebric chess notation."
        (is (= "a1" (from-coordinates-to-algebric [0 0])))
        (is (= "h8" (from-coordinates-to-algebric [7 7])))))
        
(deftest test-rank-color-assignment
    (let [width 8]
        (testing "Even ranks should have the first square dark and the last light."
            (testing "Check first even rank."
                (let [even-rank (create-rank 2 width)]
                ;(println even-rank)
                    (is (= :dark (:color (first even-rank))))
                    (is (= :light (:color (last even-rank))))))
            (testing "Check last even rank."
                (let [even-rank (create-rank 8 width)]
                    (is (= :dark (:color (first even-rank))))
                    (is (= :light (:color (last even-rank)))))))
       (testing "Odd ranks should have the first square light and the last dark."
            (testing "Check first odd rank."
                (let [odd-rank (create-rank 1 width)]
                ;(println odd-rank)
                    (is (= :light (:color (first odd-rank))))
                    (is (= :dark (:color (last odd-rank))))))
            (testing "Check last odd rank."
                (let [odd-rank (create-rank 7 width)]
                    (is (= :light (:color (first odd-rank))))
                    (is (= :dark (:color (last odd-rank)))))))))
    
(deftest test-complete-board
    (testing "Assure that the board is complete and the colors are correctly assigned."
        (let [width 8 height 8 board (create-board height width)]
            (testing "Check if the number of files and ranks are correct."
                (is (= height (count board)))
                (is (= width (count (first board)))))
            (testing "Check if an even rank begins with dark color and ends with light color."  
                (let [even-rank (nth board 0)]
                    (is (= :dark (:color (first even-rank))))
                    (is (= :light (:color (last even-rank))))))
            (testing "Check if an odd rank begins with light color and ends with dark color."  
                (let [odd-rank (nth board 1)]
                    (is (= :light (:color (first odd-rank))))
                    (is (= :dark (:color (last odd-rank)))))))))

(deftest test-replace-piece
    (testing "Check if a single piece is replaced in the board from a specified position."
        (let [
            height 8 width 8 
            new-board (create-board height width)
            coordinates [0 0]
            old-piece {:name :K :color :black}
            new-piece {:name :Q :color :white}
            square (get-in new-board coordinates)
            prepared-board (assoc-in new-board coordinates (assoc square :piece old-piece))]
            (is (= new-piece (:piece (get-in (replace-piece prepared-board coordinates new-piece) coordinates)))))))

(deftest test-parse-piece-str
    (testing "Check if parsing algebric notation with piece indication are done correctly."
        (is (= [:P :black "a1"] (parse-piece-str "Pa1")))
        (is (= [:K :white "h8"] (parse-piece-str "kh8")))))
            
(deftest test-populated-board
    (testing "Assure that the board is correctly populated."
        (let [height 8 width 8 board (create-board height width)]
            (testing "Assign a white king in the position a1."
                (let [populated-board (populate-board board ["ka1"])]
                ;(print-board populated-board)
                (is (= :K (-> (get-in populated-board [0 0]) :piece :name)))))
            (testing "Setup the white pieces."
                (let [populated-board (populate-board board 
                    ["ra1" "nb1" "bc1" "qd1" "ke1" "bf1" "ng1" "rh1"
                     "pa2" "pb2" "pc2" "pd2" "pe2" "pf2" "pg2" "ph2"])]
                ;(print-board populated-board)
                (is (= :Q (-> (get-in populated-board [0 3]) :piece :name)))
                (is (= :R (-> (get-in populated-board [0 7]) :piece :name)))
                (is (= :P (-> (get-in populated-board [1 7]) :piece :name))))))))

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
                
(deftest test-move-piece
    (testing "Check if a piece is moved to the specified location."
        (let 
            [board (setup-chess-board)
             from-position "a1"
             to-position "c4"
             from-coordinates (from-algebric-to-coordinates from-position)
             to-coordinates (from-algebric-to-coordinates to-position)
             piece-to-move ((get-in board from-coordinates) :piece)
             new-board (move-piece board from-position to-position)
             moved-piece ((get-in new-board to-coordinates) :piece)]
        (print-board new-board)
        (is (= piece-to-move (assoc moved-piece :number-of-moves 0)))
        (is (= 1 (moved-piece :number-of-moves))))))
                        
(deftest test-is-opponent
    (testing "Assure that two pieces of the same color are not opponents."
        (is (false? (is-opponent? {:name :K :color :white} {:name :Q :color :white}))))
    (testing "Assure that two pieces of the distinct colors are opponents."
        (is (is-opponent? {:name :K :color :white} {:name :B :color :black}))))

(deftest test-get-square-ocuppant-type
    (testing "Assure that the square is empty."
        (is (= :empty-square 
            (get-square-occupant-type 
                {:name :K :color :white} 
                {:color :dark :piece nil}))))
    (testing "Assure that the square has a teammate."
        (is (= :teammate 
            (get-square-occupant-type
                {:name :K :color :white} 
                {:color :dark :piece {:name :P :color :white}}))))
    (testing "Assure that the square has a opponent."
        (is (= :opponent 
            (get-square-occupant-type 
                {:name :K :color :white} 
                {:color :dark :piece {:name :P :color :black}})))))
                    
(deftest test-walk-by-steps
    (let [chess-board (setup-chess-board)]
        (testing "Check the steps for a king walking to a specified direction from its actual position."
            (let [king {:name :K :color :white} is-single-step true]
                (testing "Check the steps for a locked king walking to its right direction."
                    (is (= [] (walk-by-steps chess-board king [0 4] [0 1] is-single-step))))
                (testing "Check the steps for a free king walking to its right direction."
                    (is (= [[4 5]] (walk-by-steps chess-board king [4 4] [0 1] is-single-step))))
                (testing "Check the steps for a king walking in direction to an opponent."
                    (is (= [[6 4]] (walk-by-steps chess-board king [5 4] [1 0] is-single-step))))
                (testing "Check for possible moves for a free king walking to its right direction outside the board."
                    (is (= [] (walk-by-steps chess-board king [4 7] [0 1] is-single-step))))))
        (testing "Check the steps for a queen walking to a specified direction from its actual position."
            (let [queen {:name :Q :color :white} is-single-step false]
                (testing "Check the steps for a locked queen walking to its right direction."
                    (is (= [] (walk-by-steps chess-board queen [0 3] [0 1] is-single-step))))
                (testing "Check the steps for a free queen walking to its right direction."
                    (is (= [[4 4] [4 5] [4 6] [4 7]] (walk-by-steps chess-board queen [4 3] [0 1] is-single-step))))
                (testing "Check the steps for a queen walking in direction to an opponent."
                    (is (= [[3 3] [4 3] [5 3] [6 3]] (walk-by-steps chess-board queen [2 3] [1 0] is-single-step))))))
                    ))
 
(deftest test-get-moves-given-the-possible-steps
    (let [chess-board (setup-chess-board)]
        (testing "Check the moves given the possible steps that are possible for a king."
                (let [possible-king-steps [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
                    (testing "Check for possible moves for the king locked in the initial position."
                        (is (= [] 
                            (get-all-moves 
                                chess-board 
                                (from-algebric-to-coordinates "e1") 
                                possible-king-steps true))))
                    (testing "Check for possible moves for a king in front of the pawns rank."
                        (let [prepared-board (move-piece chess-board "e1" "e3")]
                            ;(print-board prepared-board)
                            (is (= [[2 3] [2 5] [3 3] [3 4] [3 5]] 
                                (get-all-moves 
                                    prepared-board 
                                    (from-algebric-to-coordinates "e3") 
                                    possible-king-steps true)))))
                    (testing "Check for possible moves for a free king"
                        (let [prepared-board (move-piece chess-board "e1" "e4")]    
                            ;(print-board prepared-board)
                            (is (= [[2 3] [2 4] [2 5] [3 3] [3 5] [4 3] [4 4] [4 5]] 
                                (get-all-moves 
                                    prepared-board 
                                    (from-algebric-to-coordinates "e4") 
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
                                    (from-algebric-to-coordinates "a3") 
                                    possible-queen-steps false)))))))))                
                    
(deftest test-valid-moves
    (let [chess-board (setup-chess-board)]
        (testing "Check if there is no valid moves for an empty square."
            (is (= [] (get-valid-moves chess-board [4 4]))))   
        (testing "Check the valid moves for the king from a given coordinates."
            (testing "Check for valid moves for a locked king in the initial position."
                (is (= [] (get-valid-moves chess-board [0 4]))))
            (testing "Check for valid moves for a free king in position a5."
                (let 
                    [prepared-board (move-piece chess-board "e1" "e5")]
                    (is (= [[3 3] [4 3] [5 3] [3 4] [5 4] [3 5] [4 5] [5 5]] 
                        (get-valid-moves prepared-board [4 4]))))))
        (testing "Check the valid moves for the queen from a given coordinates."
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
        (testing "Check the valid moves for the bishop from a given coordinates."
            (testing "Check for valid moves for a bishop in the center of the board."
                (let 
                    [prepared-board (move-piece chess-board "c1" "c4")]
                    ;(print-board prepared-board)
                    (is (= [[2 1] ; dx=-1 and dy=-1
                            [4 1] [5 0] ; dx=-1 and dy=1
                            [2 3]  ; dx=1 and dy=-1
                            [4 3] [5 4] [6 5]] ;dx=1 and dy=1 
                        (get-valid-moves prepared-board [3 2]))))))
        (testing "Check the valid moves for the rook from a given coordinates."
            (testing "Check for valid moves for a rook in the center of the board."
                (let 
                    [prepared-board (move-piece chess-board "a1" "b5")]
                    ;(print-board prepared-board)
                    (is (= [[4 0] ; dx=-1 and dy=0
                            [3 1] [2 1] ; dx=0 and dy=-1
                            [5 1] [6 1] ; dx=0 and dy=1
                            [4 2] [4 3] [4 4] [4 5] [4 6] [4 7]] ; dx=1 and dy=0
                        (get-valid-moves prepared-board [4 1]))))))
        (testing "Check the valid moves for the knight from a given coordinates."
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
        (testing "Check the valid moves for the pawn from a given coordinates."
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
             from-coordinates (from-algebric-to-coordinates from-position)
             to-coordinates (from-algebric-to-coordinates to-position)
             piece-to-move (:piece (get-in board from-coordinates))
             new-board (apply-move board move)
             moved-piece (:piece (get-in new-board to-coordinates))]
        (print-board new-board)
        (is (= piece-to-move (assoc moved-piece :number-of-moves 0)))
        (is (= 1 (:number-of-moves moved-piece))))))

(deftest test-play-moves
    (testing "Check if a sequence of moves is executed correctly."
        (let [board (setup-chess-board)]
            (testing "Check if the moves of both players are applied correctly."
                (let
                    [moves ["Pe2-e3...Pe7-e5"]
                     new-board (play-moves board moves)]
                    (print-board new-board)
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
                    (print-board new-board)
                    (is (true? (is-correct-piece? new-board "f7" \Q)))
                    (is (true? (is-correct-piece? new-board "c4" \B)))))
                    )))
