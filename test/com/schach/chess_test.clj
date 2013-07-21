(ns com.schach.chess-test
  (:require [clojure.test :refer :all]
            [com.schach.chess :refer :all]
            [clojure.pprint :refer :all]))

(deftest test-char-to-int
    (testing "Tests conversion from char to integer."
        (is (= 0 (char-to-int \1 \1)))
        (is (= 7 (char-to-int \8 \1)))
        (is (= 0 (char-to-int \a \a)))
        (is (= 7 (char-to-int \h \a)))))

(deftest test-convertion-from-algebric-to-coordinates
    (testing "Tests conversion from algebric chess notation to coordinates."
        (is (= [0 0] (from-algebric-to-coordinates "a1")))
        (is (= [7 7] (from-algebric-to-coordinates "h8")))))

(deftest test-even-ranks
  (testing "Even ranks should have the first square dark and the last light."
    (let [width 8]
        (testing "With first even rank."
            (let [even-rank (create-rank 2 width)]
            ;(println even-rank)
                (is (= :dark (:color (first even-rank))))
                (is (= :light (:color (last even-rank))))))
        (testing "With last even rank."
            (let [even-rank (create-rank 8 width)]
                (is (= :dark (:color (first even-rank))))
                (is (= :light (:color (last even-rank)))))))))
        
(deftest test-odd-ranks
  (testing "Odd ranks should have the first square light and the last dark."
    (let [width 8]
        (testing "With first odd rank."
            (let [odd-rank (create-rank 1 width)]
            ;(println odd-rank)
                (is (= :light (:color (first odd-rank))))
                (is (= :dark (:color (last odd-rank))))))
        (testing "With last odd rank."
            (let [odd-rank (create-rank 7 width)]
                (is (= :light (:color (first odd-rank))))
                (is (= :dark (:color (last odd-rank)))))))))
    
(deftest test-complete-board
    (testing "Assures that the board is complete and the colors are correctly assigned."
        (let [width 8 height 8 board (create-board height width)]
            (testing "Checks if the number of files and ranks are correct."
                (is (= height (count board)))
                (is (= width (count (first board)))))
            (testing "Checks if an even rank begins with dark color and ends with light color."  
                (let [even-rank (nth board 0)]
                    (is (= :dark (:color (first even-rank))))
                    (is (= :light (:color (last even-rank))))))
            (testing "Checks if an odd rank begins with light color and ends with dark color."  
                (let [odd-rank (nth board 1)]
                    (is (= :light (:color (first odd-rank))))
                    (is (= :dark (:color (last odd-rank)))))))))

(deftest test-replace-piece
    (testing "Checks if a single piece is replaced in the board from a specified position."
        (let [
            height 8 width 8 
            new-board (create-board height width)
            coordinates [0 0]
            old-piece {:name :king :color :black}
            new-piece {:name :queen :color :white}
            square (get-in new-board coordinates)
            prepared-board (assoc-in new-board coordinates (assoc square :piece old-piece))]
            (is (= new-piece (:piece (get-in (replace-piece prepared-board coordinates new-piece) coordinates)))))))

(deftest test-populated-board
    (testing "Assures that the board is correctly populated."
        (let [height 8 width 8 board (create-board height width)]
            (testing "Assigns a white king in the position a1."
                (let [populated-board (populate-board board ["wKa1"])]
                (print-board populated-board)
                (is (= :king (-> (get-in populated-board [0 0]) :piece :name)))))
            (testing "Setup the white pieces."
                (let [populated-board (populate-board board 
                    ["wRa1" "wNb1" "wBc1" "wQd1" "wKe1" "wBf1" "wNg1" "wRh1"
                     "wPa2" "wPb2" "wPc2" "wPd2" "wPe2" "wPf2" "wPg2" "wPh2"])]
                (print-board populated-board)
                (is (= :queen (-> (get-in populated-board [0 3]) :piece :name)))
                (is (= :rook (-> (get-in populated-board [0 7]) :piece :name)))
                (is (= :pawn (-> (get-in populated-board [1 7]) :piece :name))))))))
                
(deftest test-setup-chess-board
    (testing "Setup all the white and black pieces."
        (let [chess-board (setup-chess-board)]
        (print-board chess-board)
        (is (= :queen (-> (get-in chess-board [0 3]) :piece :name)))
        (is (= :rook (-> (get-in chess-board [0 7]) :piece :name)))
        (is (= :pawn (-> (get-in chess-board [1 7]) :piece :name)))
        (is (= :queen (-> (get-in chess-board [7 3]) :piece :name)))
        (is (= :rook (-> (get-in chess-board [7 7]) :piece :name)))
        (is (= :pawn (-> (get-in chess-board [6 7]) :piece :name))))))
                
(deftest test-move-piece
    (testing "Checks if a piece is moved to the specified location."
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
        
(deftest test-possible-moves
    (let [height 8 width 8]
        (testing "Checks the possible moves for the king from a given coordinates."
            (let [king (create-piece :king :white)]
                (testing "Checks for possible moves for a king in the left-bottom corner."
                    (is (= '([0 1] [1 0] [1 1]) (get-possible-moves king [0 0] height width))))
                (testing "Checks for possible moves for a king in the board center."
                    (is (= '([3 3] [3 4] [3 5] [4 3] [4 5] [5 3] [5 4] [5 5]) (get-possible-moves king [4 4] height width))))))
        (testing "Checks the possible moves for the queen from a given coordinates."
            (let [queen (create-piece :queen :white)]
                (testing "Checks for possible moves for a queen in the left-bottom corner."
                    (is (= '(
                            [0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7] ; dx = 0 and dy = 1
                            [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0] ; dx = 1 and dy = 0
                            [1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7] ; dx = 1 and dy = 1
                        ) (get-possible-moves queen [0 0] height width))))
                (testing "Checks for possible moves for a queen in the board center."
                    (is (= '(
                            [3 3] [2 2] [1 1] [0 0] ; dx = -1 and dy = -1
                            [3 4] [2 4] [1 4] [0 4] ; dx = -1 and dy = 0
                            [3 5] [2 6] [1 7] ; dx = -1 and dy = 1
                            [4 3] [4 2] [4 1] [4 0] ; dx = 0 and dy = -1
                            [4 5] [4 6] [4 7] ; dx = 0 and dy = 1
                            [5 3] [6 2] [7 1] ; dx = 1 and dy = -1
                            [5 4] [6 4] [7 4] ; dx = 1 and dy = 0
                            [5 5] [6 6] [7 7] ; dx = 1 and dy = 1
                        ) (get-possible-moves queen [4 4] height width))))))
        (testing "Checks the possible moves for the bishop from a given coordinates."
            (let [bishop (create-piece :bishop :white)]
                (testing "Checks for possible moves for a bishop in the left-bottom corner."
                    (is (= '(
                            [1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7] ; dx = 1 and dy = 1
                        ) (get-possible-moves bishop [0 0] height width))))
                (testing "Checks for possible moves for a bishop in the board center."
                    (is (= '(
                            [3 3] [2 2] [1 1] [0 0] ; dx = -1 and dy = -1
                            [3 5] [2 6] [1 7] ; dx = -1 and dy = 1
                            [5 3] [6 2] [7 1] ; dx = 1 and dy = -1
                            [5 5] [6 6] [7 7] ; dx = 1 and dy = 1
                        ) (get-possible-moves bishop [4 4] height width))))))
        (testing "Checks the possible moves for the rook from a given coordinates."
            (let [rook (create-piece :rook :white)]
                (testing "Checks for possible moves for a rook in the left-bottom corner."
                    (is (= '(
                            [0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7] ; dx = 0 and dy = 1
                            [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0] ; dx = 1 and dy = 0
                        ) (get-possible-moves rook [0 0] height width))))
                (testing "Checks for possible moves for a rook in the board center."
                    (is (= '(
                            [3 4] [2 4] [1 4] [0 4] ; dx = -1 and dy = 0
                            [4 3] [4 2] [4 1] [4 0] ; dx = 0 and dy = -1
                            [4 5] [4 6] [4 7] ; dx = 0 and dy = 1
                            [5 4] [6 4] [7 4] ; dx = 1 and dy = 0
                        ) (get-possible-moves rook [4 4] height width))))))
        (testing "Checks the possible moves for the pawn from a given coordinates."
            (let [pawn (create-piece :pawn :white)]
                (testing "Checks for possible moves for a pawn in the left-bottom corner."
                    (is (= '([0 1] [1 1]) (get-possible-moves pawn [0 0] height width))))
                (testing "Checks for possible moves for a pawn in the second rank."
                    (is (= '([2 2] [3 2] [3 3] [4 2]) (get-possible-moves pawn [3 1] height width))))
                (testing "Checks for possible moves for a pawn in the board center."
                    (is (= '([3 5] [4 5] [5 5]) (get-possible-moves pawn [4 4] height width))))))
        (testing "Checks the possible moves for the knight from a given coordinates."
            (let [knight (create-piece :knight :white)]
                (testing "Checks for possible moves for a knight in the left-bottom corner."
                    (is (= '([1 2] [2 1]) (get-possible-moves knight [0 0] height width))))
                (testing "Checks for possible moves for a knight in the board center."
                    (is (= '(
                            [2 3] [2 5] 
                            [3 2] [3 6] 
                            [5 2] [5 6] 
                            [6 3] [6 5]
                        ) (get-possible-moves knight [4 4] height width))))))))