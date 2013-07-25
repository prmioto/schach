(ns com.schach.chess-test
  (:require [clojure.test :refer :all]
            [com.schach.board :refer :all]
            [clojure.pprint :refer :all]))

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
