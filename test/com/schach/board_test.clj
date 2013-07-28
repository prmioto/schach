(ns com.schach.board-test
    (:require 
            [clojure.test :refer :all]
            [com.schach.board :refer :all]
            [com.schach.print-board :refer :all]
            [clojure.pprint :refer :all])
    (:import [com.schach.board Piece]))

(deftest test-convertion-from-algebric-to-coords
    (testing "Conversion from algebric chess notation to coords."
        (is (= [0 0] (from-algebric-to-coords "a1")))
        (is (= [7 7] (from-algebric-to-coords "h8")))))

(deftest test-convertion-from-coords-to-algebric
    (testing "Conversion from coords to algebric chess notation."
        (is (= "a1" (from-coords-to-algebric [0 0])))
        (is (= "h8" (from-coords-to-algebric [7 7])))))
        
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

(deftest test-put-and-get-piece-from-to-board
    (testing "Check if a single piece is assigned to a specified position in the board."
        (let 
            [height 8 width 8 
             board (create-board height width)
             algebric-position "a1"
             piece (Piece. :king :black 0)
             updated-board (put-piece board piece algebric-position)]
            (is (= piece (get-piece updated-board algebric-position))))))

(deftest test-move-piece
    (testing "Check if a piece is moved to the specified location."
        (let 
            [height 8 width 8 
             board (create-board height width)
             from-position "a1"
             to-position "c4"
             piece-to-move (Piece. :king :white 0)
             prepared-board (put-piece board piece-to-move from-position)
             updated-board (move-piece prepared-board from-position to-position)
             moved-piece (get-piece updated-board to-position)]
        ;(print-board updated-board)
        (is (= [:king :white 1] ((juxt :name :color :number-of-moves) moved-piece))))))

(deftest test-populate-board
    (testing "Assure that the board is correctly populated."
        (let 
            [height 8 width 8 
             board (create-board height width)]
            (testing "Assign a white king in the position a1."
                (let [updated-board (populate-board board [[:king :white "a1"]])]
                    (print-board updated-board)
                    (is (= [:king :white 0] ((juxt :name :color :number-of-moves) (get-piece updated-board "a1"))))))
            (testing "Setup the white pieces."
                (let [updated-board 
                        (populate-board board 
                            [[:queen :white "d1"]
                             [:rook :black "h1"]
                             [:pawn :white "a2"]])]
                    (print-board updated-board)
                    (is (= [:queen :white 0] ((juxt :name :color :number-of-moves) (get-piece updated-board "d1"))))
                    (is (= [:rook :black 0] ((juxt :name :color :number-of-moves) (get-piece updated-board "h1"))))
                    (is (= [:pawn :white 0] ((juxt :name :color :number-of-moves) (get-piece updated-board "a2")))))))))
        
(deftest test-get-collaboration-type
    (let 
        [white-piece (Piece. :K :white 0) 
         another-white-piece (Piece. :P :white 0)
         black-piece (Piece. :Q :black 0)]
    (testing "Assure that the square is empty."
        (is (nil? (get-collaboration-type white-piece nil))))
    (testing "Assure that the square has a teammate."
        (is (= :teammate 
            (get-collaboration-type white-piece another-white-piece))))
    (testing "Assure that the square has a opponent."
        (is (= :opponent 
            (get-collaboration-type white-piece black-piece))))))

(defn generate-rank [rank piece-list color]
    (let [width (count piece-list)]
        (vec (map #(vector %1 %2 %3)
            piece-list 
           (repeat width color) 
           (take width (map #(str (char %) rank) (iterate inc (int \a))))))))
           
(deftest test-walk-by-steps
    (let
        [height 8 width 8 
         piece-list (vec (concat
                        (generate-rank "8" [:r :n :b :q :k :b :n :r] :black)
                        (generate-rank "7" [:p :p :p :p :p :p :p :p] :black)
                        (generate-rank "2" [:p :p :p :p :p :p :p :p] :white)
                        (generate-rank "1" [:r :n :b :q :k :b :n :r] :white)))
         chess-board (populate-board (create-board height width) piece-list)]
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
