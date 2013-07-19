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
    (let [chess-width 8]
        (testing "With first even rank."
            (let [even-rank (create-rank 2 chess-width)]
            ;(println even-rank)
                (is (= :dark (:color (first even-rank))))
                (is (= :light (:color (last even-rank))))))
        (testing "With last even rank."
            (let [even-rank (create-rank 8 chess-width)]
                (is (= :dark (:color (first even-rank))))
                (is (= :light (:color (last even-rank)))))))))
        
(deftest test-odd-ranks
  (testing "Odd ranks should have the first square light and the last dark."
    (let [chess-width 8]
        (testing "With first odd rank."
            (let [odd-rank (create-rank 1 chess-width)]
            ;(println odd-rank)
                (is (= :light (:color (first odd-rank))))
                (is (= :dark (:color (last odd-rank))))))
        (testing "With last odd rank."
            (let [odd-rank (create-rank 7 chess-width)]
                (is (= :light (:color (first odd-rank))))
                (is (= :dark (:color (last odd-rank)))))))))
    
(deftest test-complete-board
    (testing "Assures that the board is complete and the colors are correctly assigned."
        (let [chess-width 8 board (create-board chess-width)]
            (testing "Checks if the number of files and ranks are correct."
                (is (= chess-width (count board)))
                (is (= chess-width (count (first board)))))
            (testing "Checks if an even rank begins with dark color and ends with light color."  
                (let [even-rank (nth board 0)]
                    (is (= :dark (:color (first even-rank))))
                    (is (= :light (:color (last even-rank))))))
            (testing "Checks if an odd rank begins with light color and ends with dark color."  
                (let [odd-rank (nth board 1)]
                    (is (= :light (:color (first odd-rank))))
                    (is (= :dark (:color (last odd-rank)))))))))

                   
(deftest test-populated-board
    (testing "Assures that the board is correctly populated."
        (let [chess-width 8 board (create-board chess-width)]
            (testing "Assigns a white king in the position a1."
                (let [populated-board (populate-board board ["wKa1"])]
                (print-board populated-board)
                (is (= :king (-> (get-in populated-board [0 0]) :piece-name)))))
            (testing "Setup the white pieces."
                (let [populated-board (populate-board board 
                    ["wRa1" "wNb1" "wBc1" "wQd1" "wKe1" "wBf1" "wNg1" "wRh1"
                     "wPa2" "wPb2" "wPc2" "wPd2" "wPe2" "wPf2" "wPg2" "wPh2"])]
                (print-board populated-board)
                (is (= :queen (-> (get-in populated-board [0 3]) :piece-name)))
                (is (= :rook (-> (get-in populated-board [0 7]) :piece-name)))
                (is (= :pawn (-> (get-in populated-board [1 7]) :piece-name)))))
            (testing "Setup all the white and black pieces."
                (let [populated-board (populate-board board 
                    ["wRa1" "wNb1" "wBc1" "wQd1" "wKe1" "wBf1" "wNg1" "wRh1"
                     "wPa2" "wPb2" "wPc2" "wPd2" "wPe2" "wPf2" "wPg2" "wPh2"
                     "bRa8" "bNb8" "bBc8" "bQd8" "bKe8" "bBf8" "bNg8" "bRh8"
                     "bPa7" "bPb7" "bPc7" "bPd7" "bPe7" "bPf7" "bPg7" "bPh7"])]
                (print-board populated-board)
                (is (= :queen (-> (get-in populated-board [0 3]) :piece-name)))
                (is (= :rook (-> (get-in populated-board [0 7]) :piece-name)))
                (is (= :pawn (-> (get-in populated-board [1 7]) :piece-name)))
                (is (= :queen (-> (get-in populated-board [7 3]) :piece-name)))
                (is (= :rook (-> (get-in populated-board [7 7]) :piece-name)))
                (is (= :pawn (-> (get-in populated-board [6 7]) :piece-name))))))))