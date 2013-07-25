(ns com.schach.board
  (:require [com.schach.common :refer :all]))

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

(defn create-piece [name color]
    {:name name :color color :number-of-moves 0})
    
(defn replace-piece [board coordinates new-piece]
    (let [square (get-in board coordinates)]
        (assoc-in board coordinates (assoc (dissoc square :piece) :piece new-piece))))

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
