(ns com.schach.board
  (:require [com.schach.common :refer :all]))

(import '[clojure.lang Sequential])
  
(defn is-algebric? [value] (string? value))
(defn are-coords? [value] (vector? value))

(def file-base-char \a)
(def rank-base-char \1)
    
(defn from-algebric-to-coords [algebric-position]
    (let [[file-char rank-char] (vec algebric-position)
          rank (char-to-int rank-char rank-base-char)
          file (char-to-int file-char file-base-char)]
        [rank file]))

(defn from-coords-to-algebric [coords]
    (let [[rank file] coords
          file-char (int-to-char file file-base-char)
          rank-char (int-to-char rank rank-base-char)]
        (str file-char rank-char)))

(defrecord Square [color piece])
(defrecord Piece [name color number-of-moves])

(defn create-rank [rank width]
    (vec (take width 
        (map #(Square. (if (even? %) :dark :light) nil)
            (iterate inc (mod rank 2))))))
            
(defn create-board
  "Creates a empty square chess board of the specified width and height."
  [height width]
  (vec (map #(create-rank % width) (range 0 height))))

(defmulti get-piece 
    "Retrieves a piece from a give location specified  
     either by coordinates or by algebric notation."
    (fn [board position] (type position)))
(defmethod get-piece  Sequential [board coords]
    (let [square (get-in board coords)]
        ;(println coords square)
        (:piece square)))
(defmethod get-piece String [board algebric-position]
    (let [coords (from-algebric-to-coords algebric-position)]
        ;(println algebric-position)
        (get-piece board coords)))

(defmulti put-piece 
    "Puts a piece in a give location specified either by 
     coordinates or by algebric notation."
    (fn [board piece position] (type position)))
(defmethod put-piece Sequential [board piece coords]
    (let 
        [square (get-in board coords)
         new-square (assoc square :piece piece)]
        ;(println square new-square)
        (assoc-in board coords new-square)))
(defmethod put-piece String [board piece algebric-position]
    (let [coords (from-algebric-to-coords algebric-position)]
        ;(println algebric-position coords)
        (put-piece board piece coords)))

(defmulti remove-piece 
    "Removes a piece from the board located in a location
     specified either by coordinates or by algebric location."
    (fn [board position] (type position)))        
(defmethod remove-piece Sequential [board coords]
    (let 
        [square (get-in board coords)
         new-square (dissoc square :piece)]
        (assoc-in board coords new-square)))
(defmethod remove-piece String [board algebric-position]
    (let [coords (from-algebric-to-coords algebric-position)]
        (remove-piece board coords)))

(defmulti move-piece
    "Moves a piece from one location to another. Both specified either
    by coordinates or by algebric notation."
    (fn [board from-position to-position] [(type from-position) (type to-position)]))
(defmethod move-piece [Sequential Sequential] [board from-coords to-coords]
    (let 
        [piece (get-piece board from-coords)
         number-of-moves (:number-of-moves piece)
         updated-piece (assoc piece :number-of-moves (inc number-of-moves))]
        (put-piece (remove-piece board from-coords) updated-piece to-coords)))
(defmethod move-piece [String String] [board from-algebric-position to-algebric-position]
    (let 
        [from-coords (from-algebric-to-coords from-algebric-position)
         to-coords (from-algebric-to-coords to-algebric-position)]
        (move-piece board from-coords to-coords)))

(defn populate-board 
    "Populates a board by a given a collection of piece information 
    represented by a vector that contains the piece name, its color and
    location in algebric notation."
    [board piece-list]
    ;(println board piece-list)
    (reduce 
        (fn [board piece-info]
            ;(println "piece-info" piece-info)
            (let 
                [[name color algebric-position] piece-info
                 new-piece (Piece. name color 0)]
                 ;(println color name algebric-position new-piece)
                (put-piece board new-piece algebric-position)))
        board
        piece-list))
 
(defn get-collaboration-type
    "Finds out if a piece is an opponent or not."
    [piece-to-move target-piece] 
    (cond
        (nil? target-piece) nil
        (not= (:color piece-to-move) (:color target-piece)) :opponent
        :else :teammate))

(defn walk-by-steps 
    "Finds the available positions for a piece in the board given a direction."  
    [board piece-to-move from-coords step is-single-step]
    (let [height (count board) width (count (board 0))]
        (loop 
            [to-coords (vec (map + from-coords step)) 
             moves [] 
             opponent-found false 
             number-of-steps 0]
           (let 
                [occupant-piece (get-piece board to-coords)
                 collaboration-type (get-collaboration-type piece-to-move occupant-piece)]
                ;(println "walk-by-steps" to-coords moves opponent-found occupant-type number-of-steps)
                (if (or 
                        (and is-single-step (= 1 number-of-steps))
                        (true? (some #(< % 0) to-coords))
                        (true? (some true? (map >= to-coords [height width])))
                        (= :teammate collaboration-type)
                        (true? opponent-found))
                    moves
                    (recur 
                        (vec (map + to-coords step))
                        (conj moves to-coords) 
                        (= :opponent collaboration-type) 
                        (inc number-of-steps)))))))
