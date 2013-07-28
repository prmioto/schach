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
        
(defn create-empty-square [color]
    {:color color :piece nil})
    
(defn create-rank [rank width]
    (vec (take width 
        (map #(create-empty-square (if (even? %) :dark :light))
            (iterate inc (mod rank 2))))))

(defn create-board
  "Creates a empty square chess board of the specified width and height."
  [height width]
  (vec (map #(create-rank % width) (range 0 height))))

(defn create-piece [name color]
    {:name name :color color :number-of-moves 0})

(defmulti get-piece (fn [board position] (type position)))
(defmethod get-piece Sequential [board coords]
    (let [square (get-in board coords)]
        (:piece square)))
(defmethod get-piece String [board algebric-position]
    (let [coords (from-algebric-to-coords algebric-position)]
        (get-piece board coords)))

(defmulti put-piece (fn [board piece position] (type position)))
(defmethod put-piece Sequential [board piece coords]
    (let 
        [square (get-in board coords)
         new-square (assoc square :piece piece)]
        (println square new-square)
        (assoc-in board coords new-square)))
(defmethod put-piece String [board piece algebric-position]
    (let [coords (from-algebric-to-coords algebric-position)]
        (println algebric-position coords)
        (put-piece board piece coords)))

(defmulti remove-piece (fn [board position] (type position)))        
(defmethod remove-piece Sequential [board coords]
    (let 
        [square (get-in board coords)
         new-square (dissoc square :piece)]
        (assoc-in board coords new-square)))
(defmethod remove-piece String [board algebric-position]
    (let [coords (from-algebric-to-coords algebric-position)]
        (remove-piece board coords)))

(defn move-piece [board from-algebric-position to-algebric-position]
    (let 
        [from-coords (from-algebric-to-coords from-algebric-position)
         to-coords (from-algebric-to-coords to-algebric-position)
         piece (get-piece board from-coords)
         number-of-moves (:number-of-moves piece)
         updated-piece (assoc piece :number-of-moves (inc number-of-moves))]
        (put-piece (remove-piece board from-coords) updated-piece to-coords)))

(defn is-opponent? [piece-to-move piece-to-check]
    (not= (:color piece-to-move) (:color piece-to-check)))

(defn get-occupant-type [piece-to-move occuppant-piece] 
    (cond
        (nil? occuppant-piece) :empty-square
        (is-opponent? piece-to-move occuppant-piece) :opponent
        (true? true) :teammate))

(defn walk-by-steps [board piece-to-move from-coords step is-single-step]
    (let [height (count board) width (count (board 0))]
        (loop 
            [to-coords (vec (map + from-coords step)) 
             moves [] 
             opponent-found false 
             number-of-steps 0]
           (let 
                [occupant-piece (get-piece board to-coords)
                 occupant-type (get-occupant-type piece-to-move occupant-piece)]
                (println "walk-by-steps" to-coords moves opponent-found occupant-type number-of-steps)
                (if (or 
                        (and is-single-step (= 1 number-of-steps))
                        (true? (some #(< % 0) to-coords))
                        (true? (some true? (map >= to-coords [height width])))
                        (= :teammate occupant-type)
                        (true? opponent-found))
                    moves
                    (recur 
                        (vec (map + to-coords step))
                        (conj moves to-coords) 
                        (= :opponent occupant-type) 
                        (inc number-of-steps)))))))
