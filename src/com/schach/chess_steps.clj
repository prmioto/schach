(ns com.schach.chess-steps
  (:require [com.schach.common :refer :all]))

(defn get-king-steps []
    (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] [dy dx]))

(defn get-queen-steps []
    (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] [dy dx]))
        
(defn get-rook-steps []
    (for [dx [-1 0 1] dy [-1 0 1] :when (and (not= 0 dx dy) (or (= 0 dx) (= 0 dy)))] [dy dx]))

(defn get-bishop-steps []
    (for [dx [-1 0 1] dy [-1 0 1] :when (and (not= 0 dx) (not= 0 dy))] [dy dx]))

(defn get-knight-steps []
    (for [dx [-2 -1 1 2] dy [-2 -1 1 2] :when (not= (abs dx) (abs dy))] [dy dx]))

(defn get-pawn-steps []
    (for [dx [-1 0 1] dy [1 2]] [dy dx]))

