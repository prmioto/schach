(ns com.schach.print-board
  (:require [com.schach.common :refer :all])
  (:require [com.schach.board :refer :all])
  (:require [com.schach.chess :refer :all]))


(defn print-board [board]
    (println
        (reduce 
            (fn [board-str rank] 
                (str board-str
                    (reduce (fn [rank-str square] 
                        (let 
                            [is-dark (= (:color square) :dark)
                             piece (:piece square)
                             is-empty (nil? piece)
                             is-black (= (:color piece) :black)
                             piece-name (if (true? is-empty) \space (first (name (:name piece))))
                             piece-representation (if (true? is-black) (partial to-upper) (partial to-lower))]
                            (str rank-str 
                                (format "%s %1s %s" 
                                    (if (true? is-dark) "[" " ") 
                                    (piece-representation piece-name)
                                    (if (true? is-dark) "]" " "))
                               "|")))    
                    "|" rank) "\n")) 
            "" (reverse board))))