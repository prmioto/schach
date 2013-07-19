(ns com.schach.chess)

(defn char-to-int [char-value base-char]
    (- (int char-value) (int base-char)))

(defn from-algebric-to-coordinates [algebric-position]
    (let [file-char (first algebric-position) rank-char (last algebric-position)
        file-base-char \a rank-base-char \1]
        ;(println "x" file-char rank-char)
        [(char-to-int rank-char rank-base-char) (char-to-int file-char file-base-char)]))

(defn create-empty-square [color]
    {:color color :piece-name nil :piece-color nil})
    
(defn create-rank [index width]
    (vec (take width 
        (map #(create-empty-square (if (zero? (mod % 2)) :dark :light))
            (iterate inc (mod index 2))))))

(defn create-board
  "Creates a empty square chess board of the specified width and height."
  [width]
  (vec (map #(create-rank % width) (range 0 width))))
    
(def chess-piece-names
    {\K :king \Q :queen \R :rook \B :bishop \N :knight \P :pawn})
        
(def chess-piece-colors
    {\w :white \b :black})

(defn populate-board [board pieces]
    ;(println board pieces)
    (reduce (fn [board piece]
        ;(println "piece" piece board)
		(let [
            piece-color-symbol (first piece)
            piece-color (chess-piece-colors piece-color-symbol)
            piece-name-symbol (first (rest piece))
            piece-name (chess-piece-names piece-name-symbol)
            position (rest (rest piece))
            coordinates (from-algebric-to-coordinates position)
            piece (get-in board coordinates)
            new-piece (assoc 
                (dissoc piece :piece-name :piece-color) :piece-color piece-color :piece-name piece-name)]
            ;(println piece-color piece-name position coordinates piece new-piece)
            (assoc-in board coordinates new-piece)))
            board pieces))
 
(defn print-board [board]
    (println
        (reduce (fn [rank-str rank] 
            (str rank-str
                (reduce (fn [square-str square] 
                    (let [
                        square-color (square :color)
                        is-dark (= square-color :dark)
                        piece-name (square :piece-name)
                        piece-color (square :piece-color)
                        is-empty (nil? piece-color)
                        is-black (= piece-color :black)]
                        ;(println piece-name)
                        (str square-str 
                            (format "%s%s%3.3s%s%s" 
                                (if (true? is-dark) "[" "(") 
                                (if (true? is-empty) " " (if (true? is-black) "+" "_")) 
                                (if (nil? piece-name) "" piece-name)
                                (if (true? is-empty) " " (if (true? is-black) "+" "_")) 
                                (if (true? is-dark) "]" ")"))
                           "|"))) 
                    "" rank) "\n")) 
            "" (reverse board))))