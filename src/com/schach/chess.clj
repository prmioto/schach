(ns com.schach.chess)

(defn abs [x] (if (>= x 0) x (* x -1)))

(defn char-to-int [char-value base-char]
    (- (int char-value) (int base-char)))

(defn from-algebric-to-coordinates [algebric-position]
    (let [file-char (first algebric-position) rank-char (last algebric-position)
        file-base-char \a rank-base-char \1]
        ;(println "x" file-char rank-char)
        [(char-to-int rank-char rank-base-char) (char-to-int file-char file-base-char)]))

(defn create-empty-square [color]
    {:color color :piece nil})
    
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

(defn create-piece [name color]
    {:name name :color color :has-moved false})
    
(defn replace-piece [board coordinates new-piece]
    (let [square (get-in board coordinates)]
        (assoc-in board coordinates (assoc (dissoc square :piece) :piece new-piece))))

(defn populate-board [board pieces]
    ;(println board pieces)
    (reduce (fn [board piece-str]
        ;(println "piece" piece-str)
		(let [
            color-symbol (first piece-str)
            color (chess-piece-colors color-symbol)
            name-symbol (first (rest piece-str))
            name (chess-piece-names name-symbol)
            algebric-position (rest (rest piece-str))
            coordinates (from-algebric-to-coordinates algebric-position)
            new-piece (create-piece name color)]
            ;(println color name algebric-position coordinates new-piece)
            (replace-piece board coordinates new-piece)))
            board pieces))

(defn setup-chess-board []
    (let [
        chess-width 8 
        board (create-board chess-width)]
        (populate-board board 
            ["wRa1" "wNb1" "wBc1" "wQd1" "wKe1" "wBf1" "wNg1" "wRh1"
             "wPa2" "wPb2" "wPc2" "wPd2" "wPe2" "wPf2" "wPg2" "wPh2"
             "bRa8" "bNb8" "bBc8" "bQd8" "bKe8" "bBf8" "bNg8" "bRh8"
             "bPa7" "bPb7" "bPc7" "bPd7" "bPe7" "bPf7" "bPg7" "bPh7"])))
 
(defn move-piece-by-coordinates [board from-coordinates to-coordinates]
    (let [
        from-square (get-in board from-coordinates)
        to-square (get-in board to-coordinates)
        piece (from-square :piece)]
        (assoc-in (assoc-in board from-coordinates (dissoc from-square :piece)) to-coordinates (assoc to-square :piece piece))))
        
(defn move-piece [board from-algebric-position to-algebric-position]
     (let [
        from-coordinates (from-algebric-to-coordinates from-algebric-position)
        to-coordinates (from-algebric-to-coordinates to-algebric-position)]
     (move-piece-by-coordinates board from-coordinates to-coordinates)))

(defmulti get-possible-moves (fn [piece coordinates width] (:name piece)))
(defmethod get-possible-moves :king [piece coordinates width]
    (let [[x y] coordinates]
        (filter (fn [[x y]] (and (>= x 0) (>= y 0) (< x width) (< y width)))
            (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] 
                [(+ dx x) (+ dy y)]))))
(defmethod get-possible-moves :queen [piece coordinates width]
    (let [[x y] coordinates]
        (reduce concat
            (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
                (loop [x (+ dx x) y (+ dy y) coll []]
                    (if (or (< x 0) (< y 0) (>= x width) (>= y width))
                        coll
                        (recur (+ dx x) (+ dy y) (conj coll [x y]))))))))                
(defmethod get-possible-moves :bishop [piece coordinates width]
    (let [[x y] coordinates]
        (reduce concat
            (for [dx [-1 0 1] dy [-1 0 1] :when (and (not= 0 dx) (not= 0 dy))]
                (loop [x (+ dx x) y (+ dy y) coll []]
                    (if (or (< x 0) (< y 0) (>= x width) (>= y width))
                        coll
                        (recur (+ dx x) (+ dy y) (conj coll [x y]))))))))    
(defmethod get-possible-moves :rook [piece coordinates width]
    (let [[x y] coordinates]
        (reduce concat
            (for [dx [-1 0 1] dy [-1 0 1] :when (and (not= 0 dx dy) (or (= 0 dx) (= 0 dy)))]
                (loop [x (+ dx x) y (+ dy y) coll []]
                    (if (or (< x 0) (< y 0) (>= x width) (>= y width))
                        coll
                        (recur (+ dx x) (+ dy y) (conj coll [x y]))))))))
(defmethod get-possible-moves :pawn [piece coordinates width]
    (let [[x y] coordinates]
        (filter (fn [[x y]] (and (>= x 0) (>= y 0) (< x width) (< y width)))
            (conj
                (for [dx [-1 0 1]] 
                    [(+ dx x) (+ 1 y)])
                [x (+ 2 y)]))))
(defmethod get-possible-moves :knight [piece coordinates width]
    (let [[x y] coordinates]
        (filter (fn [[x y]] (and (>= x 0) (>= y 0) (< x width) (< y width)))
            (for [dx [-2 -1 1 2] dy [-2 -1 1 2] :when (not= (abs dx) (abs dy))]
                    [(+ dx x) (+ dy y)]))))
                
(defn print-board [board]
    (println
        (reduce (fn [rank-str rank] 
            (str rank-str
                (reduce (fn [square-str square] 
                    (let [
                        square-color (square :colord)
                        is-dark (= square-color :dark)
                        piece-name (-> square :piece :name)
                        piece-color (-> square :piece :color)
                        is-empty (nil? piece-color)
                        is-black (= piece-color :black)]
                        ;(println (-> square :piece :name)
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