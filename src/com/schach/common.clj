(ns com.schach.common)

(use '[clojure.string :only (join split)])

(defn abs [x] (if (>= x 0) x (* x -1)))

(defn to-upper [c] (Character/toUpperCase c))
(defn to-lower [c] (Character/toLowerCase c))
(defn is-upper? [c] (Character/isUpperCase c))
(defn is-lower? [c] (Character/isLowerCase c))

(defn char-to-int [char-value base-char]
    (- (int char-value) (int base-char)))

(defn int-to-char [int-value base-char]
    (char (+ int-value (int base-char))))
