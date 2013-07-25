(ns com.schach.common-test
  (:require [clojure.test :refer :all]
            [com.schach.common :refer :all]
            [clojure.pprint :refer :all]))

(deftest test-char-to-int
    (testing "Conversion from char to integer."
        (is (= 0 (char-to-int \1 \1)))
        (is (= 7 (char-to-int \8 \1)))
        (is (= 0 (char-to-int \a \a)))
        (is (= 7 (char-to-int \h \a)))))

(deftest test-int-to-char
    (testing "Conversion from integer to char."
        (is (= \1 (int-to-char 0 \1)))
        (is (= \8 (int-to-char 7 \1)))
        (is (= \a (int-to-char 0 \a)))
        (is (= \h (int-to-char 7 \a)))))
