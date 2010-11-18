(ns clj-control.test.category
  (:use [clj-control.category] :reload)
  (:use [clj-control.core] :reload)
  (:use [clj-control.utils] :reload)
  (:use [clojure.test]))


(deftest functions-should-work-with-the->>>-combinator
  (is (= ((>>> inc inc (partial * 2) inc inc inc (partial * 3)) 3)
         39)))

(deftest functions-should-work-with-the-<<<-combinator
  (is (= ((<<< inc inc (partial * 2) inc inc inc (partial * 3)) 3)
         26)))
