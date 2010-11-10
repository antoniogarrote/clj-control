(ns clj-control.test.instances.continuation
  (:use [clj-control.functor] :reload)
  (:use [clj-control.applicative] :reload)
  (:use [clj-control.monad] :reload)
  (:use [clj-control.core] :reload)
  (:use [clj-control.utils] :reload)
  (:use [clj-control.instances.continuation] :reload)
  (:use [clojure.test]))

;;; Tests & examples

(defn square
  ([n] (make clj-control.instances.continuation.Cont (Math/pow n 2))))

(defn add-three
  ([n] (make  clj-control.instances.continuation.Cont (+ n 3))))



(deftest it-should-run-continuations
  (is (= 10
         (run-cont (square 3) inc)))
  (is (= "6"
         (run-cont (add-three 3) str))))

(deftest it-should-bind-continuations
  (is (= "12.0"
       (run-cont (m->>= (square 3) add-three) str))))

(deftest call-cc-should-be-implemented
  (is (= "5"
       (run-cont
        (call-cc (fn [k] (let [n 5]
                           (m->>= (k 5) (fn [_]
                             (m-return clj-control.instances.continuation.Cont 25))))))
        str))))
