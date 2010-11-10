(ns clj-control.test.utils
  (:use [clj-control.utils]
        [clojure.test]))

(deftest should-apply-the-function-after-partial-application-of-n-args
  (let [fx (curry 3 +)
        fx1 (apply fx [1])
        fx2 (apply fx1 [2])
        v (apply fx2 [3])]
    (is (= 6 v))
    (is (fn? fx))
    (is (fn? fx1))
    (is (fn? fx2))))

(deftest should-apply-function-application
  (is (= (map ($ 3) [(partial + 4)
                     (partial * 10)
                     (fn [x] (Math/pow x 2))
                     (fn [x] (Math/sqrt x))])
         (list 7 30 9.0 1.7320508075688772))))
