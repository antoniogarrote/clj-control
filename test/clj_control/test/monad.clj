(ns clj-control.test.monad
  (:use [clj-control.functor] :reload)
  (:use [clj-control.applicative] :reload)
  (:use [clj-control.monad] :reload)
  (:use [clj-control.core] :reload)
  (:use [clj-control.utils] :reload)
  (:use [clojure.test]))

;; Sample test applicative functor
(defrecord TestMonad [v])

(defmethod make clj-control.test.monad.TestMonad
  ([t & args]
     (TestMonad. (first args))))

(extend-type clj-control.test.monad.TestMonad
  Functor
  (f-map [this f] (make TestMonad (apply f [(:v this)])))
  (pure [this v] (make TestMonad v)))

(extend-type clj-control.test.monad.TestMonad
  Applicative
  (af-* [this f]
        (let [fx (:v this)]
          (pure this (apply fx [(:v f)])))))

(extend-type clj-control.test.monad.TestMonad
  Monad
  (m->>= [this fx]
         (let [v (:v this)]
           (fx v)))
  (m->> [this m]
        (default-m->> this m)))

(defn from-test-monad
  ([f] (:v f)))

(defn get-constant-test-monad
  "A function that generates a function returning a test monad
   with a constant value"
  ([n] (fn [] (make TestMonad n))))

(deftest should-implement->>=
  (let [result (m->>= (make TestMonad 4) (fn [a]
                 (m->>= (make TestMonad (* a 2)) (fn [b]
                   (make TestMonad (-> b (* 2) (* a)))))))]
    (is (= 64 (from-test-monad result)))))


;(deftest should-implement->>
;  (let [monad-generator (get-constant-test-monad 5)
;        result (m->>= (monad-generator) (fn [a]
;                 (m->> (make TestMonad 6) ; This return value is not threaded
;                                  ; but the function do is invoked
;                   (m->>= (monad-generator) (fn [b]
;                     (make TestMonad (+ a b)))))))]
;    (is (= 10 (from-test-monad result)))))


;(deftest m-lift-should-be-supported
;  (let [ma (make TestMonad 2)
;        mb (make TestMonad 3)]
;    (is (= 5 (from-test-monad (m-lift + ma mb))))))
;
;(deftest m-apply-should-be-supported
;  (let [mfx (make TestMonad (partial inc))
;        mx (make TestMonad 3)]
;    (is (= 4 (m-apply mfx mx)))))
;
;(deftest m-sequence-sould-be-supported
;  (let [ms [(make TestMonad 0)
;            (make TestMonad 1)
;            (make TestMonad 2)
;            (make TestMonad 3)
;            (make TestMonad 4)]]
;    (is (= (list 0 1 2 3 4)
;           (from-test-monad (m-sequence ms))))))
;
;(deftest m-replicate-should-be-supported
;  (let [m (make TestMonad 1)]
;    (is (= (list 1 1 1)
;           (from-test-monad (m-replicate m 3))))))
;
;(deftest m-map-should-be-supported
;  (let [fx (fn [a] (make TestMonad a))
;        xs (list 1 2 3 4)]
;    (is (= xs
;           (from-test-monad (m-map fx xs))))))


;; Vectors

(deftest vecs-should-support-m->>=1
  (is (= [1 1 2 2 3 3]
         (m->>= [1 2 3] (fn [x] [x x])))))

(deftest vecs-should-support-m->>=2
  (is (= [1 2 1 2 2 3 2 3 3 4 3 4]
           (m->>=
            (m->>= [1 2 3] (fn [x] [x x]))
            (fn [x] [x (inc x)])))))

;; Lists

(deftest lists-should-support-m->>=1
  (is (= '(1 1 2 2 3 3)
         (m->>= '(1 2 3) (fn [x] (list x x))))))

(deftest lists-should-support-m->>=2
  (is (= '(1 2 1 2 2 3 2 3 3 4 3 4)
           (m->>=
            (m->>= '(1 2 3) (fn [x] (list x x)))
            (fn [x] (list x (inc x)))))))

;; Sets

(deftest sets-should-support-m->>=1
  (is (= #{1 2 3 4}
         (m->>= #{1 2 3} #(set [% (inc %)])))))

(deftest sets-should-support-m->>=2
  (is (= #{1 2 3 4 5 6 9 10 12 15 20}
         (m->>=
          (m->>= #{1 2 3}
                 #(set [% (inc %)]))
          #(set [% (* 3 %) (* 5 %)])))))

;; Functions

(deftest fns-should-support-m->>=1
  (is (= 19
         ((m->>= (partial * 2) (fn [a]
            (m->>= (partial + 10) (fn [b]
              (m-return clojure.lang.Fn (+ a b)))))) 3))))

(deftest fns-should-support-m->>=2
  (is (= [6 13 3/2]
         ((m->>= (partial * 2) (fn [a]
            (m->>= (partial + 10) (fn [b]
              (m->>= (fn [x] (/ x 2)) (fn [c]
                (m-return clojure.lang.Fn [a b c]))))))) 3))))
