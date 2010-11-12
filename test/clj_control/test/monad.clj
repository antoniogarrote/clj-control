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


(deftest should-implement->>
  (let [monad-generator (get-constant-test-monad 5)
        result (m->>= (monad-generator) (fn [a]
                 (m->> (make TestMonad 6) ; This return value is not threaded
                                  ; but the function do is invoked
                   (m->>= (monad-generator) (fn [b]
                     (make TestMonad (+ a b)))))))]
    (is (= 10 (from-test-monad result)))))


(deftest m-lift-should-be-supported
  (let [ma (make TestMonad 2)]
    (is (= 3 (from-test-monad (m-lift ma inc))))))

(deftest m-apply-should-be-supported
  (let [mfx (make TestMonad inc)
        mx (make TestMonad 3)]
    (is (= 4 (from-test-monad (m-apply mfx mx))))))

(deftest m-sequence-sould-be-supported
  (let [ms [(make TestMonad 0)
            (make TestMonad 1)
            (make TestMonad 2)
            (make TestMonad 3)
            (make TestMonad 4)]]
    (is (= (list 0 1 2 3 4)
           (from-test-monad (m-sequence ms))))))

(deftest m-replicate-should-be-supported
  (let [m (make TestMonad 1)]
    (is (= (list 1 1 1)
           (from-test-monad (m-replicate m 3))))))

(deftest m-map-should-be-supported
  (let [fx (fn [a] (make TestMonad a))
        xs (list 1 2 3 4)]
    (is (= xs
           (from-test-monad (m-map fx xs))))))


;; Auxiliary test functions

(defn test-monad-law-1
  "return a >>= k = k a"
  ([k empty-m a]
     (is (= (m->>= (m-return empty-m a) k)
            (k a)))))

(defn test-monad-law-2
  "m >>= return = m"
  ([m empty-m]
     (is (= (m->>= m #(m-return empty-m %))
            m))))

(defn test-monad-law-3
  "m >>= (\\x -> k x >>= h) = (m >>= k) >>= h"
  ([m k h]
     (is (= (m->>= m (fn [x] (m->>= (k x) h)))
            (m->>= (m->>= m k) h)))))

(defn test-monad-law-4
  "fmap f xs = xs >>= return f = liftM f xs"
  ([f xs m-zero]
     (is (= (f-map xs f)
            (m->>= xs (fn [x] (m-return m-zero (f x))))))
     (is (= (f-map xs f)
            (m-lift xs f)))))


;; Vectors

(deftest vecs-should-support-m->>=1
  (is (= [1 1 2 2 3 3]
         (m->>= [1 2 3] (fn [x] [x x])))))

(deftest vecs-should-support-m->>=2
  (is (= [1 2 1 2 2 3 2 3 3 4 3 4]
           (m->>=
            (m->>= [1 2 3] (fn [x] [x x]))
            (fn [x] [x (inc x)])))))

(deftest vecs-law-1
  (let [k (fn [x] [x])
        a 1]
    (test-monad-law-1 k [] a)))

(deftest vecs-law-2
  (let [m [1]
        empty-m []]
    (test-monad-law-2 m empty-m)))

(deftest vecs-law-3
  (let [m [1]
        k (fn [l] (vec (list (inc l))))
        h (fn [l] (vec (list (* l 2))))]
    (test-monad-law-3 m k h)))

(deftest vecs-law-4
  (let [f inc
        xs [1]
        m-zero []]
    (test-monad-law-4 f xs m-zero)))


;; Lists

(deftest lists-should-support-m->>=1
  (is (= '(1 1 2 2 3 3)
         (m->>= '(1 2 3) (fn [x] (list x x))))))

(deftest lists-should-support-m->>=2
  (is (= '(1 2 1 2 2 3 2 3 3 4 3 4)
           (m->>=
            (m->>= '(1 2 3) (fn [x] (list x x)))
            (fn [x] (list x (inc x)))))))

(deftest lists-law-1
  (test-monad-law-1 list '(1) 1))

(deftest lists-law-2
  (let [m '(1)
        empty-m '(2)]
    (test-monad-law-2 m empty-m)))

(deftest lists-law-3
  (let [m [1]
        k (fn [l] (list (inc l)))
        h (fn [l] (list (* l 2)))]
    (test-monad-law-3 m k h)))

(deftest lists-law-4
  (let [f inc
        xs '(1)
        m-zero '(0)]
    (test-monad-law-4 f xs m-zero)))


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

(deftest sets-law-1
  (test-monad-law-1 #(set %) (set [1]) [1]))

(deftest sets-law-2
  (let [m (set [1])
        empty-m (set [])]
    (test-monad-law-2 m empty-m)))

(deftest sets-law-3
  (let [m (set [1])
        k (fn [l] (set (list (inc l))))
        h (fn [l] (set (list (* l 2))))]
    (test-monad-law-3 m k h)))

(deftest sets-law-4
  (let [f inc
        xs (set [1])
        m-zero (set [])]
    (test-monad-law-4 f xs m-zero)))


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

(deftest functions-law-1
  (let [k (fn [x] (fn [y] (+ x y)))
        a 1
        empty-m identity]
    (is (= (apply (m->>= (m-return empty-m a) k) [1])
           (apply (k a) [1])))))

(deftest functions-law-2
  (let [m inc
        empty-m identity]
    (is (= (apply (m->>= m #(m-return empty-m %)) [1])
           (apply m [1])))))

(deftest functions-law-3
  (let [m (fn [k] (identity k))
        k (fn [l] (fn [k] (+ l k)))
        h (fn [l] (fn [k] (* k l)))]
    (is (= (apply (m->>= m (fn [x] (m->>= (k x) h))) [1])
           (apply (m->>= (m->>= m k) h) [1])))))

(deftest functions-law-4
  (let [f inc
        xs (partial * 2)
        m-zero inc]
    (is (= (apply (f-map xs f) [1])
           (apply (m->>= xs (fn [x] (m-return m-zero (f x)))) [1])))
     (is (= (apply (f-map xs f) [1])
            (apply (m-lift xs f) [1])))))
