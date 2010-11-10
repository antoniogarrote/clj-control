(ns clj-control.test.functor
  (:use [clj-control.functor] :reload)
  (:use [clj-control.core] :reload)
  (:use [clojure.test]))

;; Sample test functor
(defrecord TestFunctor [v])

(defmethod make clj-control.test.functor.TestFunctor
  ([t & args]
     (TestFunctor. (first args))))

(extend-type clj-control.test.functor.TestFunctor
  Functor
  (f-map [this f] (make TestFunctor (apply f [(:v this)])))
  (pure [this v] (make TestFunctor v)))

(defn from-test-functor
  ([f] (:v f)))

;; Tests
(deftest should-apply-f-map-functions
  (let [f (make TestFunctor 2)
        f-prime (f-map f (partial * 2))]
    (is (= 4 (from-test-functor f-prime)))))

(deftest pure-should-wrap-the-element
  (let [fa (make TestFunctor 2)
        fb (pure fa 3)]
    (is (= TestFunctor (class fb)))
    (is (= 3 (from-test-functor fb)))))

;; Properties

; fmap id = id
(deftest fmap-identity-property
  (let [f (make TestFunctor 1)]
    (is (= (from-test-functor (identity f))
           (from-test-functor (f-map f identity))))))

; fmap (p . q) = (fmap p) . (fmap q)
(deftest composition-fmap-fs
  (let [fx1 inc
        fx2 (partial + 2)
        fx1-fx2 (comp fx1 fx2)
        f (make TestFunctor 1)]
    (is (= (from-test-functor (f-map f fx1-fx2))
           (from-test-functor (-> f (f-map fx1) (f-map fx2)))))))

;; Vectors

(deftest vectors-should-apply-f-map
  (let [f [1 2 3 4 5]
        f-prime (f-map f inc)]
    (doseq [i (range 0 4)]
      (is (= (inc (nth f i))
             (nth f-prime i))))
    (is (= (class f) (class f-prime)))))

(deftest vectors-should-implement-pure
  (let [fa []
        fb (pure fa 1)]
    (is (= [1] fb))))


;; Lists

(deftest lists-should-apply-f-map
  (let [f (1 2 3 4 5)
        f-prime (f-map f inc)]
    (doseq [i (range 0 4)]
      (is (= (inc (nth f i))
             (nth f-prime i))))
    (is (= (class f) (class f-prime)))))

(deftest lists-should-implement-pure
  (let [fa '(1 2 3 4)
        fb (pure fa 1)]
    (is (= '(1) fb))))

(deftest lists-should-implement-pure
  (let [fa '()
        fb (pure fa 1)]
    (is (= '(1) fb))))

(deftest lists-should-apply-f-map
  (let [f (map identity '(1 2 3 4 5))
        f-prime (f-map f inc)]
    (is (instance? clojure.lang.LazySeq f))
    (doseq [i (range 0 4)]
      (is (= (inc (nth f i))
             (nth f-prime i))))
    (is (= (class f) (class f-prime)))))

(deftest lists-should-implement-pure
  (let [fa (map identity '(1 2 3 4))
        fb (pure fa 1)]
    (is (= '(1) fb))))


;; Maps

(deftest maps-should-implement-f-map
  (let [f {:a 1 :b 2}
        f-prime (f-map f (fn [k v] (inc v)))]
    (doseq [k (keys f)]
      (is (= (inc (k f)) (k f-prime))))
    (is (= (class f) (class f-prime)))))

(deftest maps-should-implement-pure
  (let [fa {:a 1 :b 2}
        fb (pure fa 3)]
    (is (= {3 3} fb))))

;; Functions

(deftest should-be-equivalent-to-function-composition
  (is (= 303
         (apply (f-map (partial + 100) (partial * 3)) [1])))
  (is (= "300"
         ((f-map (partial * 100) (comp str (partial * 3))) 1))))

;; Atoms

(deftest atoms-should-implement-f-map
  (let [f (atom 1)
        f-prime (f-map f (fn [v] (inc v)))]
    (is (= (inc 1) @f-prime))
    (is (= (class f) (class f-prime)))))

(deftest atoms-should-implement-pure
  (let [fa (atom 3)
        fb (pure fa 3)]
    (is (= @fa @fb))))
