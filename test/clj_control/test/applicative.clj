(ns clj-control.test.applicative
  (:use [clj-control.functor] :reload)
  (:use [clj-control.applicative] :reload)
  (:use [clj-control.core] :reload)
  (:use [clj-control.utils] :reload)
  (:use [clojure.test]))

;; Sample test applicative functor
(defrecord TestApplicativeFunctor [v])

(defmethod make clj-control.test.applicative.TestApplicativeFunctor
  ([t & args]
     (TestApplicativeFunctor. (first args))))

(extend-type clj-control.test.applicative.TestApplicativeFunctor
  Functor
  (f-map [this f] (make TestApplicativeFunctor (apply f [(:v this)])))
  (pure [this v] (make TestApplicativeFunctor v)))

(extend-type clj-control.test.applicative.TestApplicativeFunctor
  Applicative
  (af-* [this f]
        (let [fx (:v this)]
          (pure this (apply fx [(:v f)])))))

(defn from-test-functor
  ([f] (:v f)))


;; Tests

(deftest applicative-functors-should-implement-af-*
  (let [ffx (make TestApplicativeFunctor (curry 2 +))
        fa (make TestApplicativeFunctor 1)
        fb (make TestApplicativeFunctor 2)]
    (is (= 3 (from-test-functor (-> ffx (af-* fa) (af-* fb)))))))

;; Laws

(deftest test-identity
  (let [ffx (make TestApplicativeFunctor identity)
        f (make TestApplicativeFunctor 1)]
    (is (= (from-test-functor f)
           (from-test-functor (af-* ffx f))))))

(deftest test-composition
  (let [ffxid (make TestApplicativeFunctor (curry 2 comp))
        ffxu (make TestApplicativeFunctor inc)
        ffxv (make TestApplicativeFunctor inc)
        ffxw (make TestApplicativeFunctor 1)]
    (is (= (-> ffxid (af-* ffxu) (af-* ffxv) (af-* ffxw))
           (-> ffxu (af-* (af-* ffxv ffxw)))))
    (is (= (from-test-functor (-> ffxid (af-* ffxu) (af-* ffxv) (af-* ffxw)))
           3))))

(defn transpose-vec
  ([matrix]
     (if (empty? matrix)
       (repeat '[])
       (let [xs (first matrix)
             xss (vec (rest matrix))
             fst (af-* (vec (repeat (count xs) (fn[x] (fn [y] (vec (cons x y))))))
                       xs)
             snd (af-*
                  fst
                  (vec (take (count xs) (transpose-vec xss))))]
         snd))))

;; (def *res*  (transpose-vec [[1 2 3] [4 5 6] [7 8 9]]))
;; (println (str "res is " *res*))

(defn transpose
  ([matrix]
     (if (empty? matrix)
       (repeat '())
       (let [xs (first matrix)
             xss (rest matrix)]
;;         (-> (repeat (count xs) (curry 2 cons))
;;             (af-* xs)
;;             (af-* (take (count xs) (transpose xss))))))))
         (af-*
          (af-* (repeat (count xs) (curry 2 cons)) xs)
          (take (count xs) (transpose xss)))))))

;; (def *res*  (transpose '((1 2) (3 4) (5 6))))
;; (doseq [r *res*]
;;   (print "[")
;;   (doseq [c r]
;;     (print (str " " c) ))
;;   (println " ]"))


(deftest test-homomorphism
  (let [fx inc
        x  1
        ffx (make TestApplicativeFunctor fx)
        f  (make TestApplicativeFunctor x)]
    (is (= (af-* ffx f)
           (make TestApplicativeFunctor (fx x))))))

(deftest test-interchange
  (let [fx inc
        x  1
        fu (make TestApplicativeFunctor fx)]
    (is (= (af-* fu (pure fu x))
           (af-* (pure fu ($ x)) fu)))))

;; Functions

;; fmap (\x -> x + 1) (*2) $ 3
(deftest functions-should-be-applicatives-0
  (is (= 7
         ((f-map (partial * 2) inc) 3))))

;; fmap g x = pure g <*> x
(deftest fmap-funcs-should-law1
  (is (= ((f-map (partial * 2) inc) 3)
         ((af-* (pure identity inc) (partial * 2)) 3))))

;; ghci> fmap (*3) (+100) 1
;; 303
(deftest functions-should-be-applicatives-1
  (is (= 303
         ((f-map (partial  + 100) (partial * 3)) 1))))

(deftest functions-should-be-applicatives-1
  (is (= ((af-* (pure identity (partial + 1)) (partial + 2)) 4)
         7)))

;; (<$>) :: (Functor f) => (a -> b) -> f a -> f b
;; f <$> x = fmap f x = pure f <*> x

;; ghci> :t (+) <$> (+3) <*> (*100)
;; (+) <$> (+3) <*> (*100) :: (Num a) => a -> a
;; ghci> (+) <$> (+3) <*> (*100) $ 5
;; 508

(deftest functions-should-be-applicatives-3
  (is (= 508
         (apply (-> (pure identity (curry 2 +)) (af-* (partial + 3)) (af-* (partial * 100))) [5]))))

;; ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
;; [8.0,10.0,2.5]

(deftest functions-should-be-applicatives-4
  (is (= '(8 10 5/2)
         (apply (-> (pure identity (curry 3 list)) (af-* (partial + 3)) (af-* (partial * 2)) (af-* (fn [x] (/ x 2)))) [5]))))

;; pure id <*> u = u
(deftest test-identity-functions
  (let [fidentity (make clojure.lang.Fn identity)
        f   (partial + 1)]
    (is (= ((af-* fidentity f) 4)
           (f 4)))))

;; Vectors

;; fmap g x = pure g <*> x
(deftest fmap-vectors-should-law1
  (is (= (f-map [1 2 3] inc)
         (af-* (pure [] inc) [1 2 3]))))

;; pure f <*> pure x = pure (f x)
(deftest fmap-vectors-should-law3
  (is (= (af-* (pure [] inc) (pure [] 1))
         (pure [] (inc 1)))))

;; u <*> pure y = pure ($ y) <*> u
(deftest fmap-vectors-should-law4
  (is (= (af-* [inc] (pure [] 1))
         (af-* (pure [] ($ 1)) [inc]))))

;; Lists

;; fmap g x = pure g <*> x
(deftest fmap-lists-should-law1
  (is (= (f-map '(1 2 3) inc)
         (af-* (pure '(1) inc) '(1 2 3)))))

;; pure f <*> pure x = pure (f x)
(deftest fmap-lists-should-law3
  (is (= (af-* (pure '(1) inc) (pure '(1) 1))
         (pure '(1) (inc 1)))))

;; u <*> pure y = pure ($ y) <*> u
(deftest fmap-lists-should-law4
  (is (= (af-* (list inc) (pure '(1) 1))
         (af-* (pure '(1) ($ 1)) (list inc)))))


;; Sets

;; fmap g x = pure g <*> x
(deftest fmap-sets-should-law1
  (is (= (f-map #{1 3 5} inc)
         (af-* (pure #{} inc) #{1 3 5}))))

;; pure f <*> pure x = pure (f x)
(deftest fmap-lists-should-law3
  (is (= (af-* (pure #{} inc) (pure #{} 1))
         (pure #{} (inc 1)))))

;; u <*> pure y = pure ($ y) <*> u
(deftest fmap-lists-should-law4
  (is (= (af-* #{inc} (pure #{} 1))
         (af-* (pure #{} ($ 1)) #{ inc }))))

;; Atoms

;; fmap g x = pure g <*> x
(deftest fmap-atoms-should-law1
  (is (= @(f-map (atom 1) inc)
         @(af-* (pure (atom 1) inc) (atom 1)))))

;; pure f <*> pure x = pure (f x)
(deftest fmap-atoms-should-law3
  (is (= @(af-* (pure (atom 1) inc) (pure (atom 1) 1))
         @(pure (atom 1) (inc 1)))))

;; u <*> pure y = pure ($ y) <*> u
(deftest fmap-atoms-should-law4
  (is (= @(af-* (atom inc) (pure (atom 1) 1))
         @(af-* (pure (atom 1) ($ 1)) (atom inc)))))

(deftest atoms-should-conserve-the-same-id
  (let [fa (atom inc)
        fb (atom 1)]
    (af-* fa fb)
    (is (= 2 @fa))))

;; Refs

;; fmap g x = pure g <*> x
(deftest fmap-refs-should-law1
  (dosync (is (= @(f-map (ref 1) inc)
                 @(af-* (pure (ref 1) inc) (ref 1))))))

;; pure f <*> pure x = pure (f x)
(deftest fmap-refs-should-law3
  (dosync (is (= @(af-* (pure (ref 1) inc) (pure (ref 1) 1))
                 @(pure (ref 1) (inc 1))))))

;; u <*> pure y = pure ($ y) <*> u
(deftest fmap-refs-should-law4
  (dosync (is (= @(af-* (ref inc) (pure (ref 1) 1))
                 @(af-* (pure (ref 1) ($ 1)) (ref inc))))))
