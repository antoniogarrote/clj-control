(ns clj-control.monad
  (:use [clj-control.functor])
  (:use [clj-control.applicative]))

(defprotocol Monad
  "Class Applicative m => Monad m where
     (>>=) :: m a -> (a -> m b) -> m b
     (>>)  :: m a -> m b -> m b
   Monad laws:
     return a >>= k = k a
     m >>= return = m
     m >>= (\\x -> k x >>= h) = (m >>= k) >>= h
     fmap f xs = xs >>= return f = liftM f xs"
  (m->>= [this fx])
  (m->> [this m]))


(defn default-m->>
  "A default implementation for the (>>) function
   that can be used in the definition of new monads"
  ([this m] (m->>= this (fn [_] m))))

(defn m-return
  "Just a wrapper for the pure function in the
   pointed functor interface.
   It can receive a monadic class or a monad as the first argument.
   The second argument is the value to be returned from the monadic
   computation:
     m-return :: Monad m => m a -> b -> m b
   or :
     m-return :: JavaClass mc => mc -> b -> m b"
  ([m x] (if (class? m) (make m x) (pure m x))))

;; Combinators

(defn m-lift
  "m-lift :: Monad m =>  m a -> (a -> b) -> m b"
  ([m fx] (f-map m fx)))

(defn m-apply
  "m-apply :: Monad m => m (a -> b) -> m a -> m b"
  ([mfx m] (af-* mfx m)))

(defn m-sequence
  "m-sequence :: Monad m => [m a] -> m [a]"
  ([ms]
     (if (= 2 (count ms))
       (m->>= (first ms) (fn [x]
         (m->>= (second ms) (fn [y]
           (pure (first ms) (list x y))))))
       (m->>= (first ms) (fn [x]
         (m->>= (m-sequence (rest ms)) (fn [y]
           (pure (first ms) (cons x y)))))))))


(defn m-replicate
  "m-replicate :: Monad m => Int -> m a -> m [a]"
  ([m n]
     (m-sequence (replicate n m))))

(defn m-map
  "m-map :: Monad m => (a -> m b) -> [a] -> m [b]"
  ([fm xs]
     (m-sequence (map fm xs))))

;; Monad example of "do notation"

;(defn rand-maybe
;  ([] (let [tmp (Math/floor (rand 10))
;            _ (println (str "got " tmp))]
;        (if (> tmp 5)
;          (just tmp)
;          (nothing)))))
;
;(m->>= (rand-maybe) (fn [x]
;  (m->>= (rand-maybe) (fn [y]
;    (m->>= (rand-maybe) (fn [z]
;      (just (+ x y z))))))))
