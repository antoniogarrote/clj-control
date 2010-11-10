(ns clj-control.instances.continuation
  (:use [clj-control.functor]
        [clj-control.applicative]
        [clj-control.monad]
        [clj-control.monoid]))

(defrecord Cont [k])

(defmethod make clj-control.instances.continuation.Cont
  ([t & args] (Cont. (fn [k] (k (first args))))))

;; newtype Cont r a = Cont { runCont :: (a -> r) -> r }

(defn run-cont
  ([c fx]
     ((:k c) fx)))


;; instance of Functor

(extend-type clj-control.instances.continuation.Cont
  Functor
  (f-map [this f]
         (run-cont this (fn [a] (make Cont (f a)))))
  (pure [this v] (make Cont v)))

;; instance of Applicative

(extend-type clj-control.instances.continuation.Cont
  Applicative
  (af-* [this f]
        (run-cont this (fn [fa]
                         (run-cont f (fn [b]
                                       (make Cont (fa b))))))))

;; instance of Monad

(extend-type clj-control.instances.continuation.Cont
  Monad
  (m->>= [this fx]
         (Cont. (fn [k]
                  (run-cont this
                            (fn [a]
                              (run-cont (fx a) k))))))
  (m->> [this m]
        (default-m->> this m)))


;; call-cc
;; callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k

(defn call-cc
  ([fx]
     (Cont. (fn [k]
              (run-cont (fx (fn [a](Cont. (fn [_]  (k a)))))
                        k)))))

;;; Tests & examples

(defn square-cc
  ([n] (make clj-control.instances.continuation.Cont (Math/pow n 2))))

(defn add-three-cc
  ([n] (make  clj-control.instances.continuation.Cont (+ n 3))))

