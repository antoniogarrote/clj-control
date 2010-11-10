(ns clj-control.applicative
  (:use [clj-control.functor]
        [clj-control.utils]))

(defprotocol Applicative
  "class Pointed f => Applicative f where
     (af-*) :: f (a -> b) -> f a -> f b
   laws:
      pure id <*> v = v                            -- Identity
      pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
      pure f <*> pure x = pure (f x)               -- Homomorphism
      u <*> pure y = pure ($ y) <*> u              -- Interchange"
  (af-* [this f]))


(defn af-liftn
  ([acum fs]
     (let [acum-prime (af-* acum (first fs))]
       (if (empty? (rest fs))
         acum-prime
         (recur acum-prime (rest fs)))))
  ([n fx & fs]
     (let [currified (curry n fx)]
       (af-liftn (f-map (first fs) currified) (rest fs)))))

(defn af-lift
  "liftAn :: Applicative f => ([a b ... m] -> n) -> f a -> f b -> ... -> f m -> f n
   Lift a n-ary function to actions."
  ([fx & fs]
     (apply af-liftn (concat [(count fs) fx] fs))))

(defn af-**
  "(<**>) :: Applicative f => f a -> f (a -> b) -> f b
   A variant of af-* with the arguments reversed."
  ([fa fb] (af-* fb fa)))
