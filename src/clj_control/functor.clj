(ns clj-control.functor)

(defprotocol Functor
  "class Functor f where
    f-map :: (a -> b) -> f a -> f b
   properties:
    fmap id = id
    fmap (p . q) = (fmap p) . (fmap q)"
  (f-map [this f])
  ; Pointed Functors actually
  (pure [this v]))


;; Generic interface for building the types
;; in the library

(defmulti make (fn [t & args] t))
