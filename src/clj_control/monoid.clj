(ns clj-control.monoid)

(defprotocol Monoid
  "class Monoid a where
     mempty :: a
     mappend :: a -> a -> a
     mconcat :: [a] -> a
     mconcat = foldr mappend mempty"
  (m-append [this a]))

(defmulti m-empty (fn [t] (if (instance? java.lang.Class t)
                            t (class t))))

(defmulti m-concat (fn [ms] (class (first ms))))
