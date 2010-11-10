(ns clj-control.core
  (:use [clj-control.functor]
        [clj-control.applicative]
        [clj-control.monad]
        [clj-control.monoid]))

;; Functions

;; instance Functor ((->) r) where
;;     fmap f g = (\x -> f (g x))

(extend-type clojure.lang.Fn
  Functor
  (f-map [this fx]
         (fn [x] (fx (this x))))
  (pure [this fx] (fn [_] fx)))

;; instance Applicative ((->) r) where
;;     pure x = (\_ -> x)
;;     f <*> g = \x -> f x (g x)

;; Law
;; fmap g x = pure g <*> x

(extend-type clojure.lang.Fn
  Applicative
  (af-* [this f]
        (fn [x]
          (let [this-prime (this x)]
            (this-prime (f x))))))


(defmethod make clojure.lang.Fn
  ([t & args] (fn [_] (first args))))

(extend-type clojure.lang.Fn
  Monad
  (m->>= [this fx]
         (fn [x] ((fx (this x)) x)))
  (m->> [this m]
        (default-m->> this m)))


;; Vectors

(extend-type clojure.lang.PersistentVector
  Functor
  (f-map [this f] (vec (map f this)))
  (pure [this v] [v]))

(extend-type clojure.lang.PersistentVector
  Applicative
  (af-* [this f]
        (let [cycle-this (cycle this)]
          (vec (reduce (fn [a i]
                         (conj a (apply (nth cycle-this i) [(nth f i)])))
                       []
                       (range (count f)))))))

(extend-type clojure.lang.PersistentVector
  Monad
  (m->>= [this fx]
         (vec (apply concat
                (map fx this))))
  (m->> [this m]
        (vec (apply concat (repeat (count this) m)))))

(extend-type clojure.lang.PersistentVector
  Monoid
  (m-append [this a] (vec (concat this a))))

(defmethod m-empty clojure.lang.PersistentVector
  ([t] []))

(defmethod m-concat clojure.lang.PersistentVector
  ([ms] (reduce m-append ms)))


;; Lists

(extend-type clojure.lang.PersistentList
  Functor
  (f-map [this f] (map f this))
  (pure [this v] (list v)))

(extend-type (class '())
  Functor
  (f-map [this f] this)
  ; this is wrong
  (pure [this v] '()))

(extend-type clojure.lang.LazySeq
  Functor
  (f-map [this f] (map f this))
  (pure [this v] (list v)))

(defn- consume-lists-lazy
  ([c1 c2]
     (if (or (empty? c1) (empty? c2))
       '()
       (lazy-seq (cons (apply (first c1) [(first c2)])
                       (consume-lists-lazy (rest c1) (rest c2)))))))

(extend-type clojure.lang.PersistentList
  Applicative
  (af-* [this f]
        (consume-lists-lazy (cycle this) f )))

(extend-type clojure.lang.LazySeq
  Applicative
  (af-* [this f]
        (consume-lists-lazy (cycle this) f)))

(extend-type clojure.lang.PersistentList
  Monad
  (m->>= [this fx]
         (apply concat (map fx this)))
  (m->> [this m]
        (apply concat (repeat (count this) m))))

(extend-type (class '())
  Monad
  (m->>= [this fx]
         '())
  (m->> [this m]
        '()))

(extend-type clojure.lang.LazySeq
  Monad
  (m->>= [this fx]
         (apply concat (map fx this)))
  (m->> [this m]
        (apply concat (repeat (count this) m))))

(extend-type clojure.lang.PersistentList
  Monoid
  (m-append [this a] (concat this a)))

(defmethod m-empty clojure.lang.PersistentList
  ([t] '()))

(defmethod m-concat clojure.lang.PersistentList
  ([ms] (reduce m-append ms)))

(extend-type (class '())
  Monoid
  (m-append [this a] (concat this a)))

(defmethod m-empty (class '())
  ([t] '()))

(defmethod m-concat (class '())
  ([ms] (reduce m-append ms)))

(extend-type clojure.lang.LazySeq
  Monoid
  (m-append [this a] (concat this a)))

(defmethod m-empty clojure.lang.LazySeq
  ([t] '()))

(defmethod m-concat clojure.lang.LazySeq
  ([ms] (reduce m-append ms)))


;; Maps

(extend-type clojure.lang.PersistentArrayMap
  Functor
  (f-map [this f] (reduce (fn [m [k v]] (assoc m k v))
                         {}
                         (map (fn [[k v]] [k (f k v)]) this)))
  (pure [this v] {v v}))

;; (extend-type clojure.lang.PersistentArrayMap
;;   Applicative
;;   (af-* [this f]
;;        (reduce (fn [m [k ff]]
;;                  (assoc m k (apply ff [(get f k)])))
;;                {}
;;                this)))
;;
;; (extend-type clojure.lang.PersistentArrayMap
;;   Monad
;;   (m->>= [this fx]
;;          (merge this (fx this)))
;;   (m->> [this m]
;;         (do (merge this {}) m)))
;;
;; (extend-type clojure.lang.PersistentArrayMap
;;   Monoid
;;   (m-append [this a]
;;             (merge-with m-append this a)))
;;
;; (defmethod m-empty clojure.lang.PersistentArrayMap
;;   ([t] {}))
;;
;; (defmethod m-concat clojure.lang.PersistentArrayMap
;;   ([ms] (reduce m-append ms)))


;; Sets

(extend-type clojure.lang.PersistentHashSet
  Functor
  (f-map [this f] (set (map f this)))
  (pure [this v] (set (list v))))


(extend-type clojure.lang.PersistentTreeSet
  Functor
  (f-map [this f] (apply sorted-set (map f this)))
  (pure [this v] (sorted-set v)))


(extend-type clojure.lang.PersistentHashSet
  Applicative
  (af-* [this f] (set
                  (reduce (fn [ac fx]
                            (clojure.set/union ac (map fx f)))
                          #{}
                          this))))

(extend-type clojure.lang.PersistentHashSet
  Monad
  (m->>= [this fx]
         (apply clojure.set/union
                (map fx this)))
  (m->> [this m]
        (default-m->> this m)))

;; (extend-type clojure.lang.PersistentTreeSet
;;   Applicative
;;   (af-* [this f]
;;         (let [this-prime (cycle (seq this))
;;               f-prime    (seq f)]
;;           (apply sorted-set
;;            (distinct
;;             (reduce (fn [a i]
;;                       (conj a (apply (nth this-prime i) [(nth f-prime i)])))
;;                     '()
;;                     (range (count f-prime))))))))
;;         (apply sorted-set (map f this))))


;; Numbers and Strings

(extend-type java.lang.Integer
  Monoid
  (m-append [this a] (+ this a)))

(defmethod m-empty java.lang.Integer
  ([t] 0))

(defmethod m-concat java.lang.Float
  ([ms] (reduce m-append ms)))

(extend-type java.lang.Float
  Monoid
  (m-append [this a] (+ this a)))

(defmethod m-empty java.lang.Float
  ([t] 0))

(defmethod m-concat java.lang.Float
  ([ms] (reduce m-append ms)))

(extend-type java.lang.Double
  Monoid
  (m-append [this a] (+ this a)))

(defmethod m-empty java.lang.Double
  ([t] 0))

(defmethod m-concat java.lang.Double
  ([ms] (reduce m-append ms)))

(extend-type java.lang.String
  Monoid
  (m-append [this a] (str this a)))

(defmethod m-empty java.lang.String
  ([t] ""))

(defmethod m-concat java.lang.String
  ([ms] (reduce m-append ms)))


;; Atoms

(extend-type clojure.lang.Atom
  Functor
  (f-map [this f] (swap! this (fn [v] (f v))) this)
  (pure [this v] (atom v)))

(extend-type clojure.lang.Atom
  Applicative
  (af-* [this f]
        (swap! this (fn [fx] (fx @f))) this))


;; Refs

(extend-type clojure.lang.Ref
  Functor
  (f-map [this f] (alter this (fn [v] (f v))) this)
  (pure [this v] (ref v)))

(extend-type clojure.lang.Ref
  Applicative
  (af-* [this f]
        (alter this (fn [fx] (fx @f))) this))
