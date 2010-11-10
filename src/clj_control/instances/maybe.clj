(ns clj-control.instances.maybe
  (:use [clj-control.functor]
        [clj-control.applicative]
        [clj-control.monad]
        [clj-control.monoid]))

;; Type definition

(defrecord Maybe [kind val])

;; API

(defn from-just [this]
            (if (= (:kind this) :just) (:val this)
                (throw
                 (Exception.
                  "Cannot invoke from-just if kind is nothing"))))

(defn just? [this] (= (:kind this) :just))

(defn nothing? [this] (= (:kind this) :nothing))

(defn maybe [this default fx]
  (if (just? (:kind this))
    (make Maybe :just (fx val)) default))

(defn from-maybe [this default]
  (if (= (:kind this) :just)
    (from-just (:kind this)) default))

;; Specialized type constructors

(defn just [x]
  (make Maybe :just x))

(defn nothing []
  (make Maybe :nothing))

;; instance of Functor

(extend-type clj-control.instances.maybe.Maybe
  Functor
  (f-map [this f]
         (if (just? this)
           (make Maybe :just (f (from-just this)))
           (make Maybe :nothing)))
  (pure [this v] (make Maybe :just v)))

;; instance of Applicative

(extend-type clj-control.instances.maybe.Maybe
  Applicative
  (af-* [this f]
        (if (just? this)
          (f-map f (from-just this))
          this)))

;; instance of Monad

(extend-type clj-control.instances.maybe.Maybe
  Monad
  (m->>= [this fx]
         (if (nothing? this)
           this
           (fx (from-just this))))
  (m->> [this m]
        (if (nothing? this)
          this
          m)))

;; instance of Monoid

(extend-type clj-control.instances.maybe.Maybe
  Monoid
  (m-append [this a]
            (if (and (nothing? this)
                     (just? a)) a
            (if (and (just? this)
                     (nothing? a)) this
            (make Maybe :just (m-append (from-just this) (from-just a)))))))

(defmethod m-empty clj-control.instances.maybe.Maybe
  ([t] (make Maybe :nothing nil)))

(defmethod m-concat clj-control.instances.maybe.Maybe
  ([ms] (reduce m-append ms)))


;; First newtype for Maybe

; (defrecord First [kind value])
;
; (extend First Maybe)
;
; (defmethod make First
;   ([t & args] (condp = (first args)
;                   :just (clj-control.instances.maybe.First. :just args)
;                   :nothing (clj-control.instances.maybe.First. :nothing nil)
;                   (throw (Exception. (str "Cannot build First type with kind " (first args)))))))
;
; (extend-type First
;   Monoid
;   (m-append [this a]
;             (if (and (nothing? this)
;                      (just? a)) a
;             (if (and (just? this)
;                      (nothing? a)) this
;             this))))
;
; (defmethod m-empty clj-control.instances.maybe.First
;   ([t] (make Maybe :nothing nil)))
;
; (defmethod m-concat clj-control.instances.maybe.First
;   ([ms] (reduce m-append ms)))

;; Type constructor

(defmethod make clj-control.instances.maybe.Maybe
  ([t & args]
     (let [kind (first args)
           maybe-args (rest args)]
       (condp = kind
           :just (clj-control.instances.maybe.Maybe. :just (first maybe-args))
           :nothing (clj-control.instances.maybe.Maybe. :nothing nil)
           (throw (Exception. "Wrong arguments for monad constructor"))))))


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
