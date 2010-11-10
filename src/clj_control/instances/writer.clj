(ns clj-control.instances.writer
  (:use [clj-control.functor]
        [clj-control.applicative]
        [clj-control.monad]))


(defrecord Writer [val log])

(defmethod make clj-control.instances.writer.Writer
  [t & args]
  (clj-control.instances.writer.Writer. (first args) (second args)))

;; API

(defn write [v l]
  (make clj-control.instances.writer.Writer v l))

(defn tell [l]
  (make clj-control.instances.writer.Writer nil l))

;; instance of Functor

(extend-type clj-control.instances.writer.Writer
  Functor
  (f-map [this f]
         (make Writer (f (:val this)) (:log this)))
  (pure [this v] (make Writer v "")))

;; instance of Applicative

(extend-type clj-control.instances.writer.Writer
  Applicative
  (af-* [this f]
        (make Writer
              (apply (:val this) [(:val f)])
              (str (:log this) (:log f)))))

;; instance of Monad

(extend-type clj-control.instances.writer.Writer
  Monad
  (m->>= [this fx]
         (let [new-writer (apply fx [(:val this)])]
           (make Writer (:val new-writer) (str (:log this) (:log new-writer)))))
  (m->> [this m]
        (m->> this (fn [_] m))))
