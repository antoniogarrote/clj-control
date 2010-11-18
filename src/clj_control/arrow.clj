(ns clj-control.arrow
  (:use [clj-control.category])
  (:use [clj-control.utils]))

(defprotocol Arrow
  (a-first [this])
  (a-second [this])
  (a-*** [this a])
  (a-&&& [this a])
  ;; Choice
  (a-true [this])
  (a-false [this])
  (a-and [this a])
  (a-or [this a]))

(defmulti arr (fn [t & args] t))

;; map-a

(defn list-case
  "listcase []	= Left ()
   listcase (x:xs) = Right (x,xs)"
  ([xs] (if (empty? xs)
          (list false '())
          (list true (list (first xs) (rest xs))))))

;; (defn map-a
;;   "mapA f = arr listcase >>>
;;             arr (const []) ||| (f *** mapA f >>> arr (uncurry (:)))"
;;   ([f] (>>> (arr (class f) list-case)
;;                 (a-or (arr (class f) (fn [_] '()))
;;                       (>>> (a-*** f (map-a f))
;;                              (arr (class f) (pa cons)))))))

