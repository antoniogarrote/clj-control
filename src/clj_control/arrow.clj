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


(defmacro parse-cmd
  ([cmd pat type rest-cmds]
     (println (str "cmd vale " cmd))
     (if (= 3 (count cmd))
         ;; arr -< exp
       (let [a (first cmd)
             e (last cmd)
             _ (println (str "a " a " e " e " pat " pat " ::: " (count rest-cmds)))]
         (if (empty? rest-cmds)
           `(>>> (arr ~type (fn ~pat ~e))
                 ~a)
           `(>>> (arr ~type
                      (fn ~pat (>>> ~e
                                   (parse-cmd ~(first rest-cmds)
                                              [ ~(vec (flatten  (list (first pat) a))) ]
                                              ~type ~(rest rest-cmds)))))
                 ~a)))
       (if (= "<-" (str (second cmd)))
         `(>>> (a-&&& (arr ~type identity)
                      (proc-a-exp ~type ~pat :at [~(vec (drop 2 cmd))]))
               (proc-a-exp ~type [ ~(vec (flatten  (list (first (flatten pat)) (first cmd)))) ] :at ~rest-cmds))
         (throw (Exception. (str "unknown command " cmd)))))))

;;(proc-a clojur.lang.Fn
;;        [x y] ->
;;        [a <- (p * 2) -< (+ x y)])
(defmacro proc-a-exp
  ([type pat _ cmds]
     (let [f-cmd (first cmds)
           r-cmds (rest cmds)
           _ (println (str "cmds vale " cmds))]
       `(parse-cmd ~f-cmd ~pat ~type ~r-cmds))))

(defmacro proc-a [type pat _ & cmds]
  `(proc-a-exp ~type ~pat :a ~cmds))
