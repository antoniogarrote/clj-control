(ns clj-control.utils)

(defn curry
  "supports partial application of a function for a certain number of
   arguments passed as an argument to thsi function"
  ([n f] (curry f n []))
  ([f n ac]
     (if (= n 0)
       (apply f ac)
       (fn [x] (curry f (dec n) (conj ac x))))))

(defn $
  "Function application"
  ([x] (fn [f] (apply f [x]))))
