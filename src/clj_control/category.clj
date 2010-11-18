(ns clj-control.category)

(defprotocol Category
  "class Category cat where
     id ::cat a a
     (.) :: cat b c -> cat a b -> cat a c"
  (c-id [this])
  (c-comp [this c]))


(defn >>>
  "(>>>) :: Category cat => cat a b -> cat b c -> cat a c
   Left-to-right composition "
  ([cab cbc]
     (c-comp cab cbc))
  ([cab cbc & args]
     (if (empty? args)
       (c-comp cab cbc)
       (apply >>> (cons (c-comp cab cbc) args)))))


(defn <<<
  "(<<<) :: Category cat => cat b c -> cat a b -> cat a c
     Right-to-left composition"
  ([cbc cab]
     (c-comp cab cbc))
  ([cbc cab & args]
     (apply >>> (reverse (flatten (list [cbc cab] args))))))
