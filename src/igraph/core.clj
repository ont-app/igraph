(ns ^{:author "Eric D. Scott",
      :doc "Abstractions over a graph object, intended to sit alongside the 
other basic clojure data structures such as maps and sequences.
"}
    igraph.core
  (:require [clojure.set :as set]
            )
  (:gen-class))


(defprotocol IGraph
  "An abstraction for graphs"
  (normal-form [g] "Returns <map> s.t. #(keys %) -> #{::schema ::contents}
Where 
<schema> := a map s.t. (keys %) -> #{::indexed-as ...}
<contents> := a map s.t. (keys %) -> {<subject> {<predicate> #{<object>...}...}...}
<indexed-as> := [<functional-subject> <function-predicate> <functional-object>],
  an exhaustive and non-repeating vector of <canonical-args>.
  By default: [::subject ::predicate ::object]
<function-subject> is one of <canonical-args>
<functional-predicate> is one of <canonical-args>
<function-object> is one of <canonical-args>
<canonical-args> :=  #{::subject ::predicate ::argument}
<subject> is a hashable value (typically a keyword) indentifying some functional
  subject
<predicate> is a hashable value (typically a keyword) identifying some function
  predicate
<object> is an arbitrary clojure object.
")
  (add [g triple])
  (get-p-o [g s])
  (get-o [g s p])
  (ask [g s p o])
  )

(declare make-graph)

(deftype Graph [schema contents]
  IGraph
  (normal-form [g]
    {:schema (.schema g)
     :contents (.contents g)
     })
  (add [g triple]
    (let [[s p o] triple]
      (make-graph
       :schema (.schema g)
       :contents (assoc-in (.contents g)
                           [s p]
                           (set/union #{o}
                                      (get-in (.contents g) [s p] #{}))))))
  (get-p-o [g s] (get (.contents g) s))
  (get-o [g s p] (get-in (.contents g) [s p]))
  (ask [g s p o] (not (not (get-in (.contents g) [s p o])))))


                       

(defn make-graph
  ([&{:keys [schema contents]
      :or {schema [::subject ::predicate ::object]
           contents {}}}]
   (Graph. schema contents)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (normal-form (make-graph)))
