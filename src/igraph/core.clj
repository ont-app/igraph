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
<schema> :=  (keys %) -> #{::indexed-as ...}
<contents> := a map s.t. (keys %) -> {<subject> {<predicate> #{<object>...}...}...}
<indexed-as> := [<functional-subject> <function-predicate> <functional-object>],
  an exhaustive and non-repeating vector of <canonical-args>.
  By default: [::subject ::predicate ::object]
<function-subject> is one of <canonical-args>
<functional-predicate> is one of <canonical-args>
<function-object> is one of <canonical-args>
<canonical-args> :=  #{::subject ::predicate ::object}
<subject> is a hashable value (typically a keyword) indentifying some functional
  subject
<predicate> is a hashable value (typically a keyword) identifying some function
  predicate
<object> is an arbitrary clojure object.
")

  (add [g to-add]
    "Returns <g>, with <to-add> added to its contents
Where
<g> is a graph
<to-add> is in some format interpretable as [[<subject> <predicate> <object>]...]
"
    )
  (get-p-o [g s]
    "Returns {<predicate> #{<object> ...}} associated with <s> in <g>
Where
<predicate> and <object> are in a triple [<s> <predicate> <object>] in <g>
"
    )
  (get-o [g s p]
    "Returns {<object> ...} for <s> and <p> in <g>
Where
<g> is a graph
<s> is subject of some triples in <g>
<p> is predicate of some triples in <g>
<object> appears in triple [<s> <p> <object>] in <g>
"
    )
  (ask [g s p o]
    "Returns true iff [<s> <p> <o>] appears in <g>
"
    )
  (query [g q]
    "Returns #{<binding> ...} for query <q> applied to <g>
Where
<g> is a graph
<query> is a query in a format suitable for querying <g>
<binding> := {<var> <value>, ...}
<var> is a variable specified in <q>
<value> is a value found in <g> bounder to <var> per <q>
"
    )
  ;; for IFn
  (invoke [g] [g s] [g s p] [g s p o]
    "Applies <g> as a function to the rest of its arguments, representing 
   triples [<s> <p> <o>] in <g> respectively.
(g) -> {<s> {<p> #{<o>...}...}...} (normal form)
(g s) -> {<p> #{<o>...}, ...}
(g s p) -> #{<o> ...}
(g s p o) -> true iff [<s> <p> <o>] is in <g>
")
  )

