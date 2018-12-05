(ns ^{:author "Eric D. Scott",
      :doc "Abstractions over a graph object, intended to sit alongside the 
other basic clojure data structures such as maps and sequences.
"}
    igraph.core
  (:require [clojure.set :as set]
            ))


(defprotocol IGraph
  "An abstraction for S-P-O graphs"
  (normal-form [g] "Returns {<s> {<p> #{<o>...}...}...}
Where 
<s> is the subject of a triple := [<s> <p> <o>] in <g>
<p> is predicate of same
<o> is the object of same
")

  (add [g to-add]
    "Returns <g>, with <to-add> added to its contents
Where
<g> is a graph
<to-add> is in some format interpretable as a set of triples.
"
    )
  (get-p-o [g s]
    "Returns {<p> #{<o> ...}} associated with <s> in <g>
Where
<g> is a graph
<s> is subject 
<p> and <o> are in triples := [<s> <p> <o>] in <g>
"
    )
  (get-o [g s p]
    "Returns {<o> ...} for <s> and <p> in <g>
Where
<g> is a graph
<s> is subject of some triples in <g>
<p> is predicate of some triples in <g>
<o> appears in triple [<s> <p> <o>] in <g>
"
    )
  (ask [g s p o]
    "Returns truthy value iff [<s> <p> <o>] appears in <g>
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
(g) -> {<s> {<p> #{<o>...}...}...} ;; AKA normal-form
(g s) -> {<p> #{<o>...}, ...}
(g s p) -> #{<o> ...}
(g s p o) -> <o> iff [<s> <p> <o>] is in <g>
")
  )

